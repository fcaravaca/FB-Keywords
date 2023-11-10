require(tidyverse)
require(gtrendsR)
require(jsonlite)
require(stringr)
require(ggplot2)

Sys.setlocale("LC_ALL", "English")

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


# Read CSVs
all_words <- read.csv("eu-political-barometer_keywords_2023_v02.csv") %>% mutate(week = as.Date(week))
posts_activity <- read.csv("eu-political-barometer_post-activity_2023_v02.csv") %>% mutate(week = as.Date(week))

combined_activity <- posts_activity %>% group_by(week) %>% summarise(postsWithText = sum(postsWithText))
posts_weeks <- as.Date((all_words %>% group_by(week) %>% slice(1) %>% ungroup %>% arrange(week))$week)

compareTermWithRequests <- function(term, scale=T){
  #gTrendsRes <- gtrends(c(term), time=paste("2018-12-30",Sys.Date()))
  gtrends_data <- Raubenheimer_method_test_file(term, posts_weeks) #gTrendsRes$interest_over_time$hits
  
  term_tible <- all_words %>% filter(word == term) %>% group_by(week) %>% summarise(freq=sum(freq)) %>% mutate(week = as.Date(week))
  complete_df <- complete(data.frame(week = as.Date(posts_weeks)), week)
  
  term_tible <- left_join(complete_df, term_tible, by = "week") %>%
    replace_na(list(freq = 0)) %>% filter(!is.na(week))
  
  euPBData <- as.matrix(as.data.frame(term_tible$freq))
  
  class(euPBData) <- "double"
  euPBData <- rowSums(euPBData)
  
  compareDf <- data.frame("gTrends"=gtrends_data[1:length(euPBData)], "euPB" = euPBData) %>% mutate_cond(gTrends=="<1", gTrends=0) %>% 
    mutate(week = as.Date(posts_weeks)) %>% inner_join(combined_activity, by="week")
  if(scale){
    compareDf <- compareDf %>% mutate(euPB = euPB / postsWithText)
  }
  
  compareDf <- compareDf %>% mutate(euPB = euPB/max(euPB) * 100)
  compareDf <- compareDf %>% mutate(gTrends = as.numeric(gTrends), euPB = floor(euPB)) %>%
    mutate_cond(is.na(gTrends), gTrends=0) %>% mutate(baseline = gtrends_data[1:length(euPBData)]) %>%
    mutate(gTrends = gTrends/max(gTrends) * 100)
  
  compareDf
}


printTerms <- function(terms, intervalLow, intervalHigh){
  all_terms <- data.frame(gTrends= 0, euPB= 0, week = as.Date("2019-01-07"), term="", postsWithText = 0, baseline="")
  for(term in terms){
    compare_df <- compareTermWithRequests(term, T)  %>% filter(as.Date(week) > intervalLow) %>% filter(as.Date(week) < intervalHigh) %>% mutate(term = paste(term, " (cor: ", round(cor(gTrends, euPB),digits=5), ")", sep=""))
    
    all_terms <- all_terms %>% union(compare_df %>% mutate(baseline=as.character(baseline))) # Baseline may contain "<1" instead of a number 
  }
  
  print(all_terms %>% filter(term != "") %>% ggplot() + geom_line(aes(x=week, y=gTrends, color="Google Trends")) + 
          geom_line(aes(x=week, y=euPB, color="Political Parties on Facebook")) + theme_minimal() + facet_wrap(~term) +
          labs(x="Date", y=paste("Relative Interest"), color="Source") + theme(legend.position = "bottom"))
}



google_trends_call <- function(term, date1, date2, blocks_of_n_weeks){
  
  date2 <- ymd(date2)
  date2 <- date2 + weeks(blocks_of_n_weeks-1)
  date_check <- ymd(date1) + days(1)
  date_check <- as.character(date_check)
  date2 <- as.character(date2)
  print(paste(term, date1, date2))
  

  while(TRUE) {
    tryCatch({
      gTrendsRes <- gtrends(c(term), time=paste(date1,date2), onlyInterest = T)
      print("query done")
      Sys.sleep(2)
      if(is.null(gTrendsRes$interest_over_time)){
        print("Null interest over time")
        return(NA)
      }
      if(dim(gTrendsRes$interest_over_time %>% filter(as.character(date) == date_check))[1] != 0){ # Non weekly case
        gTrendsRes$interest_over_time <- gTrendsRes$interest_over_time %>% mutate(hits = as.numeric(gsub("<1", "0", hits)) ) %>%
          mutate(group = rep(1:(ceiling(n() / 7)), each = 7, length.out = n())) %>% group_by(group) %>% summarise(mean_hits = mean(hits))
        return(gTrendsRes$interest_over_time$mean_hits)
      }else{
        hits_array <- gTrendsRes$interest_over_time$hits
        hits_array <- as.numeric(gsub("<1", "0", hits_array))
        return(hits_array)
      }
    }, error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")


      print(paste(date1, date2))
      Sys.sleep(10)
    })
  }
}

aux_merge_arrays <- function(w1, w2, n){
  print(n)
  if(is.na(w1)[1] && is.na(w2)[1]){
    return(NA) # This is a very unsual scenario
  }
  if(is.na(w1)[1]){
    print(w1)
    w1 <- array(0, dim = c(n - length(w2)))
  }
  if(is.na(w2)[1]){
    print(w2)
    w2 <- array(0, dim = c(n - length(w1)))
  }
  return(c(w1,w2))
}




Raubenheimer_method_test_file <- function(term, og_weeks_array) {
  
  
  if(file.exists(paste("obtained_samples", term, "file.csv", sep="_"))) {
    obteined_samples <- read.csv(paste("obtained_samples", term, "file.csv", sep="_"))
    obteined_samples <- obteined_samples %>% rowwise() %>%
      mutate(Mean_S = mean(c_across(starts_with("S")), na.rm = TRUE))
    
    return(obteined_samples$Mean_S)
  
  }
  
  blocks_of_n_weeks = 4
  weeks_array <- og_weeks_array[seq(1, length(og_weeks_array), by = blocks_of_n_weeks)]
  
  obteined_samples <- tibble(weeks= og_weeks_array)
  n <- length(weeks_array)
  
  S <- n 
  k <- 1
  
  print(n)
  
  if (n %% 2 == 0) {
    B <- n / 2
    R <- B
  } else {
    B <- (n - 1) / 2
    R <- B + 1
  }
  
  # First width always the whole data
  w1 <- google_trends_call(term, weeks_array[1], weeks_array[n], blocks_of_n_weeks)
  obteined_samples <- obteined_samples %>% mutate("{paste('S', k, sep='')}":=c(w1))
  k <- k + 1
  
  w1 <- google_trends_call(term, weeks_array[1], weeks_array[B], blocks_of_n_weeks)
  w2 <- google_trends_call(term, weeks_array[B+1], weeks_array[n], blocks_of_n_weeks)
  obteined_samples <- obteined_samples %>% mutate("{paste('S', k, sep='')}":=aux_merge_arrays(w1,w2, n*blocks_of_n_weeks))
  
  k <- k + 1
  
  S <- S - 2
  
  if(n %% 2 != 0){
    w1 <- google_trends_call(term, weeks_array[1], weeks_array[R], blocks_of_n_weeks)
    w2 <- google_trends_call(term, weeks_array[R+1], weeks_array[n], blocks_of_n_weeks)
    obteined_samples <- obteined_samples %>% mutate("{paste('S', k, sep='')}":=aux_merge_arrays(w1,w2, n*blocks_of_n_weeks))
    
    k <- k + 1
    S <- S - 1
  }
  
  while (TRUE) { # S should be even at this point
    write.csv(obteined_samples, paste("obtained_samples", term, "file.csv", sep="_"))
    B <- B - 1
    
    if (n %% 2 == 0) {
      R <- n - B
    } else {
      R <- (n - B) + 1
    }
    
    w1 <- google_trends_call(term, weeks_array[1], weeks_array[B], blocks_of_n_weeks)
    w2 <- google_trends_call(term, weeks_array[B+1], weeks_array[n], blocks_of_n_weeks)
    print(paste("A", length(w1), length(w2)))
    obteined_samples <- obteined_samples %>% mutate("{paste('S', k, sep='')}":=aux_merge_arrays(w1,w2, n*blocks_of_n_weeks))
    k <- k + 1
    
    w1 <- google_trends_call(term, weeks_array[1], weeks_array[R], blocks_of_n_weeks)
    w2 <- google_trends_call(term, weeks_array[R+1], weeks_array[n], blocks_of_n_weeks)
    print(paste("B", length(w1), length(w2)))
    obteined_samples <- obteined_samples %>% mutate("{paste('S', k, sep='')}":=aux_merge_arrays(w1,w2, n*blocks_of_n_weeks))
    k <- k + 1
    
    # Make API calls based on W1, W2, W3, W4
    S <- S - 2
    if (S == 0) break
    
    
  }
  
  write.csv(obteined_samples, paste("obtained_samples", term, "file.csv", sep="_"))
  obteined_samples <- obteined_samples %>% rowwise() %>%
    mutate(Mean_S = mean(c_across(starts_with("S")), na.rm = TRUE))
  
  return(obteined_samples$Mean_S)
}




terms_to_analyze <- c("coronavirus", "covid", "ukraine", "healthcare", "climate", "budget")
printTerms(terms_to_analyze, as.Date("2020-01-01"), as.Date("2023-01-01"))

