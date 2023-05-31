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
all_words <- read.csv("eu-political-barometer_keywords_2023_v01.csv") %>% mutate(week = as.Date(week))
posts_activity <- read.csv("eu-political-barometer_post-activity_2023_v01.csv") %>% mutate(week = as.Date(week))

combined_activity <- posts_activity %>% group_by(week) %>% summarise(postsWithText = sum(postsWithText))
posts_weeks <- as.Date((all_words %>% group_by(week) %>% slice(1) %>% ungroup %>% arrange(week))$week)

compareTermWithRequests <- function(term, scale=T){
  gTrendsRes <- gtrends(c(term), time=paste("2018-12-30",Sys.Date()))
  gtrends_data <- gTrendsRes$interest_over_time$hits
  
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


printTerms <- function(terms, scale=T){
  all_terms <- data.frame(gTrends= 0, euPB= 0, week = as.Date("2019-01-07"), term="", postsWithText = 0, baseline="")
  for(term in terms){
    compare_df <- compareTermWithRequests(term, scale) %>% mutate(term = paste(term, " (cor: ", round(cor(gTrends, euPB),digits=5), ")", sep=""))
    
    all_terms <- all_terms %>% union(compare_df %>% mutate(baseline=as.character(baseline))) # Baseline may contain "<1" instead of a number 
  }
  
  print(all_terms %>% filter(term != "") %>% ggplot() + geom_line(aes(x=week, y=gTrends, color="Google Trends")) + 
          geom_line(aes(x=week, y=euPB, color="Political Parties on Facebook")) + theme_minimal() + facet_wrap(~term) +
          labs(x="Date", y=paste("Relative Interest"), color="Source") + theme(legend.position = "bottom"))
}

corona <- c("coronavirus", "covid", "pandemic", "lockdown", "vaccine", "healthcare", "infect", "symptom", "virus")
war <- c("russia", "ukraine", "war", "bomb", "invasion", "peace", "putin", "kiev", "nato")

printTerms(corona)
printTerms(war)

