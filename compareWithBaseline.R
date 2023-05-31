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


totalWords <- all_words %>% group_by(word, week) %>% summarise(freq=sum(freq)) %>% mutate(week = as.Date(week))

wordsValue <- totalWords %>% left_join(combined_activity, by="week") %>% filter(!is.na(postsWithText)) %>% mutate(value = freq/postsWithText)

printWithBaseline <- function(baseline, termToCompare){
  info <- wordsValue %>% filter(word %in% c(baseline, termToCompare))
  
  info <- info %>% mutate_cond(word == baseline, word="baseline") %>% mutate_cond(word == termToCompare, word="toCompare")
  info <- info %>% select(word, week, freq) %>% spread(word, freq) %>% mutate_cond(is.na(toCompare), toCompare=0)
  
  info
}


printTermsWithBaseline <- function(baseline, termsToCompare){
  allTermTibble <- tibble(week=as.Date(character()), baseline=integer(), toCompare=integer(), term=character())
  
  for(term in termsToCompare){
    allTermTibble <- allTermTibble %>% union(printWithBaseline(baseline, term) %>% mutate(term=term))
  }
  allTermTibble <- allTermTibble %>% mutate(value=toCompare/baseline)
  
  allTermTibble %>% ggplot(aes(x=as.Date(week), y=value)) + geom_line() + facet_wrap(~term, scales = "free_y") + theme_minimal() +
    labs(x="Date", y=paste("Term vs. baseline:", baseline))
  
}


printTermsWithBaseline("work", c("coronavirus", "infect", "kiev", "nato", "pandemic", "russia", "ukraine", "virus", "war"))
