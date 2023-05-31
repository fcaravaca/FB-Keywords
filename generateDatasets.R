require("quanteda")
require(quanteda.textplots)
require(quanteda.corpora) #devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.textstats)
require(readtext)
library(tm)
library(topicmodels)
library(reshape2)
library(pals)
library(tidyverse) # general utility & workflow functions
library(data.table)
library(textstem)
library(stringi)


load_initial_data <- function(initial_date,final_date, country_to_filter){
  view_party <- read.csv("view_party.csv", encoding="UTF8")
  
  view_party_data <- view_party %>% filter(!is.na(facebook_id))# %>% filter(country_name != "Germany")
  
  all_posts_collected_utf.8 <<- read.csv("all_posts.csv", encoding="UTF8")
  if(!missing(country_to_filter)){
    view_party_data <- view_party_data %>% filter(country_name==country_to_filter)
  }
  
  view_party_data <<- view_party_data
  posts_data <<- all_posts_collected_utf.8 %>% filter(page_id %in% c(view_party_data$facebook_id))
  
  
  for(id in view_party_data$facebook_id){
    if(!is.na(view_party_data$left_right[view_party_data$facebook_id==id])){
      posts_data$ideology[posts_data$page_id==id] <- switch(round(view_party_data$left_right[view_party_data$facebook_id==id])+1, "far-left", "far-left", "left", "left", "center", "center", "center", "right", "right", "far-right", "far-right")
    }else{
      posts_data$ideology[posts_data$page_id==id] <- NA
    }
    posts_data$left_right[posts_data$page_id==id] <- view_party_data$left_right[view_party_data$facebook_id==id]
    posts_data$country_name[posts_data$page_id==id] <- paste(view_party_data$country_name[view_party_data$facebook_id==id])
    posts_data$family_name[posts_data$page_id==id] <- paste(view_party_data$family_name[view_party_data$facebook_id==id])
    posts_data$party_name_short[posts_data$page_id==id] <- paste(view_party_data$party_name_short[view_party_data$facebook_id==id])
    posts_data$party_name[posts_data$page_id==id] <- paste(view_party_data$party_name[view_party_data$facebook_id==id])
    posts_data$party_id[posts_data$page_id==id] <- view_party_data$party_id[view_party_data$facebook_id==id]
  }
  
  dates <- strsplit(as.character(posts_data$timestamp), split=" ")
  sep<- sapply(dates, "[", 1)
  
  year <- format(as.Date(sep, format="%Y-%m-%d"), format = "%Y")
  month <- format(as.Date(sep, format="%Y-%m-%d"), format = "%Y/%m")
  week <- format(as.Date(sep, format="%Y-%m-%d"), format = "%Y/%U")
  day <- format(as.Date(sep, format="%Y-%m-%d"), format = "%Y/%m/%d")
  for(w in seq(week)){
    p <- str_split(week[w], "/")
    if(p[[1]][2] == "00"){
      week[w] = paste(strtoi(p[[1]][1])-1, 52, sep="/")
    }
  }
  
  week <- as.Date(paste(week, "0", sep = "/"), format = "%Y/%U/%w")
  
  posts_data <- posts_data %>% add_column(year, .before = "comments")
  posts_data <- posts_data %>% add_column(month, .before = "year")
  posts_data <- posts_data %>% add_column(week, .before = "month")
  posts_data <- posts_data %>% add_column(day, .before = "week")
  posts_data$week[posts_data$month=="2019/01" & is.na(posts_data$week)] <<- as.Date("2018-12-31", format="%Y-%m-%d")
  posts_data$week[posts_data$month=="2020/01" & is.na(posts_data$week)] <<- as.Date("2019-12-30", format="%Y-%m-%d")
  
  #TODO: Remove filters)
  posts_data_act <- data.table(posts_data)
  print(paste("Total posts wihtout filters:", length(posts_data_act$comments)))
  posts_data_act <- posts_data_act  %>% filter(as.Date(timestamp) < as.Date(final_date)) %>% filter(as.Date(timestamp) > as.Date(initial_date))
  print(paste("Total posts with date filters:", length(posts_data_act$comments)))

  posts_data <<- posts_data
  posts_data_act <<- posts_data_act
  countries <<- unique(posts_data$country_name)
}
load_initial_data("2018-12-31", "2029-05-21")


facebook_parties <- posts_data %>% select(party_id, left_right, country_name, family_name, party_name, party_name_short, post_url) %>%
                           group_by(party_id) %>% group_by(party_id) %>% slice(1) %>% mutate(facebook_page = str_split(post_url, "/", simplify=T)[,4]) %>%
                           select(-(post_url))

write.csv(facebook_parties, "facebook_parties.csv", row.names=F)

posts_activity <- posts_data %>% mutate(withText = str_length(text_translation) > 0) %>% group_by(party_id, week, withText) %>%
  summarise(posts=n())

posts_activity <- posts_activity %>%
  group_by(party_id, week) %>%
  summarize(postsWithText = sum(posts[withText == TRUE]),
            totalPosts = sum(posts))

write.csv(posts_activity, "post_activity.csv", row.names=F)

party_posts <- posts_data

# Remove invalid escape sequences
party_posts$text_translation <- gsub("\\\\u\\{[0-9A-Fa-f]+\\}", "", party_posts$text_translation, perl = TRUE)
# Unscape characters 
party_posts$text_translation <- stri_unescape_unicode(party_posts$text_translation)

#Remove ' or ’
party_posts$text_translation <- gsub("'s\\b|'", "", party_posts$text_translation)
party_posts$text_translation <- gsub("’s\\b|’", "", party_posts$text_translation)
#Remove URLs
party_posts$text_translation <- gsub("http\\S*", "", party_posts$text_translation)
party_posts$text_translation <- gsub("www\\S*", "", party_posts$text_translation)
party_posts$text_translation <- gsub("\\b\\w+\\..+?\\b", "", party_posts$text_translation)

party_posts$text_translation <- str_replace_all(party_posts$text_translation, "-", " ")
party_posts$text_translation <- str_replace_all(party_posts$text_translation, "_", " ")
party_posts$text_translation <- str_replace_all(tolower(party_posts$text_translation), "covid19", "covid")
party_posts$text_translation <- str_replace_all(party_posts$text_translation, "# ", "#")


# Remove emojis
party_posts$text_translation <-  gsub("[\U{1F300}-\U{1F5FF}\U{1F900}-\U{1F9FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{2600}-\U{26FF}\U{2700}-\U{27BF}\U{FE00}-\U{FEFF}\U{1F1E0}-\U{1F1FF}\U{1F191}-\U{1F251}\U{203C}\U{2049}\U{20E3}\U{2122}\U{2139}\U{2194}-\U{2199}\U{21A9}-\U{21AA}\U{231A}-\U{231B}\U{2328}\U{2388}\U{23CF}\U{23E9}-\U{23F3}\U{23F8}-\U{23FA}\U{24C2}\U{25AA}-\U{25AB}\U{25B6}\U{25C0}\U{25FB}-\U{25FE}\U{2600}-\U{2605}\U{2607}-\U{2612}\U{2614}-\U{2615}\U{2618}\U{261D}\U{2620}-\U{2621}\U{2626}\U{262A}-\U{262B}\U{2638}-\U{263A}\U{2648}-\U{2653}\U{2660}\U{2663}\U{2665}-\U{2666}\U{2668}-\U{267B}\U{267E}-\U{267F}\U{2692}-\U{2697}\U{2699}\U{269B}-\U{269C}\U{26A0}-\U{26A1}\U{26AA}-\U{26AB}\U{26B0}-\U{26B1}\U{26BD}-\U{26BE}\U{26C4}-\U{26C5}\U{26C8}\U{26CE}-\U{26CF}\U{26D1}\U{26D3}-\U{26D4}\U{26E9}-\U{26EA}\U{26F0}-\U{26F5}\U{26F7}-\U{26FA}\U{26FD}\U{2702}\U{2705}\U{2708}-\U{270D}\U{270F}\U{2712}\U{2714}\U{2716}\U{271D}\U{2721}\U{2728}\U{2733}-\U{2734}\U{2744}\U{2747}\U{274C}\U{274E}\U{2753}-\U{2755}\U{2757}\U{2763}-\U{2764}\U{2795}-\U{2797}\U{27A1}\U{27B0}\U{27BF}\U{2934}-\U{2935}\U{2B05}-\U{2B07}\U{2B1B}-\U{2B1C}\U{2B50}\U{2B55}\U{3030}\U{303D}\U{3297}\U{3299}\U{1F004}\U{1F0CF}\U{1F170}-\U{1F171}\U{1F17E}-\U{1F17F}\U{1F18E}\U{1F191}-\U{1F19A}\U{1F1E6}-\U{1F1FF}\U{1F201}-\U{1F202}\U{1F21A}\U{1F22F}\U{1F232}-\U{1F236}\U{1F238}-\U{1F23A}\U{1F250}-\U{1F251}\U{1F300}-\U{1F320}\U{1F32D}-\U{1F335}\U{1F337}-\U{1F37C}\U{1F37E}-\U{1F393}\U{1F3A0}-\U{1F3CA}\U{1F3CF}-\U{1F3D3}\U{1F3E0}-\U{1F3F0}\U{1F3F4}\U{1F3F8}-\U{1F43E}\U{1F440}\U{1F442}-\U{1F4FC}\U{1F4FF}-\U{1F53D}\U{1F54B}-\U{1F54E}\U{1F550}-\U{1F567}\U{1F57A}\U{1F595}-\U{1F596}\U{1F5A4}-\U{1F5A5}\U{1F5A8}\U{1F5B1}-\U{1F5B2}\U{1F5BC}\U{1F5C2}-\U{1F5C4}\U{1F5D1}-\U{1F5D3}\U{1F5DC}-\U{1F5DE}\U{1F5E1}\U{1F5E3}\U{1F5E8}\U{1F5EF}\U{1F5F3}\U{1F5FA}-\U{1F64F}\U{1F680}-\U{1F6C5}\U{1F6CB}-\U{1F6D2}\U{1F6E0}-\U{1F6E5}\U{1F6E9}\U{1F6EB}-\U{1F6EC}\U{1F6F0}\U{1F6F3}-\U{1F6F8}\U{1F910}-\U{1F93A}\U{1F93C}-\U{1F93E}\U{1F940}-\U{1F945}\U{1F947}-\U{1F94C}\U{1F950}-\U{1F96B}\U{1F980}-\U{1F997}\U{1F9C0}-\U{1F9C2}\U{1F9D0}-\U{1F9FF}\U{1FA70}-\U{1FA73}\U{1FA78}-\U{1FA7A}\U{1FA80}-\U{1FA82}\U{1FA90}-\U{1FA95}\U{200C}]+", "", party_posts$text_translation, perl = TRUE)

# Remove words starting with #, @ or digit character
party_posts$text_translation <- gsub("(^|\\s)[@#]\\S+|\\b\\d+\\S*\\b", "", party_posts$text_translation)
party_posts$text_translation <- gsub("\\b\\d\\w*\\b", "", party_posts$text_translation)

# Remove other symbols
party_posts$text_translation <- gsub("[\U0001D400-\U0001D7FF]", "", party_posts$text_translation)

corpus_party <- corpus(party_posts %>% mutate(extra_text = text_translation) %>% select(text_translation, party_id, post_id, week, extra_text), text_field="text_translation")
                       
tokens_party <- quanteda::tokens(corpus_party, remove_punct = T, remove_symbols = T, remove_numbers=TRUE)


toks_nostop <- tokens_select(tokens_party, pattern = stopwords("en"), selection = "remove")

lemmas_table <- lexicon::hash_lemmas %>% #lemmas table without numbers
     filter(!grepl("^\\d+$", lemma))

tokens_lemma <- tokens_replace(toks_nostop, pattern = lemmas_table$token, replacement = lemmas_table$lemma)

toks_ngram <- tokens_ngrams(tokens_lemma, n = 1:2)

dfmat <- dfm(toks_ngram)

# change to 150
dfmtrimmed <- dfm_trim(dfmat, min_docfreq = 150, min_termfreq = 1, verbose = TRUE)

manageException <- function(elements, dfm_w, text_key, update_key, add_key){
  amount <- sum(grepl(text_key, docvars(dfm_w)$extra_text))
  if(amount == 0){
    return(elements)
  }
  
  elements <- elements %>% mutate(freq = if_else(key == update_key, freq - amount, freq))
  
  if (!(add_key %in% elements$key)) {
    new_row <- tibble(key = add_key, freq = amount)
    elements <- bind_rows(elements, new_row)
  } else {
    elements <- elements %>%
      mutate(freq = if_else(key == add_key, freq + amount, freq))
  }
  
  return(elements)
}


get_info_from_week <- function(row){
  dfm_w <- dfm_trim(row, min_docfreq = 1, min_termfreq = 1)
  elements <- convert((dfm_w), to = "data.frame") %>% gather() %>% filter(value > 0) %>% filter(key != "doc_id") %>%
    mutate(value = 1) %>% # We only want one appparance for post
    group_by(key) %>% summarise(freq = sum(value)) %>% arrange(-freq)

  # Exceptions 
  elements <- manageException(elements, dfm_w, "united kingdom", "unite_kingdom", "united_kingdom")
  elements <- manageException(elements, dfm_w, "united states", "unite_state", "united_states")
  elements <- manageException(elements, dfm_w, "united nations", "unite_nation", "united_nations")
  
  elements <- elements %>% filter(freq > 0)
  elements
}


all_words = data.frame(party_id=NA,word = NA, week=NA,freq=1)

i <- 0
for(party_c in unique(docvars(dfmtrimmed)$party_id)){
  i <- i + 1
  print(paste(party_c, i))
  all_words_party = data.frame(party_id=NA,word = NA, week=NA,freq=1)
  
  dfm_party <- dfm_subset(dfmtrimmed, party_id==party_c)
  for(week_c in unique(paste(docvars(dfm_party)$week))){
    dfm_week <- dfm_subset(dfm_party, week==week_c)
    info <- get_info_from_week(dfm_week)
    info <- info %>% mutate(party_id=party_c, week=paste(week_c), word=key) %>% select(party_id, word, week, freq)
    all_words_party <- rbind(all_words_party, info)
  }
  all_words <- rbind(all_words, all_words_party)
}

all_words <- all_words %>% dplyr::filter(!is.na(party_id))

fwrite(data.table(all_words), "keywords.csv")
