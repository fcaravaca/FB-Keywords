library(tidyverse)
library(tidytext)
library(widyr)

require("quanteda")
require(quanteda.textplots)
require(quanteda.corpora) #devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.textstats)
require(readtext)
library(tm)
#library(topicmodels)
library(reshape2)
library(pals)
library(tidyverse) # general utility & workflow functions
library(data.table)
library(textstem)
library(stringi)


# turn your original vector into a tibble with an ID column
ss <- c(
  "ibm madrid limited research", 
  "madrid limited research", 
  "limited research",
  "research",
  "ee"
) %>% as.tibble() %>% 
  rowid_to_column("ID")


# create df of words & counts (tf-idf needs this)
ss_words <- ss %>% 
  unnest_tokens(words, value) %>% 
  count(ID, words, sort = TRUE)

# create tf-idf embeddings for your data
ss_tfidf <- ss_words %>% 
  bind_tf_idf(ID, words, n)

# return list of document similarity
ss_tfidf %>% 
  pairwise_similarity(ID, words, tf_idf, sort = TRUE)


########

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


# Read CSVs (all words eu-political-barometer_keywords_2023_v02)
all_words_global <- read.csv("eu-political-barometer_keywords_2023_v02.csv") %>% mutate(week = as.Date(week))
posts_activity <- read.csv("eu-political-barometer_post-activity_2023_v02.csv") %>% mutate(week = as.Date(week))
parties <- read.csv("eu-political-barometer_facebook-parties_2023_v02.csv") 

countriesToSelect <- c("Portugal", "Spain", "Denmark")


all_words_parties <- all_words_global %>% inner_join(parties %>% filter(country_name %in% countriesToSelect)) %>% filter(week < as.Date("2019-06-30"))

tf <- all_words_parties %>% group_by(party_id, party_name, party_name_short, word) %>% summarise(freq = sum(freq)) %>% ungroup #%>% filter(freq >= 5)



##################




###################
# READ MANIFESTOS # 
###################

manifestos <- tibble(text=NA, party=NA, country=NA)

for(country in countriesToSelect){
  for(party_n in list.files(country)){
    manifestos <- manifestos %>% union( read.csv2(paste(country, party_n, sep="/"), sep = ",") %>%
                                          select(text) %>% 
                                          mutate(party=str_split(str_split(party_n, ".csv", simplify=T)[1], "_", simplify = T)[1]) %>% 
                                          mutate(country=country))
  }
}

manifestos <- manifestos %>% filter(!is.na(text))
  
  
  
########

# Remove invalid escape sequences
manifestos$text <- gsub("\\\\u\\{[0-9A-Fa-f]+\\}", "", manifestos$text, perl = TRUE)
# Unscape characters 
manifestos$text <- stri_unescape_unicode(manifestos$text)

#Remove ' or ’
manifestos$text <- gsub("'s\\b|'", "", manifestos$text)
manifestos$text <- gsub("’s\\b|’", "", manifestos$text)
#Remove URLs
manifestos$text <- gsub("http\\S*", "", manifestos$text)
manifestos$text <- gsub("www\\S*", "", manifestos$text)
manifestos$text <- gsub("\\b\\w+\\..+?\\b", "", manifestos$text)

manifestos$text <- str_replace_all(manifestos$text, "-", " ")
manifestos$text <- str_replace_all(manifestos$text, "_", " ")
manifestos$text <- str_replace_all(tolower(manifestos$text), "covid19", "covid")
manifestos$text <- str_replace_all(manifestos$text, "# ", "#")


# Remove emojis
manifestos$text <-  gsub("[\U{1F300}-\U{1F5FF}\U{1F900}-\U{1F9FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{2600}-\U{26FF}\U{2700}-\U{27BF}\U{FE00}-\U{FEFF}\U{1F1E0}-\U{1F1FF}\U{1F191}-\U{1F251}\U{203C}\U{2049}\U{20E3}\U{2122}\U{2139}\U{2194}-\U{2199}\U{21A9}-\U{21AA}\U{231A}-\U{231B}\U{2328}\U{2388}\U{23CF}\U{23E9}-\U{23F3}\U{23F8}-\U{23FA}\U{24C2}\U{25AA}-\U{25AB}\U{25B6}\U{25C0}\U{25FB}-\U{25FE}\U{2600}-\U{2605}\U{2607}-\U{2612}\U{2614}-\U{2615}\U{2618}\U{261D}\U{2620}-\U{2621}\U{2626}\U{262A}-\U{262B}\U{2638}-\U{263A}\U{2648}-\U{2653}\U{2660}\U{2663}\U{2665}-\U{2666}\U{2668}-\U{267B}\U{267E}-\U{267F}\U{2692}-\U{2697}\U{2699}\U{269B}-\U{269C}\U{26A0}-\U{26A1}\U{26AA}-\U{26AB}\U{26B0}-\U{26B1}\U{26BD}-\U{26BE}\U{26C4}-\U{26C5}\U{26C8}\U{26CE}-\U{26CF}\U{26D1}\U{26D3}-\U{26D4}\U{26E9}-\U{26EA}\U{26F0}-\U{26F5}\U{26F7}-\U{26FA}\U{26FD}\U{2702}\U{2705}\U{2708}-\U{270D}\U{270F}\U{2712}\U{2714}\U{2716}\U{271D}\U{2721}\U{2728}\U{2733}-\U{2734}\U{2744}\U{2747}\U{274C}\U{274E}\U{2753}-\U{2755}\U{2757}\U{2763}-\U{2764}\U{2795}-\U{2797}\U{27A1}\U{27B0}\U{27BF}\U{2934}-\U{2935}\U{2B05}-\U{2B07}\U{2B1B}-\U{2B1C}\U{2B50}\U{2B55}\U{3030}\U{303D}\U{3297}\U{3299}\U{1F004}\U{1F0CF}\U{1F170}-\U{1F171}\U{1F17E}-\U{1F17F}\U{1F18E}\U{1F191}-\U{1F19A}\U{1F1E6}-\U{1F1FF}\U{1F201}-\U{1F202}\U{1F21A}\U{1F22F}\U{1F232}-\U{1F236}\U{1F238}-\U{1F23A}\U{1F250}-\U{1F251}\U{1F300}-\U{1F320}\U{1F32D}-\U{1F335}\U{1F337}-\U{1F37C}\U{1F37E}-\U{1F393}\U{1F3A0}-\U{1F3CA}\U{1F3CF}-\U{1F3D3}\U{1F3E0}-\U{1F3F0}\U{1F3F4}\U{1F3F8}-\U{1F43E}\U{1F440}\U{1F442}-\U{1F4FC}\U{1F4FF}-\U{1F53D}\U{1F54B}-\U{1F54E}\U{1F550}-\U{1F567}\U{1F57A}\U{1F595}-\U{1F596}\U{1F5A4}-\U{1F5A5}\U{1F5A8}\U{1F5B1}-\U{1F5B2}\U{1F5BC}\U{1F5C2}-\U{1F5C4}\U{1F5D1}-\U{1F5D3}\U{1F5DC}-\U{1F5DE}\U{1F5E1}\U{1F5E3}\U{1F5E8}\U{1F5EF}\U{1F5F3}\U{1F5FA}-\U{1F64F}\U{1F680}-\U{1F6C5}\U{1F6CB}-\U{1F6D2}\U{1F6E0}-\U{1F6E5}\U{1F6E9}\U{1F6EB}-\U{1F6EC}\U{1F6F0}\U{1F6F3}-\U{1F6F8}\U{1F910}-\U{1F93A}\U{1F93C}-\U{1F93E}\U{1F940}-\U{1F945}\U{1F947}-\U{1F94C}\U{1F950}-\U{1F96B}\U{1F980}-\U{1F997}\U{1F9C0}-\U{1F9C2}\U{1F9D0}-\U{1F9FF}\U{1FA70}-\U{1FA73}\U{1FA78}-\U{1FA7A}\U{1FA80}-\U{1FA82}\U{1FA90}-\U{1FA95}\U{200C}]+", "", manifestos$text, perl = TRUE)

# Remove words starting with #, @ or digit character
manifestos$text <- gsub("(^|\\s)[@#]\\S+|\\b\\d+\\S*\\b", "", manifestos$text)
manifestos$text <- gsub("\\b\\d\\w*\\b", "", manifestos$text)

manifestos$text <- gsub("[\U0001D400-\U0001D7FF]", "", manifestos$text, perl = TRUE)

manifestos <- manifestos %>% mutate(text =  gsub("\\d+$", "", text))
manifestos <- manifestos %>% filter(str_length(text) >= 20)

corpus_party <- corpus(manifestos %>% mutate(extra_text = text) %>% select(text, party, country), text_field="text")

tokens_party <- quanteda::tokens(corpus_party, remove_punct = T, remove_symbols = T, remove_numbers=TRUE)


toks_nostop <- tokens_select(tokens_party, pattern = stopwords("en"), selection = "remove")

lemmas_table <- lexicon::hash_lemmas %>% #lemmas table without numbers
  filter(!grepl("^\\d+$", lemma))

tokens_lemma <- tokens_replace(toks_nostop, pattern = lemmas_table$token, replacement = lemmas_table$lemma)

toks_ngram <- tokens_ngrams(tokens_lemma, n = 1)

dfmat <- dfm(toks_ngram)

# change to 150
dfmtrimmed <- dfm_trim(dfmat, min_docfreq = 1, min_termfreq = 1, verbose = TRUE)

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
  elements <- convert((dfm_w), to = "data.frame") %>% gather() %>% filter(value > 0) %>% filter(key != "doc_id") %>% filter(key != "â") %>% mutate(value = as.numeric(value)) %>% #mutate(value = 1) %>% # We only want one appparance for post
    group_by(key) %>% summarise(freq = sum(value)) %>% arrange(-freq)
  
  # Exceptions 
  elements <- manageException(elements, dfm_w, "united kingdom", "unite_kingdom", "united_kingdom")
  elements <- manageException(elements, dfm_w, "united states", "unite_state", "united_states")
  elements <- manageException(elements, dfm_w, "united nations", "unite_nation", "united_nations")
  
  elements <- elements %>% filter(freq > 0)
  elements
}


all_words = data.frame(party=NA,word = NA, freq=1, country=NA)

i <- 0
for(party_c in unique(docvars(dfmtrimmed)$party)){
  i <- i + 1
  print(paste(party_c, i))
  all_words_party = data.frame(party=NA,word = NA,freq=1)
  
  dfm_party <- dfm_subset(dfmtrimmed, party==party_c)

  info <- get_info_from_week(dfm_party)
  info <- info %>% mutate(party=party_c, word=key) %>% select(party, word, freq)
  all_words_party <- rbind(all_words_party, info)
  all_words_party <- all_words_party %>% mutate(country=docvars(dfm_party)$country[1])
  
  all_words <- rbind(all_words, all_words_party)
}

manifesto_tf <- all_words %>% dplyr::filter(!is.na(party))









facebook_tf <- all_words_parties %>% group_by(party_name_short, word, country_name) %>% summarise(freq = sum(freq)) %>% ungroup %>% rename(country = country_name, party = party_name_short) %>% filter(freq>6)




calculate_cosine_similarity <- function(tf_1, tf_2){
  merged_tibble <- merge(tf_1, tf_2, by = "word", suffixes = c("_tf_1", "_tf_2"), all = TRUE)
  
  merged_tibble[is.na(merged_tibble)] <- 0
  
  # Extract the frequency columns
  tfidf_tf_1 <- merged_tibble$tfidf_tf_1
  tfidf_tf_2 <- merged_tibble$tfidf_tf_2
  
  dot_product <- sum(tfidf_tf_1 * tfidf_tf_2)
  magnitude_1 <- sqrt(sum(tfidf_tf_1^2))
  magnitude_2 <- sqrt(sum(tfidf_tf_2^2))
  cosine_similarity <- dot_product / (magnitude_1 * magnitude_2)
  
  return(cosine_similarity)
}


m_tf <- tibble(manifesto_tf %>% mutate(id = paste("m", party, sep="_")))

f_tf <- facebook_tf %>% mutate(id = paste("f", party, sep="_")) %>% filter(party %in% unique(manifesto_tf$party))

#f_tf <- f_tf %>% mutate(freq = freq * max(m_tf$freq) /max(f_tf$freq))

all_tf <- (m_tf %>% filter(word %in% unique(all_words_global$word))) %>% union(f_tf %>% filter(word %in% unique(m_tf$word))) %>% filter(!str_detect(word, "_"))




all_tf <- mutate(all_tf, tf = freq / sum(freq))

# Step 2: Calculate Document Frequency (DF)
df <- all_tf %>%
  group_by(word) %>%
  summarize(df = n_distinct(id))

# Step 3: Calculate Inverse Document Frequency (IDF)
n <- n_distinct(all_tf$id)  # Total number of documents
df <- mutate(df, idf = log(n / (df + 1))) %>% mutate_cond(idf < 0, idf=0)

# Step 4: Calculate TF-IDF
tfidf <- left_join(all_tf, df, by = "word") %>%
  mutate(tfidf = tf * idf)

# View the resulting tibble with TF-IDF values
tfidf <- tfidf #%>% filter(df > 4)


partiesToAnalyze <- unique(tfidf$party)

heatMapLabels <- c()

for(party in partiesToAnalyze){
  heatMapLabels <- c(heatMapLabels, paste("f", party, sep="_"))
  heatMapLabels <- c(heatMapLabels, paste("m", party, sep="_"))
}

label_combinations <- expand.grid(heatMapLabels, heatMapLabels)

label_matrix <- matrix(NA, nrow = length(heatMapLabels), ncol = length(heatMapLabels), 
                       dimnames = list(heatMapLabels, heatMapLabels))


heatMapTibble <- tibble(var1=NA, var2=NA, value=1, country1=NA, country2=NA) 

for(label1 in heatMapLabels){
  for(label2 in heatMapLabels){
    heatMapTibble <- heatMapTibble %>% union( tibble(var1=label1, var2=label2, 
                                                     value=calculate_cosine_similarity(tfidf %>% filter(id == label1), tfidf %>% filter(id == label2)),
                                                     country1=(tfidf %>% filter(id == label1))$country[1],country2=(tfidf %>% filter(id == label2))$country[1]
                                                     ))
  }
}

heatMapTibble <- heatMapTibble %>% filter(!is.na(var1)) %>% mutate(value = round(value, digits=2))


# Create a heatmap using ggplot
#ggplot(heatMapTibble, aes(var1, var2, fill = value)) +
#  geom_tile() +
#  scale_fill_gradientn(colours = c("#dafdce", "#24469d", "#081d58"), na.value = "transparent") +  # Custom color gradient
#  labs(x = "X Labels", y = "Y Labels", fill = "Value") +
#  geom_text(aes(label = value), color = "white", size = 4) +
#  theme_minimal() 

heatMapTibble2 <- heatMapTibble %>% filter(country1 == country2)


ggplot(heatMapTibble2, aes(var1, var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#B2E0A2", "#24469d", "#081d58"), na.value = "transparent") +  # Custom color gradient
  labs(x = "", y = "", fill = "Cosine Similarity") +
  geom_text(aes(label = value), color = "white", size = 4) +
  theme_minimal() + theme(legend.position="none", axis.text = element_text(size = 7)) +
  facet_wrap(~country2, scales="free", ncol =2)




wordsparties <- all_words_global %>% group_by(party_id, word) %>% summarise(freq=sum(freq)) %>% left_join(parties, by="party_id") 

activityparties <- posts_activity %>% group_by(party_id) %>% summarise(postsWithText = sum(postsWithText), totalPosts=sum(totalPosts)) %>% left_join(parties, by="party_id")


activityparties %>% group_by(country_name) %>% summarise(parties = n(), medianPostsText=median(postsWithText), postsMedian=median(totalPosts),
                                                         postsWithText=sum(postsWithText), totalPosts=sum(totalPosts),
                                                                        
                                                         )


wordsparties %>% group_by(country_name, word) %>% summarise(freq=sum(freq)) %>% group_by(country_name) %>% summarise(uniqueTerms=n(), simpleTerms=sum(grepl("_", word)))
