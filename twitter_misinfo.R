library(tm)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(tidyverse)

data <- read.csv("032020_tweets.csv")
dt <- data[1:1000,]
tweets <- dt$tweet_text

# Make a vector source: 
tweets_source <- VectorSource(tweets)
# Make a volatile corpus: 
tweets_corpus <- VCorpus(tweets_source)
# Apply various preprocessing functions
tm_map(tweets_corpus, removeNumbers)
tm_map(tweets_corpus, removePunctuation)
tm_map(tweets_corpus, tolower) 
tm_map(tweets_corpus, removeWords, c(stopwords("en"), "the", "and", "this", "for", "are", "with", "all", "you", "more", "your", "are", "that", "not", "his", "our", "from", "her", "was", "who"))
tm_map(tweets_corpus, stripWhitespace)
tm_map(tweets_corpus, stemDocument, language = "en")

# tm_map(tweets_corpus, content_transformer(replace_abbreviation))
# Stem words
# stem_words <- stemDocument(c("complicatedly", "complicated","complication"))
# Complete words using single word dictionary
# stemCompletion(stem_words, c("complicate"))     
# Complete words using entire corpus
# stemCompletion(stem_words, tweets_corpus)

tweets_dtm <- DocumentTermMatrix(tweets_corpus)

rowTotal <- apply(tweets_dtm, 1, sum)
tweets_dtm_new <- tweets_dtm[rowTotal > 0, ]

# set a seed so that the output of the model is predictable
tweets_lda <- LDA(tweets_dtm_new, k = 5, control = list(seed = 1234))
tweets_lda
# A LDA_VEM topic model with 5 topics.

tweets_topics <- tidy(tweets_lda, matrix = "beta")
tweets_topics

tweets_top_terms <- tweets_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

tweets_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

top_10terms <- terms(tweets_lda,10)
top_10terms

library(qdapRegex)
twt_txt_url <- rm_twitter_url(twt_txt)
twt_txt_url[1:3]
twt_txt_chrs  <- gsub("[^A-Za-z]", " ", twt_txt_url)






