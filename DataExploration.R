rm(list=ls())
library(tidyverse)
library(dplyr)
library(stringr)
library(quanteda)
library(tidytext)


#set current working directory
setwd("/home/roshan/ToxicCommentClassification")

train <- read_csv("train.csv") 
test <- read_csv("test.csv") 
subm <- read_csv("sample_submission.csv") 

paste("Train column names:", paste(names(train), collapse = ", "))
paste("Test column names:", paste(names(test), collapse = ", "))

#frequencies of the different classes:
options(repr.plot.width=6, repr.plot.height=4)

# Display percent of dataset that belongs to a certain toxic class
paste(paste0(names(train)[3:8], " ", round(100*apply(train[,3:8], 2, sum)/nrow(train),2), "%"), collapse = ", ")

# Bar plot of class counts
dfm <- data_frame(classification=names(apply(train[,3:8],2,sum)), n=apply(train[,3:8],2,sum)) %>%
  mutate(classification=reorder(classification,-n))
ggplot(data=dfm, aes(x=classification, y=n)) +  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.9, color="white", size=3.5)

paste0("Comments with more than one class selected: ",  sum(apply(train[,3:8], 1, function(x){ sum(x)>1 } )) )

#percentages from other classes that overlap with toxic:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$toxic==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")

#percentages from other classes that overlap with severe_toxic:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$severe_toxic==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")

#percentages from other classes that overlap with obscene:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$obscene==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")

#percentages from other classes that overlap with threat:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$threat==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")

#percentages from other classes that overlap with insult:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$insult==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")

#percentages from other classes that overlap with identity_hate:
paste(paste0(names(train)[3:8], " ", round(100*apply(train[train$identity_hate==1, 3:8], 2, sum)/apply(train[, 3:8], 2, sum)), "%"), collapse = ", ")


tidy_text <- train  %>%
  select(id, comment_text, toxic, severe_toxic, obscene, threat, insult, identity_hate) %>%
  unnest_tokens(word, comment_text)
#top 10 words, before data cleaning.
options(repr.plot.width=4, repr.plot.height=4)

tidy_text  %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="steelblue") +
  xlab(NULL) +
  coord_flip()



options(repr.plot.width=4, repr.plot.height=4)

# Remove stop words
data(stop_words)
tidy_text_nostops  <- tidy_text %>%
  anti_join(stop_words)

# #top 10 words, after data cleaning.
tidy_text_nostops  %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="steelblue") +
  xlab(NULL) +
  coord_flip()

#top words for each class compare
options(repr.plot.width=7, repr.plot.height=6)

# Build a frequency table that contains comments of all 6 classes
toxic_comments <- bind_rows( tidy_text_nostops %>% filter(toxic==1) %>% select(id, word) %>% 
                               count(word, sort = TRUE) %>% mutate(classification="toxic"),
                             tidy_text_nostops %>% filter(severe_toxic==1) %>% select(id, word) %>%
                               count(word, sort = TRUE) %>% mutate(classification="severe_toxic"),
                             tidy_text_nostops %>% filter(obscene==1) %>% select(id, word)  %>% 
                               count(word, sort = TRUE) %>% mutate(classification="obscene"),
                             tidy_text_nostops %>% filter(threat==1) %>% select(id, word)    %>% 
                               count(word, sort = TRUE) %>% mutate(classification="threat"),
                             tidy_text_nostops %>% filter(insult==1) %>% select(id, word)  %>% 
                               count(word, sort = TRUE) %>% mutate(classification="insult"),
                             tidy_text_nostops %>% filter(identity_hate==1) %>% select(id, word)  %>% 
                               count(word, sort = TRUE) %>% mutate(classification="identity_hate")) %>% 
  mutate(classification = as.factor(classification), word=as.factor(word)) %>%
  select(classification, word, n)

# Plot tops words by classification
toxic_comments %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(classification) %>%
  top_n(10) %>%
  ungroup  %>% 
  ggplot(aes(word, n, fill=classification)) +
  geom_bar(show.legend = FALSE, stat="identity") + coord_flip() +
  facet_wrap(~classification, scales="free")


#WordCloud For Toxic
options(repr.plot.width=6, repr.plot.height=6)

#Wordclouds for 1st three classes
library(wordcloud, warn.conflicts = FALSE, quietly=TRUE)
library(RColorBrewer, warn.conflicts = FALSE, quietly=TRUE)
library(reshape2, warn.conflicts = FALSE, quietly=TRUE)

paste("Wordcloud for ", names(train)[3])
toxic_comments %>%
  filter(classification==names(train)[3]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))

#WordCloud For SevereToxic

options(repr.plot.width=6, repr.plot.height=6)
paste("Wordcloud for ", names(train)[4])
toxic_comments %>%
  filter(classification==names(train)[4]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))

#WordCloud For obscene
options(repr.plot.width=6, repr.plot.height=6)
paste("Wordcloud for ", names(train)[5])
toxic_comments %>%
  filter(classification==names(train)[5]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))

#'Wordcloud for  threat'
options(repr.plot.width=6, repr.plot.height=6)
paste("Wordcloud for ", names(train)[6])
toxic_comments %>%
  filter(classification==names(train)[6]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))


#'Wordcloud for  insult'
options(repr.plot.width=6, repr.plot.height=6)
paste("Wordcloud for ", names(train)[7])
toxic_comments %>%
  filter(classification==names(train)[7]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))

#'Wordcloud for  identity_hate'

options(repr.plot.width=6, repr.plot.height=6)
paste("Wordcloud for ", names(train)[8])
toxic_comments %>%
  filter(classification==names(train)[8]) %>%
  top_n(100) %>% 
  with(wordcloud(word, n, min.freq = 5, scale=c(3, 1), colors= brewer.pal(4, "Dark2")))


options(repr.plot.width=7, repr.plot.height=6)

# tf-idf analysis
total_words <- toxic_comments %>% 
  group_by(classification) %>% 
  summarize(total = sum(n))

toxic_words <- left_join(toxic_comments, total_words)

toxic_words <- toxic_words %>% 
  bind_tf_idf(word, classification, n)

toxic_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(classification) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = classification)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~classification, ncol = 3, scales = "free") +
  coord_flip()




















