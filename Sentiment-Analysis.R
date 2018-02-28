#The tidytext package contains several sentiment lexicons in the sentiments dataset.
library(tidytext)

#The three general-purpose lexicons are

#AFINN : from Finn Årup Nielsen,
#bing : from Bing Liu and collaborators, and
#nrc : from Saif Mohammad and Peter Turney.

#These lexicons contain many English words and the words are assigned scores for positive/negative
#sentiment, and also possibly emotions like joy, anger, sadness, and so forth. 
#The nrc lexicon categorizes words in a binary fashion ("yes"/"no") into categories of positive,
#negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 
#The bing lexicon categorizes words in a binary fashion into positive and negative categories. 
#The AFINN lexicon assigns words with a score that runs between -5 and 5, 
#with negative scores indicating negative sentiment and positive scores indicating positive sentiment. 
#All of this information is tabulated in the sentiments dataset, 
#and tidytext provides a function get_sentiments() to get specific sentiment lexicons without the columns that are not used in that lexicon.

get_sentiments("afinn")
sentiments
get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("loughran")

#With data in a tidy format, sentiment analysis can be done as an inner join. 
#This is another of the great successes of viewing text mining as a tidy data analysis task; 
#much as removing stop words is an antijoin operation, performing sentiment analysis is an inner join operation.

#What are the most common joy words in Emma ?

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore_case = TRUE)))) %>%
  ungroup()%>%
  unnest_tokens(word,text)

#Now that the text is in a tidy format with one word per row, 
#we are ready to do the sentiment analysis. First, let's use the NRC lexicon and filter() 
#for the joy words. Next, let's filter() the data frame with the text from the books for 
#the words from Emma and then use inner_join() to perform the sentiment analysis. 
#What are the most common joy words in Emma? Let's use count() from dplyr.

nrc_sentiment <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

emma_book <- tidy_books %>%
  filter(book == "Emma") %>%
  select(book,word)

emma_book %>%
  inner_join(nrc_sentiment) %>%
  count(word,sort=TRUE)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book,index = linenumber %/% 80,sentiment)%>%
  spread(sentiment, n, fill = 0)%>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(janeaustensentiment,aes(index,sentiment,fill=book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book,ncol = 2,scales = "free_x")

#2.3 Comparing the three sentiment dictionaries

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>%
                            inner_join(get_sentiments("bing"))%>%
                            mutate(method = "Bing et al"),
                          pride_prejudice %>%
                            inner_join(get_sentiments("nrc"))%>%
                            filter(sentiment %in% c("positive","negative"))%>%
                            mutate(method ="NRC")) %>%
  count(method, index = linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positive - negative)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#---------Most common positive and negative words------------------

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()%>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y") +
  labs(y="Contribution to sentiment",
       x=NULL)+
  coord_flip()

?bind_rows

#-----------------Wordclouds-----------------------------
install.packages("wordcloud")
library(wordcloud)

tidy_books %>%
  anti_join(stop_words)%>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 100))

prideprejudice

#Lots of useful work can be done by tokenizing at the word level,
#but sometimes it is useful or necessary to look at different units of text.
#For example, some sentiment analysis algorithms look beyond only unigrams (i.e. single words) 
#to try to understand the sentiment of a sentence as a whole. 

#R packages included 
# --> coreNLP (T. Arnold and Tilton 2016)
# --> cleanNLP (T. B. Arnold 2016)
# --> sentimentr (Rinker 2017) 
# are examples of such sentiment analysis algorithms. 
#For these, we may want to tokenize text into sentences, and it makes sense to use a new name for the output column in such a case.

PandP_sentences <- data_frame(text=prideprejudice) %>%
  unnest_tokens(sentence,text,token="sentences")

?unnest_tokens
PandP_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter,text,token="regex", pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())
