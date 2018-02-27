#The tidy text format as being a table with one-token-per-row. 
#A token is a meaningful unit of text, such as a word, that we are interested in using for analysis
#Tokenization is the process of splitting text into tokens. 
#This one-token-per-row structure is in contrast to the ways text is often stored in current analyses, perhaps as strings or in a document-term matrix. 
#For tidy text mining, the token that is stored in each row is most often a single word,
#but can also be an n-gram, sentence, or paragraph. 
#In the tidytext package, we provide functionality to tokenize by commonly used units of text like these and convert to a one-term-per-row format.

text <- c("Because I could not stop for death - ",
           "He kindly stopped for me - ",
           "The Carriage held but just Ourselves - ",
            "and Immortality"
         )
#In order to turn this into a tidy text data, we first need to put it into a dataframe. 
library(dplyr)
text_df <- data_frame(line = 1:4, text)
text_df

#What does it mean that this data frame has printed out as a "tibble"? 
#A tibble is a modern class of data frame within R, available in the dplyr and tibble packages, 
#that has a convenient print method, will not convert strings to factors, and does not use row names.
#Tibbles are great for use with tidy tools.

#This text_df data frame containing text isn't yet compatible with tidy text analysis, though. 
#We can't filter out words or count which occur most frequently, 
#since each row is made up of multiple combined words. 
#We need to convert this so that it has one-token-per-document-per-row.

install.packages("tidytext")
library(tidytext)

text_df %>%
  unnest_tokens(word,text)

# Tidying the works of Jane Austen
install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                                ignore_case = TRUE)))) %>%
  ungroup()

original_books
tidy_books <- original_books %>%
  unnest_tokens(word,text)

tidy_books

#Now that the data is in one-word-per-row format, 
#we can manipulate it with tidy tools like dplyr. 
#Often in text analysis, we will want to remove stop words; 
#stop words are words that are not useful for an analysis, 
#typically extremely common words such as "the", "of", "to", and so forth in English. 
#We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join().

data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books

library("ggplot2")
tidy_books %>%
  count(word,sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#---------- GUTENBURGR--------------------

install.packages("gutenbergr")
library(gutenbergr)

gw <- gutenberg_works()
hgwells <- gutenberg_download(c(35,36,5230,159))
hgwells

tidy_hgwells <- hgwells %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

#Just for kicks, what are the most common words in these novels of H.G. Wells

tidy_hgwells %>%
  count(word,sort=TRUE)%>%
  arrange()

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word,sort = TRUE)
