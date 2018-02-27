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
text_df <- data.frame(text)
text_df
