#Disclaimer : Lyric Analysis is performed using a tutorial online. 

#import libraries
library(dplyr) #data manipulation
library(ggplot2) #data visualization 
library(tidyr) #text mining
#install.packages("gridExtra")
library(gridExtra)
#install.packages("wordcloud2")
library(wordcloud2)

#Read the data
getwd()
prince_org = read.csv("prince_raw_data.csv")
head(prince_org,1)
ncol(prince_org)
nrow(prince_org)
colnames(prince_org)

str(prince_org)

#Extract specific columns and rename them
#Note select() allows you to select and rename columns together!
prince <- prince_org %>%
  select(lyrics = text,song,year,album,peak,
         us_pop=US.Pop,us_rnb = US.R.B)

glimpse(prince[139,])
dim(prince)

# Pattern replacement. Get rid of those pesky contractions by creating a little function
# that handles most scenarios using gsub()

fix.contractions <- function(doc)
{
  doc <- gsub("won't","will not",doc)
  doc <- gsub("can't","can not",doc)
  doc <- gsub("n't","not",doc)
  doc <- gsub("'ll","will",doc)
  doc <- gsub("'re","are",doc)
  doc <- gsub("'ve","have",doc)
  doc <- gsub("'m","am",doc)
  doc <- gsub("'d","would",doc)
  doc <- gsub("'s","",doc)
  return(doc)
}

prince$lyrics <- sapply(prince$lyrics,fix.contractions)

removeSpecialChars <- function(x)
{
  x <- gsub("[^a-zA-Z0-9]"," ",x)
  return(x)
}

#remove special characters
prince$lyrics <- sapply(prince$lyrics,removeSpecialChars)

prince$lyrics <- sapply(prince$lyrics,tolower)

str(prince[139, ]$lyrics, nchar.max = 300)
summary(prince)

prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s",
           ifelse(prince$year %in% 1980:1989, "1980s",
           ifelse(prince$year %in% 1990:1999, "1990s",
           ifelse(prince$year %in% 2000:2009, "2000s",
           ifelse(prince$year %in% 2010:2015, "2010s",
                  "NA"))))))

prince <- prince %>%
  mutate(chart_level =
           ifelse(prince$peak %in% 1:10,"Top 10",
           ifelse(prince$peak %in% 11:100, "Top 100","Uncharted")))

prince <- prince %>%
  mutate(charted =
           ifelse(prince$peak %in% 1:100, "Charted","Uncharted"))

#save the dataset to new .csv for later use in tutorials
write.csv(prince,file="prince_new.csv")

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

prince %>%
  filter(decade != "NA") %>%
  group_by(decade,charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x=decade,y=number_of_songs,fill=charted),stat ='identity') +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  ggtitle("Released songs") +
  labs(x= NULL,y="Song count")

prince %>%
  filter(peak > 0) %>%
  group_by(decade,chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade,y=number_of_songs,fill=chart_level),stat='identity')+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  ggtitle("Charted Songs") +
  labs(x=NULL,y="Song count")

#Songs that hit no 1. on the chart
library(knitr) # for dynamic reporting
#install.packages("kableExtra")
#install.packages("formattable")
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function


prince %>%
  filter(peak == "1") %>%
  select(year,song,peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue","lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen","lightgreen")(peak)) %>%
  kable("html",escape = FALSE,align = 'c',caption = 'Princes No.1 Songs') %>%
  kable_styling(bootstrap_options = 
                  c("striped","condensed","bordered"),
                full_width = FALSE)

#--------------TEXT MINING --------------------------------------

#Text mining can also be thought of as text analytics. The goal is to discover relevant information that
#is possibly unknown or buried beneath the obvious. 
#Natural Language Processing (NLP) is one methodology used in mining text. 
#It tries to decipher the ambiguities in written language by tokenization, clustering, extracting entity 
#and word relationships, and using algorithms to identify themes and quantify subjective information. 

# prince_words_filtered is the tidy text version of the prince data frame without 1) stop words, 2) undesirable words, and 3) 1-3 character words. 

prince
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

prince_words_filtered <- prince %>%
  unnest_tokens(word,lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word)>3)

prince_words_filtered %>%
  filter(word == 'race') %>%
  select(word,song,year,peak,decade,chart_level,charted) %>%
  arrange() %>%
  top_n(10,song) %>%
  mutate(song = color_tile("lightblue","lightblue")(song)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html",escape=FALSE,align='c',caption = 'Tokenized Format Example') %>%
  kable_styling(bootstrap_options = c('striped','condensed','bordered'),
                full_width = FALSE)

full_word_count <- prince %>%
  unnest_tokens(word,lyrics) %>%
  group_by(song,chart_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

full_word_count[1:10,] %>%
  ungroup(num_words,song) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(song = color_tile("lightpink","lightpink")(song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), 
                full_width = FALSE)

full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = chart_level )) +
  ylab("Song Count") + 
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

full_word_count %>%
  filter(chart_level == "Top 10" & num_words > 800) %>%
  left_join(prince_org,by="song") %>%
  select(Song = song,
         "Word count" = num_words,
         "Peak position" = peak,
         "US pop" = US.Pop,
         "US R&B" = US.R.B,
         Canada = CA,
         Ireland = IR) %>%
  kable("html",escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped","condensed","bordered"))


prince_words_filtered %>%
  count(word,sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot() +
  geom_col(aes(word,n),fill =my_colors[4]) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") +
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()

#------------------Word Clouds--------------------------------------

prince_words_count <- prince_words_filtered %>%
  count(word,sort = TRUE)

wordcloud2(prince_words_count[1:300,],size = 0.5)

wordcloud2(prince_words_count[1:300, ],figPath = "guitar.png",
           color = "random-dark",size=1.5)

letterCloud(prince_words_count[1:300,],word ="PRINCE", size=2)

popular_words <- prince_words_filtered %>%
  group_by(chart_level) %>%
  count(word,chart_level,sort=TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(chart_level,n) %>%
  mutate(row = row_number())

popular_words %>%
  ggplot(aes(row,n,fill=chart_level)) +
  geom_col(show.legend = NULL) +
  labs(x=NULL,y="Song Count") +
  ggtitle("Popular words by Chart level") +
  theme_lyrics() +
  facet_wrap(~chart_level,scales="free") +
  scale_x_continuous(# This handles replacement of row
    breaks = popular_words$row, #notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

timeless_words <- prince_words_filtered %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  count(word,decade,sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade,n) %>%
  mutate(row = row_number())

timeless_words %>%
  ggplot(aes(row,n,fill=decade)) +
  geom_col(show.legend = NULL) +
  labs(x=NULL,y="Song count") +
  ggtitle("Timeless Words") +
  theme_lyrics() +
  facet_wrap(~decade,scales="free",ncol=5) +
  scale_x_continuous( #This handles replacement of row
    breaks = timeless_words$row,
    labels = timeless_words$word) +
  coord_flip()

prince_word_lengths <- prince %>%
  unnest_tokens(word,lyrics) %>%
  group_by(song,decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word))

prince_word_lengths %>%
  count(word_length,sort=TRUE) %>%
  ggplot(aes(word_length),binwidth = 10) +
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by=2),
                 show.legend = FALSE) +
  xlab("Word Length") +
  ylab("Word Count") +
  ggtitle("Word length distribution") +
  theme(plot.title=element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

wc <- prince_word_lengths %>%
  ungroup() %>%
  select(word,word_length) %>%
  distinct() %>%
  arrange(desc(word_length))

wordcloud2(wc[1:300, ],
           size =.15,
           minSize = .0005,
           ellipticity = .3,
           rotateRatio = 1,
           fontWeight = "bold")
?wordcloud2

lex_diversity_per_year <- prince %>%
  filter(decade != "NA") %>%
  unnest_tokens(word,lyrics) %>%
  group_by(song,year)%>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(year,lex_diversity)) +
  geom_point(color = my_colors[3],
             alpha = .4,
             size = 4,
             position = "jitter") +
  stat_smooth(color = "black",se = FALSE,method = "lm") +
  geom_smooth(aes( x = year,y = lex_diversity),se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab(" ") +
  ylab(" ") +
  scale_color_manual(values = my_colors) +
  theme_classic() +
  theme_lyrics()

diversity_plot

lex_density_per_year <- prince %>%
  filter(decade != "NA") %>%
  unnest_tokens(word,lyrics) %>%
  group_by(song,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(year,lex_density)) +
  geom_point(color = my_colors[4],
             alpha = .4,
             position = "jitter") +
  stat_smooth(color = "black",
              se = FALSE,
              method = "lm") +
  geom_smooth(aes(x = year, y=lex_density),
              se = FALSE,
              color = "blue",
              lwd = 2) +
  ggtitle("Lexical Density") +
  xlab(" ") +
  ylab(" ") +
  scale_color_manual(values = my_colors) +
  theme_classic() +
  theme_lyrics()

density_plot

chart_history <- prince %>%
  filter(peak > 0 ) %>%
  group_by(year,chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot(aes(year, number_of_songs)) +
  geom_point(color = my_colors[5],
             alpha = 0.4,
             size = 4,
             position = "jitter") +
  geom_smooth(aes(x = year, y = number_of_songs),
              se = FALSE,
              method = "lm",
              color = "black") +
  geom_smooth(aes(x = year, y = number_of_songs),
              se = FALSE,
              color = "blue",
              lwd = 2) +
  ggtitle("Chart History") +
  xlab(" ") +
  ylab(" ") +
  scale_color_manual(values = my_colors) +
  theme_classic() +
  theme_lyrics()

grid.arrange(diversity_plot,density_plot,chart_history,ncol=3)

popular_tdidf_words <- prince %>%
  unnest_tokens(word,lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) >3) %>%
  count(chart_level,word,sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word,chart_level,n)

head(popular_tdidf_words)
