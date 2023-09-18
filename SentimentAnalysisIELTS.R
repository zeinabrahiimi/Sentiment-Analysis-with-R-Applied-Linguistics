
# Installing packages
install.packages(c("readxl", "tidytext", "dplyr", "tidyr", "ggplot2", 
                   "textdata", "stringr", "igraph", 
                   "ggraph", "widyr", "tidyselect", "readr"))

# Loading packages
library(readxl)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(textdata)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
library(tidyselect)
library(readr)

# Reading data from CSV
library(readr)
text <- read_csv("ielts_writing_dataset.csv") 


# Preprocessing text data
words <- text %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% # remove extra words (link, emoji,etc.)
  unnest_tokens(word, text, token = "regex", pattern = reg) %>% #turn text into bag of words
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


# Sentiment Analysis using NRC lexicon
nrc <- get_sentiments('nrc')
sentimentNRC <- words %>%
  inner_join(nrc, by = "word")


# Writing the sentiment analysis results to a CSV file named 'l_nrc_IELTS.csv' 
write_csv(sentimentNRC, 'l_nrc_IELTS.csv', file = "UTF-8")

# Creating a new graphics device with a specified width and height
dev.new(width=20, height=10, unit="in")

# Calculating word counts for each word-sentiment combination
nrc_word_counts <- words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Grouping the word counts by sentiment
nrc_word_counts %>%
  group_by(sentiment) %>%
  
  # Selecting the top 10 words for each sentiment
  top_n(10) %>%
  ungroup() %>%
  
  # Reordering the words based on their frequency within each sentiment
  mutate(word = reorder(word, n)) %>%
  
  # Creating a bar plot with word frequency on the y-axis, colored by sentiment
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  
  
  # Adding titles and labels
  labs(title = "Sentiment for IELTS Essays",
       y = "Contribution to sentiment",
       x = NULL) + # No x-axis label
  # Flip the coordinate system for horizontal bars
  coord_flip()

# Saving the resulting plot 
ggsave('sen.tiff', sen, device = "tiff", dpi = 900)



