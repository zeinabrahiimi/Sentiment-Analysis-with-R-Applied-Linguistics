# Create a regex pattern
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"


# Install packages
install.packages(c("readxl", "tidytext", "dplyr", "tidyr", "ggplot2", 
                   "textdata", "stringr", "igraph", 
                   "ggraph", "widyr", "tidyselect", "readr"))

# Load packages
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


library(readr)
text <- read_csv("ielts_writing_dataset.csv") # kare KHODAM


words <- text %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% # remove extra words (link, emoji,etc.)
  unnest_tokens(word, text, token = "regex", pattern = reg) %>% #turn text into bag of words
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

nrc <- get_sentiments('nrc')

sentimentNRC <- words %>%
  inner_join(nrc, by = "word")

save_as_csv(sentimentNRC, 'l_nrc_IELTS.csv', fileEncoding = "UTF-8")

dev.new(width=20, height=10, unit="in")


nrc_word_counts <- words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment for IELTS Essays",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


sen <- nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment for IELTS Essays",
       y = "Sentiment Analysis",
       x = NULL) +
  coord_flip()
ggsave('sen.tiff', sen, device = "tiff", dpi = 900)


