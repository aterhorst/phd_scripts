
# Analyse auto-coded documents - interviewee responses to questions only.

# Case 1

require(readtext)

case_1 <- readtext("~/ownCloud/innovation network analysis/qualitative data/NLP/case 1/*.docx")
case_2 <- readtext("~/ownCloud/innovation network analysis/qualitative data/NLP/case 2/*.docx")
case_3 <- readtext("~/ownCloud/innovation network analysis/qualitative data/NLP/case 3/*.docx")

# Do some preliminary cleaning.

require(tidyverse)
require(tidytext)

# load standard stop words

data(stop_words)

# create custom stop words case 1

custom_stop_words_1 <- data.frame(word = c("erik",
                                         "siedler",
                                         "tom",
                                         "mayne",
                                         "dean",
                                         "hart",
                                         "gerd",
                                         "uitdewilligen",
                                         "christopher",
                                         "boucher",
                                         "stephen",
                                         "cahoon",
                                         "reference",
                                         "references",
                                         "coded",
                                         "coverage"), stringsAsFactors = F)

# Create a tidy corpus.

tokenised_1 <- case_1 %>%
  # ditch numbers
  mutate(text = str_remove_all(text, "[0-9]")) %>%
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch superfluous words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words)

bigram_1 <- case_1 %>%
  # ditch numbers
  mutate(text = str_remove_all(text, "[0-9]")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom_stop_words$word, 
         !word2 %in% custom_stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") 

# plot most frequently used words

tokenised_1 %>% 
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# plot most frequently used bigrams

bigram_1 %>% 
  count(bigram, sort = T) %>%
  top_n(50) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Perform sentiment analysis

tokenised_1 %>% 
  # join sentiment library 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  arrange(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# Topic modelling using LDA

tokenised_1_dtm <- tokenised_1 %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

model_1 <- LDA(tokenised_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

get_terms(model_1, 20)


# create custom stop words case 2

custom_stop_words_2 <- data.frame(word = c("ron",
                                         "mulder",
                                         "martin",
                                         "paley",
                                         "nicolas",
                                         "lyon",
                                         "mark",
                                         "freeman",
                                         "kendra",
                                         "kerrisk",
                                         "mikail",
                                         "karttunen",
                                         "laurie",
                                         "hooper",
                                         "nick",
                                         "dornauf",
                                         "reference",
                                         "references",
                                         "coded",
                                         "coverage"), stringsAsFactors = F)

# Create a tidy corpus.

tokenised_2 <- case_2 %>%
  # ditch numbers
  mutate(text = str_remove_all(text, "[0-9]")) %>%
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch superfluous words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words_2)

bigram_2 <- case_2 %>%
  # ditch numbers
  mutate(text = str_remove_all(text, "[0-9]")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom_stop_words_2$word, 
         !word2 %in% custom_stop_words_2$word) %>%
  unite(bigram, word1, word2, sep = " ") 

# plot most frequently used words

tokenised_2 %>% 
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# plot most frequently used bigrams

bigram_2 %>% 
  count(bigram, sort = T) %>%
  top_n(50) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Perform sentiment analysis

tokenised_2 %>% 
  # join sentiment library 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  arrange(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# Topic modelling using LDA

tokenised_2_dtm <- tokenised_2 %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

model_2 <- LDA(tokenised_2_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

get_terms(model_2, 20)
