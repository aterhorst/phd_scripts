
require(tidyverse)
require(tidytext)
require(qdap)

corpus_case_1 <- readtext("~/owncloud/phd_nlp/case 1/*.docx") %>% mutate(case = 1) 
corpus_case_2 <- readtext("~/owncloud/phd_nlp/case 2/*.docx") %>% mutate(case = 2)
corpus_case_3 <- readtext("~/owncloud/phd_nlp/case 3/*.docx") %>% mutate(case = 3)

corpus <- bind_rows(corpus_case_1, corpus_case_2, corpus_case_3) %>%
  rowwise() %>%
  mutate(text = str_replace_all(text,  "’", "'"),
         text = replace_contraction(text)) %>%
  as_tibble()

# do some preliminary cleaning

remove_stuff <- data.frame(word = c("internals",
                                    "reference",
                                    "references",
                                    "coverage",
                                    "coded",
                                    "yeah",
                                    "csiro",
                                    "houston's",
                                    "delaval",
                                    "de",
                                    "laval",
                                    "alan",
                                    "henderson",
                                    "christopher",
                                    "boucher",
                                    "erik",
                                    "eric",
                                    "siedler",
                                    "stephen",
                                    "cahoon",
                                    "dean",
                                    "hart",
                                    "gerd",
                                    "uitdewilligen",
                                    "tom",
                                    "thomas",
                                    "mayne",
                                    "peter",
                                    "tyson",
                                    "kendra",
                                    "kerrisk",
                                    "laurie",
                                    "hooper",
                                    "mark",
                                    "freeman",
                                    "martin",
                                    "paley",
                                    "mikael",
                                    "karttunen",
                                    "nick",
                                    "dornauf",
                                    "nicolas",
                                    "lyon",
                                    "ron",
                                    "mulder",
                                    "benita",
                                    "vincent",
                                    "gary",
                                    "fitt",
                                    "taylor",
                                    "welsh",
                                    "giorgio",
                                    "venturieri",
                                    "paulo",
                                    "de",
                                    "souza",
                                    "oliverio",
                                    "delgado",
                                    "carrillo",
                                    "andy",
                                    "andrew",
                                    "barron",
                                    "peter",
                                    "marendy"),
                            stringsAsFactors = F)


unigrams <- corpus %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  anti_join(remove_stuff) %>%
  filter(!str_detect(word, "[0-9]+"))

bigrams <- corpus %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # remove bigrams that identify companies, places, etc.
  filter(!str_detect(bigram, "[0-9]+"),
         !str_detect(bigram, "fresh freight"),
         !str_detect(bigram, "houston farm"),
         !str_detect(bigram, "srt logistics"),
         !str_detect(bigram, "logistics lab"),
         !str_detect(bigram, "de laval"),
         !str_detect(bigram, "sydney university"),
         !str_detect(bigram, "new south"),
         !str_detect(bigram, "south wales"),
         !str_detect(bigram, "future dairy"),
         !str_detect(bigram, "indistinct words"),
         !str_detect(bigram, "dairy australia")) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% remove_stuff$word, 
         !word2 %in% remove_stuff$word) %>%
  unite(bigram, word1, word2, sep = " ")


require(ggwordcloud)
require(ggthemes)

case_id <- c("1" = "Case 1", 
             "2" = "Case 2", 
             "3" = "Case 3")

wc1 <- ggplot(unigrams %>% 
         group_by(case) %>%
         count(word) %>%
         top_n(50) %>%
         mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
         ungroup(), 
       aes(label = word, 
           angle = angle,
           size = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 18) +
  theme_map() +
  facet_wrap( ~ case, 
              ncol = 1,
              labeller = as_labeller(case_id))

wc2 <- ggplot(bigrams %>% 
         group_by(case) %>%
         count(bigram) %>%
         top_n(50) %>%
         mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
         ungroup(), 
       aes(label = bigram, 
           angle = angle,
           size = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 18) +
  theme_map() +
  facet_wrap( ~ case, 
              ncol = 1,
              labeller = as_labeller(case_id)) 

ggsave("~/owncloud/phd_nlp/bigram.pdf", width = 210, height = 297, units = "mm", wc2)
ggsave("~/owncloud/phd_nlp/unigram.pdf", width = 210, height = 297, units = "mm", wc1)



require(topicmodels)

corpus_dtm <-unigrams %>%
  filter(case == 3) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

model <- LDA(corpus_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))  

corpus_beta <- tidy(model, matrix = "beta")
corpus_gamma <- tidy(model, matrix = "gamma")


# visualise topics

require(scales)

top_terms <- corpus_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- corpus_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))  

gamma_terms %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.005, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.55),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma)) +
  theme(text = element_text(size = 14))




require(widyr)

# Invoke sliding windows function to id skipgrams

slide_windows <- function(tbl, doc_var, window_size) {
  # each word gets a skipgram (window_size words) starting on the first
  # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2
  
  each_total <- tbl %>% 
    group_by(!!doc_var) %>% 
    mutate(doc_total = n(),
           each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
    pull(each_total)
  
  rle_each <- rle(each_total)
  counts <- rle_each[["lengths"]]
  counts[rle_each$values != window_size] <- 1
  
  # each word get a skipgram window, starting on the first
  # account for documents shorter than window
  id_counts <- rep(rle_each$values, counts)
  window_id <- rep(seq_along(id_counts), id_counts)
  
  
  # within each skipgram, there are window_size many offsets
  indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
    map2(rle_each[["values"]] - 1,
         ~ seq.int(.x, .x + .y)) %>% 
    map2(counts, ~ rep(.x, .y)) %>%
    flatten_int() +
    window_id
  
  tbl[indexer, ] %>%
    bind_cols(data_frame(window_id)) %>%
    group_by(window_id) %>%
    filter(n_distinct(!!doc_var) == 1) %>%
    ungroup
}


pmi_corpus <- corpus %>%
  filter(case == 2) %>%
  # tokenise
  unnest_tokens(word, text) %>%
  # get rid of numbers
  filter(!str_detect(word, "[0-9]+")) %>%
  # get rid of stopwords
  anti_join(stop_words) %>%
  anti_join(remove_stuff) %>%
  slide_windows(quo(doc_id), 8) %>%
  pairwise_pmi(word, window_id)

tidy_word_vectors <- pmi_corpus %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)

pca <- tidy_word_vectors %>%
  filter(dimension <= 12) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup %>%
  mutate(item1 = reorder(item1, value)) %>%
  group_by(dimension, item1) %>%
  arrange(desc(value)) %>%
  ungroup %>%
  mutate(item1 = factor(paste(item1, dimension, sep = "__"), 
                        levels = rev(paste(item1, dimension, sep = "__"))),
         dimension = factor(paste0("Dimension ", dimension),
                            levels = paste0("Dimension ", as.factor(1:24)))) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  labs(x = NULL, y = "Value")

ggsave("~/owncloud/phd_nlp/word_vector_pca.pdf", pca)
