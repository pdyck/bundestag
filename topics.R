library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)

Yes# Data Import

speakers <- read_csv2("./data/speakers.csv")
sessions <- read_csv2("./data/sessions.csv")
speeches <- read_csv2("./data/speeches.csv")
utterances <- read_csv2("./data/utterances.csv")

data(stopwords_de)
stopwords <- data.frame(token = stopwords_de)
sentiments <- read_csv2("./sentiments/sentiments.csv")

# Create tokens

tokens <- speakers %>%
  left_join(speeches, "speaker_id") %>%
  left_join(sessions, "session_id") %>%
  left_join(utterances, "speech_id") %>%
  unnest_tokens(token, utterance) %>%
  filter(str_detect(token, "[a-z]")) %>%
  anti_join(stopwords, by = "token") %>%
  left_join(sentiments, "token") %>%
  select(session_id, group, speaker_id, speech_id, token, sentiment)

# Topic model

session_dfm <- tokens %>%
  count(session_id, token, sort = TRUE) %>%
  cast_dfm(session_id, token, n)

session_topic_model <- stm(session_dfm, K = 6, verbose = FALSE, init.type = "Spectral")
session_td_beta <- tidy(session_topic_model)

session_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic), term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered()
  