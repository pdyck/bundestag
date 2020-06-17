library(tidyverse)
library(tidytext)
library(lsa)

# Data Import

speakers <- read_csv2("./data/speakers.csv")
sessions <- read_csv2("./data/sessions.csv")
speeches <- read_csv2("./data/speeches.csv")
utterances <- read_csv2("./data/utterances.csv")

data(stopwords_de)
stopwords <- data.frame(token = stopwords_de)
sentiments <- read_csv2("./sentiments/sentiments.csv")

# Exploration

colors <- c(
  "CDU/CSU" = "#000000",
  "SPD" = "#ff0000",
  "AfD" = "#009ee0",
  "FDP" = "#ffcc00",
  "BÜNDNIS 90/DIE GRÜNEN" = "#679801",
  "DIE LINKE" = "#aa0065",
  "fraktionslos" = "#808080"
)

# ---

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(speaker_id) %>%
  arrange(-n) %>%
  left_join(speakers, by = "speaker_id") %>%
  select(firstname, lastname, group, number_of_speeches = n) %>%
  top_n(10, wt = number_of_speeches) %>%
  ggplot(aes(x = reorder(lastname, -number_of_speeches), y = number_of_speeches, fill = lastname)) +
    geom_bar(stat = "identity") +
    ggtitle("Anzahl Reden pro Redner (Top 10)") +
    xlab("Redner") +
    ylab("Anzahl Reden")

# ---

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(group) %>%
  select(group, number_of_speeches = n) %>%
  top_n(10, wt = number_of_speeches) %>%
  ggplot(aes(x = reorder(group, -number_of_speeches), y = number_of_speeches, fill = group)) +
    geom_bar(stat = "identity") +
    ggtitle("Anzahl Reden pro Fraktion") +
    xlab("Fraktion") +
    ylab("Anzahl Reden") +
    scale_fill_manual("Fraktionen", values = colors,  na.value = "grey")
 
# ---

speeches_per_speaker.speeches <- left_join(speeches, speakers, "speaker_id") %>%
  count(group)

speeches_per_speaker.speakers <- speakers %>% count(group)

speeches_per_speaker <- left_join(speeches_per_speaker.speeches, speeches_per_speaker.speakers, "group") %>%
  mutate(ratio = n.x / n.y) %>%
  arrange(desc(ratio)) %>%
  select(group, ratio)

speeches_per_speaker %>%
  ggplot(aes(reorder(group, -ratio), ratio, fill = group)) +
    geom_bar(stat = "identity") +
    ggtitle("Durchschnittliche Anzahl Reden pro MdB nach Fraktion") +
    xlab("Fraktion") +
    ylab("Durschnittliche Anzahl Reden") +
    scale_fill_manual("Fraktionen", values = colors, na.value = "grey")

# ---

tokens <- speakers %>%
  left_join(speeches, "speaker_id") %>%
  left_join(sessions, "session_id") %>%
  left_join(utterances, "speech_id") %>%
  unnest_tokens(token, utterance) %>%
  filter(str_detect(token, "[a-z]")) %>%
  anti_join(stopwords, by = "token") %>%
  left_join(sentiments, "token") %>%
  select(session_id, group, speaker_id, speech_id, token, sentiment)

# ---

labertaschen <- tokens %>%
  count(speech_id) %>%
  left_join(speeches, "speech_id") %>%
  group_by(speaker_id) %>%
  summarise(mean = mean(n, na.rm = TRUE)) %>%
  left_join(speakers, "speaker_id") %>%
  select(firstname, lastname, group, mean) %>%
  arrange(desc(mean))

# ---

group_sentiments <- tokens %>%
  filter(!is.na(group)) %>%
  filter(group != "fraktionslos") %>%
  group_by(session_id, group) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = as.Date(date)) %>%
  select(session_id, date, group, mean_sentiment)

ggplot(group_sentiments, aes(x = date, y = mean_sentiment, group = group, color = group_sentiments$group)) +
  geom_smooth(se = FALSE, span = 0.5) +
  scale_color_manual("Fraktionen", values = colors) +
  ggtitle("Sentiment der Fraktionen") +
  xlab("Zeit") +
  ylab("Sentiment")

# ---

tokens %>%
  group_by(session_id) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = as.Date(date)) %>%
  select(date, mean_sentiment) %>%
    ggplot(aes(x = date, y = mean_sentiment)) +
      geom_smooth(se = TRUE, span = 0.25) +
      ggtitle("Sentiment im Bundestag") +
      xlab("Zeit") +
      ylab("Sentiment")

# --

speaker_sentiments <- tokens %>%
  group_by(speaker_id) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  left_join(speakers, "speaker_id")

speaker_sentiments %>%
  ggplot(aes(x = mean_sentiment, fill = group)) +
  geom_histogram(bins = 50) +
  ggtitle("Sentiment der MdB") +
  xlab("Sentiment") +
  ylab("Anzahl MdB") +
  scale_fill_manual("Fraktionen", values = colors, na.value = "grey") +
  facet_wrap(~group)

# ---

tokens %>%
  distinct(token, sentiment) %>%
  arrange(sentiment)

tokens %>%
  count(token, sentiment) %>%
  filter(is.na(sentiment)) %>%
  arrange(desc(n))

# ---

tf_idf_sessions <- tokens %>%
  count(session_id, token, sort = TRUE) %>%
  bind_tf_idf(token, session_id, n) %>%
  group_by(session_id) %>%
  top_n(10, tf_idf) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = as.Date(date))

tf_idf_sessions %>%
  filter(session_id %in% c("19_154", "19_155", "19_156", "19_157", "19_158", "19_159", "19_160", "19_161", "19_162", "19_163", "19_164")) %>%
  ggplot(aes(x = reorder(token, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~date, scales = "free") +
  ggtitle("Top Themen der letzten Wochen") +
  xlab("Tf-idf-Maß") +
  ylab("Token")

# ---

tf_idf_sessions_top_25 <- tf_idf_sessions %>%
  ungroup() %>%
  count(token) %>%
  arrange(desc(n)) %>%
  top_n(5, n) %>%
  pull(token)

tf_idf_sessions %>%
  filter(token %in% tf_idf_sessions_top_25) %>%
  ggplot(aes(x = date, y = tf_idf, group = token, fill = token)) +
  geom_line()

# ---

bigrams <- speakers %>%
  left_join(speeches, "speaker_id") %>%
  left_join(sessions, "session_id") %>%
  left_join(utterances, "speech_id") %>%
  unnest_tokens(bigram, utterance, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(str_detect(word1, "[a-z]")) %>%
  filter(str_detect(word2, "[a-z]")) %>%
  filter(!word1 %in% stopwords$token) %>%
  filter(!word2 %in% stopwords$token) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  select(session_id, group, speaker_id, speech_id, bigram)

bigram_tf_idf_sessions <- bigrams %>%
  count(session_id, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, session_id, n) %>%
  group_by(session_id) %>%
  top_n(10, tf_idf) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = as.Date(date))

# ---

tokens %>%
  filter(token %in% c("klimakrise", "coronakrise", "hongkong", "mali", "organspende", "eu")) %>%
  group_by(session_id) %>%
  count(token) %>%
  left_join(sessions, "session_id") %>%
  select(date, count = n, token) %>%
  ggplot(aes(x = date, y = count)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ token, scales = "free_y")
