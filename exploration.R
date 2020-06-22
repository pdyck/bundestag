library(tidyverse)
library(lubridate)
library(tidytext)
library(lsa)
library(wordcloud)

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

# --- Most speeches by speaker

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(speaker_id) %>%
  arrange(-n) %>%
  left_join(speakers, by = "speaker_id") %>%
  select(firstname, lastname, group, number_of_speeches = n) %>%
  top_n(10, wt = number_of_speeches) %>%
  ggplot(aes(x = reorder(lastname, -number_of_speeches), y = number_of_speeches)) +
    geom_bar(stat = "identity") +
    ggtitle("Wer hält die meisten Reden?") +
    xlab("Redner") +
    ylab("Anzahl Reden")

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(speaker_id) %>%
  left_join(speakers, by = "speaker_id") %>%
  select(firstname, lastname, group, number_of_speeches = n) %>%
  ggplot(aes(number_of_speeches)) +
  geom_histogram(bins = 60, show.legend = FALSE)
  
# --- Most speeches by group

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(group) %>%
  select(group, number_of_speeches = n) %>%
  top_n(10, wt = number_of_speeches) %>%
  ggplot(aes(x = reorder(group, -number_of_speeches), y = number_of_speeches, fill = group)) +
    geom_bar(stat = "identity") +
    ggtitle("In welcher Fraktion werden die meisten Reden gehalten?") +
    xlab("Fraktion") +
    ylab("Anzahl Reden") +
    scale_fill_manual("Fraktionen", values = colors,  na.value = "grey")
 
# --- Most speeches by group relative to its members

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
    ggtitle("In welcher Fraktion werden die meisten Reden pro Mitglied gehalten?") +
    xlab("Fraktion") +
    ylab("Anzahl Reden") +
    scale_fill_manual("Fraktionen", values = colors, na.value = "grey")

# --- Tokenization and stopwords

tokens <- speakers %>%
  left_join(speeches, "speaker_id") %>%
  left_join(sessions, "session_id") %>%
  left_join(utterances, "speech_id") %>%
  unnest_tokens(token, utterance) %>%
  filter(str_detect(token, "[a-z]")) %>%
  anti_join(stopwords, by = "token") %>%
  left_join(sentiments, "token") %>%
  select(session_id, group, speaker_id, speech_id, token, sentiment)

# --- Who used the most words during speeches?

labertaschen <- tokens %>%
  count(speech_id) %>%
  left_join(speeches, "speech_id") %>%
  group_by(speaker_id) %>%
  summarise(mean = mean(n, na.rm = TRUE)) %>%
  left_join(speakers, "speaker_id") %>%
  select(firstname, lastname, group, mean) %>%
  arrange(desc(mean))

# --- Sentiments aggregated to groups by session

group_sentiments <- tokens %>%
  filter(!is.na(group)) %>%
  filter(group != "fraktionslos") %>%
  group_by(session_id, group) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = ymd(date)) %>%
  select(session_id, date, group, mean_sentiment)

ggplot(group_sentiments, aes(x = date, y = mean_sentiment, group = group, color = group_sentiments$group)) +
  geom_smooth(se = FALSE, span = 0.5) +
  scale_color_manual("Fraktionen", values = colors) +
  ggtitle("Sentiment der Fraktionen") +
  xlab("Zeit") +
  ylab("Sentiment")

# --- Overall sentiment by session

tokens %>%
  group_by(session_id) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  left_join(sessions, "session_id") %>%
  mutate(date = ymd(date)) %>%
  select(date, mean_sentiment) %>%
    ggplot(aes(x = date, y = mean_sentiment)) +
      geom_smooth(se = TRUE, span = 0.25) +
      ggtitle("Sentiment im Bundestag") +
      xlab("Zeit") +
      ylab("Sentiment")

# --- Distribution of speaker sentiments in groups

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

# --- Highest sentiment scores for tokens

tokens %>%
  distinct(token, sentiment) %>%
  arrange(sentiment)

tokens %>%
  count(token, sentiment) %>%
  filter(is.na(sentiment)) %>%
  arrange(desc(n))

# --- Tf-idf score by session (relevant topics in sessions)

tf_idf_sessions <- tokens %>%
  count(session_id, token, sort = TRUE) %>%
  bind_tf_idf(token, session_id, n) %>%
  group_by(session_id) %>%
  top_n(25, tf_idf) %>%
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

# --- Explore relevant topics

tf_idf_sessions_top_25 <- tf_idf_sessions %>%
  group_by(token) %>%
  summarise(n = sum(n), tf_idf = sum(tf_idf)) %>%
  top_n(50, tf_idf)

# --- Explore bigrams

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

bigram_tf_idf_sessions_top <- bigram_tf_idf_sessions %>%
  group_by(bigram) %>%
  summarise(n = sum(n), tf_idf = sum(tf_idf)) %>%
  top_n(50, tf_idf) %>%
  arrange(desc(tf_idf))

# --- Frequency of selected tokens in sessions

tokens %>%
  filter(token %in% c("co2", "mietpreisbremse", "pandemie", "haushalt", "bundeswehr", "bafög")) %>%
  group_by(session_id) %>%
  count(token) %>%
  left_join(sessions, "session_id") %>%
  select(date, count = n, token) %>%
  ggplot(aes(x = date, y = count)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ token) +
    ggtitle("Häufigkeit von Token") +
    xlab("Datum") +
    ylab("Anzahl")

# --- Top positive and negative sentiment tokens in corpus

top_positive_sentiment <- tokens %>%
  distinct(token, .keep_all = TRUE) %>%
  top_n(15, sentiment) %>%
  select(token, sentiment)

top_negative_sentiment <- tokens %>%
  distinct(token, .keep_all = TRUE) %>%
  top_n(-15, sentiment) %>%
  select(token, sentiment)

bind_rows(top_positive_sentiment, top_negative_sentiment) %>%
  ggplot(aes(x = reorder(token, sentiment), y = sentiment, fill = sentiment > 0)) +
  geom_col() +
  coord_flip() +
  ggtitle("Sentiments") +
  xlab("Token") +
  ylab("Sentiment") +
  theme(legend.position = "none")

# --- Tf-idf for groups

tf_idf_groups <- tokens %>%
  filter(speaker_id != "11004393") %>% # filter out Frisian speeches
  count(group, token) %>%
  bind_tf_idf(token, group, n)

tf_idf_groups %>%
  filter(group != "fraktionslos") %>%
  group_by(group) %>%
  top_n(10, tf_idf) %>%
  ggplot(aes(x = reorder(token, tf_idf), y = tf_idf, fill = group)) +
    geom_col() +
    scale_fill_manual(values = colors) +
    coord_flip() +
    theme(legend.position = "none") +
    facet_wrap(~ group, scales = "free_y") +
    ggtitle("Token mit dem höchsten Tf-idf-Maß in den Fraktionen") +
    xlab("") +
    ylab("Tf-idf-Maß")

# --- Tf-idf for selected speakers

tf_idf_speakers <- tokens %>%
  count(speaker_id, token) %>%
  bind_tf_idf(token, speaker_id, n)

tf_idf_speakers %>%
  left_join(speakers, "speaker_id") %>%
  filter(speaker_id %in% c(
    "11004724", # AfD Gauland
    "11004930", # AfD Weidel
    "11004097", # FDP Lindner
    "11001235", # FDP Kubicki
    "11002746", # GRÜ Özdemir
    "11004245", # GRÜ Baerbock
    "11003715", # SPD Klingbeil
    "11004656", # CDU Amthor
    "11004938", # CDU Ziemiak
    "11004267", # SPD Esken
    "11000756", # LIN Gysi
    "11004183"  # LIN Wagenknecht
  )) %>%
  group_by(speaker_id) %>%
  top_n(10, tf_idf) %>%
  ggplot(aes(x = reorder(token, tf_idf), y = tf_idf, fill = group)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~ lastname, scales = "free_y") +
  ggtitle("Token mit dem höchsten Tf-idf-Maß nach Redner") +
  xlab("") +
  ylab("Tf-idf-Maß")
  
# --- AfD specific tf-idf analysis

sentiments_speakers_afd <- tokens %>%
  filter(group == "AfD") %>%
  group_by(speaker_id) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))

tf_idf_speakers_afd <- tokens %>%
  filter(group == "AfD") %>%
  count(speaker_id, token) %>%
  bind_tf_idf(token, speaker_id, n)

tf_idf_speakers_afd %>%
  left_join(speakers, "speaker_id") %>%
  group_by(speaker_id) %>%
  top_n(10, tf_idf) %>%
  ggplot(aes(x = reorder(token, tf_idf), y = tf_idf, fill = group)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~ lastname, scales = "free_y")

tf_idf_groups %>%
  filter(group == "AfD") %>%
  with(wordcloud(token, tf_idf, random.order = FALSE, max.words = 50))

# --- Speakers that were mentioned by other speakers

mentions <- speakers %>%
  mutate(token = tolower(lastname)) %>%
  filter(!token %in% c("weiß", "lange", "schön", "grund", "ernst", "neu", "klare", "kraft", "müller", "frei", "groß")) %>%
  left_join(tokens, "token") %>%
  select(session_id, speech_id, mentioned_by = speaker_id.y, mentioned = speaker_id.x)

mentions %>%
  count(mentioned, sort = TRUE) %>%
  mutate(speaker_id = mentioned) %>%
  left_join(speakers, "speaker_id")

mentions %>%
  filter(mentioned == "11001478") %>%
  mutate(speaker_id = mentioned_by) %>%
  left_join(speakers, "speaker_id") %>%
  count(group)

mentions %>%
  mutate(speaker_id = mentioned_by) %>%
  left_join(speakers, "speaker_id") %>%
  count(group)

mentions %>%
  mutate(speaker_id = mentioned) %>%
  left_join(speakers, "speaker_id") %>%
  count(lastname, sort = TRUE)

# --- Gendering

gendered_tokens <- tokens %>%
  filter(endsWith(token, "innen")) %>%
  filter(!token %in% c(
    "beginnen", "zurückgewinnen", "darinnen", "entrinnen", "erkenntnisgewinnen", "jahresgewinnen", "abzugewinnen",
    "effizienzgewinnen", "weiterspinnen", "hallenserinnen", "städterinnen", "rationalisierungsgewinnen", "fortspinnen",
    "entsinnen", "spinnen", "besinnen", "zurückzugewinnen", "zurückbesinnen", "sinnen", "finnen", "binnen", "ansinnen",
    "innen", "drinnen", "gewinnen", "abgewinnen", "zinsgewinnen", "buchgewinnen", "bankgewinnen", "umverteilungsgewinnen"
  ))
  
tokens %>%
  count(group) %>%
  left_join(gendered_tokens %>% count(group), "group") %>%
  mutate(ratio = n.y / n.x) %>%
  arrange(desc(ratio)) %>%
  ggplot(aes(x = reorder(group, -ratio), y = ratio, fill = group)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none") +
  ggtitle("Verhätnis von gegenderten Wörtern zu allen Wörtern") +
  xlab("Fraktion") +
  ylab("Verhältnis")
