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
