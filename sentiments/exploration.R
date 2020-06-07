library(tidyverse)
library(tidytext)

# Data Import

speakers <- read_csv2("./data/speakers.csv")
sessions <- read_csv2("./data/sessions.csv")
speeches <- read_csv2("./data/speeches.csv")
utterances <- read_csv2("./data/utterances.csv")

sentiments <- read_csv2("./sentiments/sentiments.csv")

# Exploration

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(speaker_id) %>%
  arrange(-n) %>%
  left_join(speakers, by = "speaker_id") %>%
  select(firstname, lastname, group, number_of_speeches = n) %>%
  
  top_n(10, wt = number_of_speeches) %>%
  
  ggplot(aes(x = reorder(lastname, -number_of_speeches), y = number_of_speeches, fill = lastname)) +
  geom_bar(stat = "identity")

speakers %>%
  left_join(speeches, by = "speaker_id") %>%
  count(group) %>%
  select(group, number_of_speeches = n) %>%
  
  top_n(10, wt = number_of_speeches) %>%
  
  ggplot(aes(x = reorder(group, -number_of_speeches), y = number_of_speeches, fill = group)) +
  geom_bar(stat = "identity")
