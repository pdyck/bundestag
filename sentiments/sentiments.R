library(tidyverse)
library(tidytext)

# Data Import

sentiws.tsv.col_names <- c("word_pos", "sentiment", "inflections")
sentiws.tsv.positive <- read_tsv("./sentiments/SentiWS_v2.0_Positive.txt", col_names = FALSE)
sentiws.tsv.negative <- read_tsv("./sentiments/SentiWS_v2.0_Negative.txt", col_names = FALSE)
names(sentiws.tsv.positive) <- sentiws.tsv.col_names
names(sentiws.tsv.negative) <- sentiws.tsv.col_names

# Join positive and negative sentiments

sentiws <- bind_rows(sentiws.tsv.positive, sentiws.tsv.negative) %>%
  mutate(
    word = gsub("\\|.*", "", word_pos),
    pos = gsub(".*\\|", "", word_pos)
  ) %>%
  unnest_tokens(token, word) %>%
  select(token, sentiment)

# Data Export

write_csv2(sentiws, "./sentiments/sentiments.csv")
