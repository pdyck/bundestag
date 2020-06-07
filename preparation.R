library(xml2)
library(tidyverse)

# Data Import

protocols <- list.files("./raw", "*-data.xml") %>%
  paste("./raw", ., sep = "/") %>%
  map(~read_xml(.))

# Data Preparation

bundestag.sessions <- protocols %>%
  map_df(~data.frame(
    term = xml_attr(., "wahlperiode"),
    session_number = xml_attr(., "sitzung-nr"),
    date = xml_attr(., "sitzung-datum") %>% as.Date("%d.%m.%Y"),
    stringsAsFactors = FALSE
  )) %>%
  bind_rows() %>%
  unite(session_id, term:session_number) %>%
  select(session_id, date)

bundestag.speakers <- protocols %>%
  map(~xml_find_all(., ".//rednerliste/redner")) %>%
  flatten() %>%
  map_df(~data.frame(
    speaker_id = xml_attr(., "id"),
    title = xml_find_first(., "name/titel") %>% xml_text() %>% trimws(),
    firstname = xml_find_first(., "name/vorname") %>% xml_text() %>% trimws(),
    affix = xml_find_first(., "name/namenszusatz") %>% xml_text() %>% trimws(),
    lastname = xml_find_first(., "name/nachname") %>% xml_text() %>% trimws(),
    role = xml_find_first(., "name/rolle/rolle_kurz") %>% xml_text() %>% trimws(),
    group = xml_find_first(., "name/fraktion") %>% xml_text() %>% trimws() %>% gsub("/ ", "/", .),
    stringsAsFactors = FALSE
  )) %>%
  bind_rows() %>%
  distinct(speaker_id, .keep_all = TRUE)

bundestag.speeches <- protocols %>%
  map(~xml_find_all(., ".//sitzungsverlauf/tagesordnungspunkt/rede")) %>%
  flatten() %>%
  map_df(~data.frame(
    speech_id = xml_attr(., "id"),
    speaker_id = xml_find_first(., "p/redner") %>% xml_attr("id"),
    term = xml_root(.) %>% xml_attr("wahlperiode"),
    session_number = xml_root(.) %>% xml_attr("sitzung-nr"),
    stringsAsFactors = FALSE
  )) %>%
  bind_rows() %>%
  unite(session_id, term:session_number) %>%
  select(session_id, speech_id, speaker_id)

bundestag.utterances <- protocols %>%
  map(~xml_find_all(., ".//sitzungsverlauf/tagesordnungspunkt/rede")) %>%
  flatten() %>%
  map(~xml_find_all(., "p")) %>%
  flatten() %>%
  map_df(~data.frame(
    speech_id = xml_parent(.) %>% xml_attr("id"),
    attr_klasse = xml_attr(., "klasse"),
    utterance = xml_text(.),
    stringsAsFactors = FALSE
  )) %>%
  bind_rows() %>%
  filter(attr_klasse %in% c("J", "J_1", "O")) %>%
  select(speech_id, utterance)

# Data Export

write_csv2(bundestag.sessions, "./data/sessions.csv")
write_csv2(bundestag.speakers, "./data/speakers.csv")
write_csv2(bundestag.speeches, "./data/speeches.csv")
write_csv2(bundestag.utterances, "./data/utterances.csv")
