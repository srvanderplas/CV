library(tidyverse)

bibliography <- bib2df::bib2df("data/CV.bib")
bibliography <- bibliography %>% mutate(
  YEAR = ifelse(is.na(YEAR), readr::parse_number(DATE), YEAR),
  YEAR = as.numeric(YEAR)
)

bibliography |>
  mutate(AUTHOR = map_chr(AUTHOR, ~paste(., collapse = " & "))) |>
  mutate(PUBLICATION = if_else(is.na(JOURNAL), PUBLISHER, JOURNAL)) |>
  select(YEAR, AUTHOR, TITLE, PUBLICATION, ISBN, DOI) |>
  write_csv(file = "vanderplas_pubs.csv")
