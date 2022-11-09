# Script to convert talk spreadsheet into CV sections
library(dplyr)
library(lubridate)
library(googlesheets4)
sheet_link <- "https://docs.google.com/spreadsheets/d/1zOKie2rqIcxQMuAzn1g7K-2_O5yOhHo2QD7EAGjeFzs/edit?usp=sharing"
talk_data <- read_sheet(sheet_link)

source("generic.R")

# Update to match your info
talk_order <- c("Invited", "Contributed", "Seminars")

# Create columns for year - CV doesn't need that much detail
talk_data <- talk_data %>% 
  mutate(year = year(Date)) %>%
  # Convert NAs to ''
  mutate(across(where(is.character), ~ifelse(is.na(.), "", .))) %>%
  mutate(Type = factor(Type, levels = talk_order, ordered = T)) %>%
  mutate(slides = sprintf("\\href{%s}{Link}", escapeLatexSpecials(Link)) %>% 
           gsub("^.href.{3}Link.$", "", .) %>%
           gsub("$(.){2}", "\1", .)) %>%
  arrange(desc(Date), Type)

print_talks <- function(df, include_links = F) {
  sprintf("\\tldatecventry{%d}{%s}{%s}{%s}{%s}{%s} %% %s", 
          df$year, df$Title, 
          df$Event, df$Event2, 
          df$Location, ifelse(include_links, df$slides, ""),
          as.character(df$Date))
}

print_heading <- function(type, data, heading_type = "\\subsection{%s}", spacing = "\\medskip") {
  c(
    sprintf(heading_type, type),
    print_talks(data),
    spacing, 
    ""
  )
}

tmp <- talk_data %>% nest_by(Type)
info <- purrr::map2(tmp$Type, tmp$data, print_heading)
  
writeLines(unlist(info), con = "talks.tex")
