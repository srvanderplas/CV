#!/usr/bin/Rscript

# Script to get spreadsheet data

suppressPackageStartupMessages({
  library(dplyr) # verbs
  library(purrr) # list/df manipulation
  library(magrittr) # pipe and extract2()
  library(readxl) # read xlsx
})


# library(googlesheets4) # API access
# token <- gs4_auth(cache = T)
# 
# # Update to match your info
# sheet_link <- "https://docs.google.com/spreadsheets/d/1zOKie2rqIcxQMuAzn1g7K-2_O5yOhHo2QD7EAGjeFzs/edit#gid=1325854112"

sheet_link <- "data/CV.xlsx"

name_map <- tibble(
  tab_name = c("Education", "Experience", "Grants", "PRS", "Awards", 
               "Software", "Talks", "Reviewing", "Outreach", 
               "Teaching", "CourseDev", 
               "Mentoring", "Committees", 
               "Service", "Workshops", 
               "ProfDev"),
  obj_name = c("edu_data", "exp_data", "grant_data", "prs_data", "award_data",  
               "sw_data", "talk_data", "reviewing_data", "outreach_data", 
               "teach_data", "coursedev_data", 
               "mentor_data", "committee_data", 
               "service_data", "workshop_data", 
               "profdev_data")
)

sh_names <- excel_sheets(sheet_link)

warning(
  paste(
    paste(setdiff(sh_names, name_map$tab_name), collapse = ", "), 
    " does not have a corresponding method")
)

suppressMessages({
  tmp <- tibble(tab_name = sh_names) %>%
    inner_join(name_map) %>%
    mutate(data = map(tab_name, ~read_excel(sheet_link, sheet = .)))
    # mutate(data = map(tab_name, ~read_sheet(sheet_link, sheet = .)))
  
  walk2(tmp$obj_name, tmp$data, ~assign(.x, value = .y, envir = .GlobalEnv))
})

# save.image("CV.RData")
