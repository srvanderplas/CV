#!/usr/bin/Rscript

# Script to convert spreadsheet into CV sections

suppressPackageStartupMessages({
  library(dplyr) # verbs
  library(tidyr) # nest/unnest, mostly
  library(lubridate) # date handling
  library(glue) # pasting stuff together
  library(stringr) # string manipulation
  library(scales) # formatting numbers as e.g. dollars
  library(purrr) # list/df manipulation
  library(magrittr) # pipe and extract2()
})

# Get data read into separate tables
source("code/get_data.R")

# Functions used to build CV files
source("code/build_functions.R")

talk_order <- c("Invited", "Contributed", "Seminars")
grant_order <- c("Under Review", "Funded", "Not Funded")
service_order <- c("discipline", "institution", "department")

if (!dir.exists("tex-deps")) dir.create("tex-deps")

# --- Process Education --------------------------------------------------------
if (nrow(edu_data) > 0) {
  edu_data %>%
    mutate(across(c(start, end), ymd)) %>%
    arrange(desc(end), desc(start)) %>%
    mutate(across(c(start, end), year)) %>%
    make_generic(datenames = c("start", "end"), 
                 fieldnames = c("degree", "major", "school", "other"),
                 get_year = F) %>%
    add_heading(make_heading("Education", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "tex-deps/edu.tex")
}

# --- Process Experience -------------------------------------------------------
if (nrow(exp_data) > 0) {
  exp_data %>%
    mutate(across(c(start, end), ymd)) %>%
    filter(include == 1) %>%
    mutate(end = if_else(is.na(end), today(), end)) %>%
    mutate(end = if_else(end==0, today(), end)) %>%
    arrange(desc(end), desc(start)) %>%
    mutate(end = if_else(end == today(), NA, end)) %>%
    mutate(texlines = make_generic(., datenames = c("start", "end"), 
                                   fieldnames = c("position", "department", 
                                                  "location", "other"))) %>%
    extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Professional Experience", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "tex-deps/exp.tex")
}

# --- Process Awards -----------------------------------------------------------
if (nrow(award_data) > 0) {
  award_data %>%
    arrange(desc(start), desc(end), type) %>%
    filter(include) %>%
    make_generic(datenames = c("start", "end"),
                 fieldnames = c("title", "organization", "other")) %>%
    add_heading(make_heading("Awards", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "tex-deps/awards.tex")
}
# --- Process Grants -----------------------------------------------------------
if (nrow(grant_data) > 0) {
  
  # Format grant data better
  grant_data_fix <- grant_data %>%
    mutate(across(ends_with("_total"), as.numeric)) %>%
    mutate(status = factor(status, levels = grant_order, ordered = T)) %>%
    arrange(status, desc(year_applied), desc(end), desc(start)) %>%
    # Lots more info here, so have to do some extra formatting...
    mutate(funding = paste(funding_org, funding_title, sep = ": "),
           grant_total_str = dollar_format()(grant_total),
           sub_total_str = dollar_format()(sub_total),
           amount_sub = sprintf("Total: %s, Sub: %s (%s)", grant_total_str, sub_total_str, sub_note) %>%
             gsub(" \\(\\)$", "", .) %>%
             gsub(" \\(NA\\)", "", .),
           amount_nosub = sprintf("Total: %s", grant_total_str),
           amount = if_else(subaward, amount_sub, amount_nosub),
           start = if_else(status != "Funded", year_applied, start),
           end = if_else(status != "Funded", year_applied, end))

  grant_data_fix %>% 
    nest(data = -status) %>%
    mutate(
      header = make_heading(status),
      texlines = purrr::map(data, ~make_generic(., 
        datenames = c("start", "end"),
        fieldnames = c("funding", "grant_title", 
                       "grant_role", "amount"))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    unnest(status) %>%
    extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Grants", "section")) %>%
    writeLines(con = "tex-deps/grants.tex")
}


# --- Process Talks ------------------------------------------------------------
if(nrow(talk_data) > 0) {
  talk_data %>%
    mutate(Type = factor(Type, levels = talk_order, ordered = T)) %>%
    mutate(slideLink = ifelse(is.na(Link), "", 
                              sprintf("\\href{%s}{\\faIcon{chalkboard}}", Link)),
           Title = paste(Title, slideLink),
           start = Date,
           end = Date) %>%
    arrange(Type, desc(Date)) %>%
    nest(data = -Type) %>%
    mutate(
      header = make_heading(Type),
      texlines = .$data %>% 
        map(
          ~make_generic(., datenames = c("start", "end"), get_year = T,
                        fieldnames = c("Title", "Event", "Event2", "Location"))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(Type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_entry_top("\\cvitem{}{\\small\\faIcon{chalkboard} provides a link to slides, where available}") %>%
    add_heading(make_heading("Talks", "section")) %>%
    writeLines(con = "tex-deps/talks.tex")
}

# --- Process Software ---------------------------------------------------------
if (nrow(sw_data) > 0) {
  sw_data %>%
    mutate(deprecated = is.na(date_end)) %>%
    arrange(desc(date_start)) %>%
    make_generic(datenames = c("date_start", "date_end"),
                 fieldnames = c("package", "description", "link"),
                 get_year = F) %>%
    add_entry_top("\\cvitem{}{\\footnotesize Dates show initial involvement; only packages which are no longer maintained have end dates.}") %>%
    add_heading(make_heading("Software", "subsection")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "tex-deps/software.tex")
}
# --- Process Teaching ---------------------------------------------------------
teach_data %>%
  mutate(semester = factor(semester, levels = c("Spring", "Fall"), ordered = T),
         label = paste(semester, year)) %>%
  mutate(course = paste(course_prefix, course_number)) %>%
  mutate(note = ifelse(!is.na(eval_mean),
                       paste(note, sprintf("Evals: %.2f (mean), %.0f (median)", eval_mean, eval_median), sep = ". "),
                       note)) %>%
  arrange(desc(year), semester, course_number) %>%
  make_generic(timeline = T, datenames = c("year"),
               fieldnames = c("course", "course_title", "location", "note")) %>%
  add_heading(make_heading("Teaching", "section")) %>%
  add_spacing("\\medskip") %>%
  writeLines(con = "tex-deps/teaching.tex")


# --- Process Mentoring --------------------------------------------------------
if(nrow(mentor_data) > 0) {
  mentor_data %>%
    mutate(degree = factor(degree, levels = c("Ph.D.", "MS", "Undergraduate", "Summer"))) %>%
    mutate(graduated = year_end > 0, show_span = year_start != year_end) %>%
    arrange(degree, graduated, desc(year_start), desc(year_end)) %>%
    nest(data = -degree) %>%
    mutate(
      header = make_heading(degree),
      texlines = .$data %>% 
        map(
          ~make_generic(., datenames = c("year_start", "year_end"),
                        fieldnames = c("name", "description", "note", "school"))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(degree) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Mentoring", "section")) %>%
    writeLines(con = "tex-deps/mentoring.tex")
}
# --- Process Reviewing --------------------------------------------------------
if(nrow(reviewing_data) > 0) {
  rev_txt <- reviewing_data %>%
    group_by(journal) %>%
    summarize(year = max(year)) %>%
    arrange(desc(year)) %>%
    magrittr::extract2("journal") %>%
    paste0(collapse = ", ") %>%
    paste("I have provided peer reviews for ", .) %>%
    make_cvitem("Reviewing", .)
}

# --- Process Service ----------------------------------------------------------
if(nrow(service_data) > 0) {
  
  service_data %>%
    mutate(Type = factor(str_to_title(Type), 
                         levels = unique(str_to_title(service_order), Type))) %>%
    arrange(Type, desc(year_start), desc(year_end)) %>%
    nest(data = -Type) %>%
    mutate(
      header = make_heading(Type),
      texlines = 
        map(data, 
          ~make_generic(., datenames = c("year_start", "year_end"),
                        fieldnames = c("position", "organization1", "organization2", "description"))),
      texlines = map2(texlines, header, add_heading)
    ) %>%
    tidyr::unnest(Type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Service", "section")) %>%
    add_entry_bottom(rev_txt) %>%
    writeLines(con = "tex-deps/service.tex")
}

# --- Process ProfDev ----------------------------------------------------------
if(nrow(profdev_data) > 0) {
  profdev_data %>%
    arrange(desc(start_date), desc(end_date)) %>%
    make_generic(datenames = c("start_date", "end_date"), 
                 fieldnames = c("description", "ex1", "ex2", "ex3")) %>%
    add_heading(make_heading("Professional Development", "section")) %>%
    add_spacing("\\medskip") %>%
    writeLines(con = "tex-deps/profdev.tex")
}


# --- Process Outreach ---------------------------------------------------------
if(nrow(outreach_data) > 0) {
  outreach_data %>%
    # mutate(start_date = ymd(start_date), end_date = ymd(end_date)) %>%
    arrange(type, desc(start_date), desc(end_date)) %>%
    nest(data = -type) %>%
    mutate(
      header = make_heading(type),
      texlines = 
        map(data, 
            ~make_generic(., datenames = c("start_date", "end_date"), 
                          get_year = F,
                          fieldnames = c("description", "location", "ex1", "ex2", "ex3"))) %>%
        map2(., header, add_heading)
    ) %>%
    tidyr::unnest(type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Outreach", "section")) %>%
    writeLines(con = "tex-deps/outreach.tex")
}


# --- Process Workshops --------------------------------------------------------
if(nrow(workshop_data) > 0) {
  workshop_data %>%
    mutate(Date = ymd(Date)) %>%
    arrange(Type, desc(Date)) %>%
    nest(data = -Type) %>%
    mutate(
      header = make_heading(Type),
      texlines = 
        map(data, 
            ~make_generic(., datenames = c("Date"), 
                          fieldnames = c("Title", "length", "Event", "Location", "Notes"))) %>%
        map2(., header, add_heading)
    ) %>%
    tidyr::unnest(Type) %>%
    magrittr::extract2("texlines") %>%
    unlist() %>%
    add_heading(make_heading("Workshops", "subsection")) %>%
    writeLines(con = "tex-deps/workshops.tex")
}


