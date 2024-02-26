
# Stolen from https://stackoverflow.com/questions/36338629/escaping-special-latex-characters-in-r
escapeLatexSpecials <- function(x) {
  # x <- gsub("\\", "$\\backslash$", x, fixed = T)
  x[!is.na(x)] <- gsub("#", "\\\\#", x[!is.na(x)])
  x[!is.na(x)] <- gsub("$", "\\$", x[!is.na(x)], fixed = T)
  x[!is.na(x)] <- gsub("%", "\\\\%", x[!is.na(x)])
  x[!is.na(x)] <- gsub("&", "\\\\&", x[!is.na(x)])
  x[!is.na(x)] <- gsub("~", "\\\\~", x[!is.na(x)])
  x[!is.na(x)] <- gsub("_", "\\\\_", x[!is.na(x)])
  # x <- gsub("^", "\\\\^", x)
  # x <- gsub("\\{", "\\\\{", x)
  # x <- gsub("\\}", "\\\\}", x)
  # x <- gsub(">", "$>$", x)
  # x <- gsub("<", "$<$", x)
  return(x)
}

determine_datenames <- function(data, datenames) {
  data %>% select(any_of(datenames)) %>%
    mutate(row = row_number()) %>%
    nest(data = -row) %>%
    mutate(
      nvals = map_int(data, ~length(unique(unlist(.)))),
      na_present = map_lgl(data, ~sum(is.na(unlist(.))) > 0),
      datenames = if_else(nvals == 1, list("start"), list(c("start", "end")))
    ) %>%
    select(datenames)
}

add_heading <- function(entries, heading) {
  c(heading, entries)
}

add_spacing <- function(x, spacing = "\\medskip") {
  c(x, spacing, "")
}

add_entry_top <- function(entries, new_entry) {
  c(new_entry, entries)
}

add_entry_bottom <- function(entries, new_entry) {
  c(entries, new_entry)
}


make_heading <- function(heading_name, heading_type = "subsection") {
  glue("\\{heading_type} {{{heading_name}}}")
}

collapse_labels <- function(labelname, ...) {
  paste0("{{ {", labelname, "} }}")
}

make_cvitem <- function(x, y) {
  x <- escapeLatexSpecials(x)
  y <- escapeLatexSpecials(y)
  paste0("\\cvitem{",x,"}{", y,"}")
}

#' Clean NA fields from latex code
#' 
#' @param x string to clean
#' @return a vector the same length as x
#' @importFrom stringr str_replace_all
clean_na <- function(x, rep = "{}") {
  str_replace_all(x, "\\{ ?NA ?\\}", rep)
}


#' Make a generic set of CV entries
#' 
#' @param data data frame of information
#' @param timeline whether `moderncvtimeline` is to be used or not
#' @param datenames vector of up to two date names, in order
#' @param fieldnames vector of up to five field names, in order
#' @param ... arguments to be passed on to make_XXcventry
#' @return vector of latex commands, one for each row of data
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate groupby `%>%`
#' @importFrom glue glue_collapse glue_data
make_generic <- function(data, 
                         timeline = T, 
                         datenames = paste0("date", 1:2),
                         labelname = NULL,
                         fieldnames = paste0("field", 1:5), ...){
  
  # data <- data %>%
  #   mutate(across(any_of(datenames), ~if_else(is.na(.), 0, .))) 
  
  date_res <- handle_dates(dates = data[,datenames], 
                           label = data[,labelname], 
                           timeline = timeline, ...) 
  
  # Handle variable numbers of included fields
  extra <- ""
  if (length(fieldnames) < 5) {
    extra <- rep("{}", length = 5 - length(fieldnames)) %>% paste(collapse = "")
  } else if (length(fieldnames) > 5) {
    warning("Only first 5 fields will be used")
    fieldnames <- fieldnames[1:5]
  }
  
  # Escape latex characters in fields
  data <- data %>%
    mutate(across(all_of(fieldnames), escapeLatexSpecials))
  
  # Create fields from data + template
  data$fields <- paste0("{{{", fieldnames, "}}}") %>%
    glue_collapse() %>%
    glue_data(data, .) %>%
    paste0(., extra)
  
  # Paste date template and field template together, clean out NAs
  date_res$date_filled %>%
    clean_na(rep = "{0}") %>%
    paste0(., data$fields) %>%
    clean_na()
}

#' Handle date formatting more intelligently
#' 
#' @param dates one or two columns of numeric or POSIX data
#' @param label optional column of labels to use for dates
#' @param timeline default TRUE, whether to use timeline
#' @param get_year default FALSE, whether only the year should be displayed.
#'                 This will result in dates being truncated to the year, 
#'                 instead of the default behavior of using the label field 
#'                 and passing month information into the label.
handle_dates <- function(dates, labelcol, timeline = T, get_year = F) {
  stopifnot(is.logical(timeline))
  stopifnot(is.null(get_year) | is.logical(get_year))
  dates <- check_date_format(dates)
  
  
  # First, handle different data types of dates. 
  # End goal is num_dates for timeline calculations 
  # and the ability to create labels if specified.
  if (!all(sapply(dates, is.numeric))) {
    num_dates <- dates %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), 
                                  as.Date)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), 
                                  lubridate::decimal_date)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  \(x) if_else(x == 1970, 0, x)))
  } else {
    num_dates <- dates
  }
  
  labeldim <- prod(dim(labelcol))
  if (labeldim == 0) {
    labelcol <- rep("", nrow(num_dates))
  }
  
  if(!all(names(num_dates) %in% c("start", "end"))) {
    names(num_dates) <- c("start", "end")[1:ncol(num_dates)]
  }
  
  res <- dplyr::bind_cols(num_dates, label = labelcol, 
                          timeline = timeline, gy = get_year) %>%
    dplyr::mutate(entry = (dplyr::row_number())) %>%
    tidyr::pivot_longer(c(start, end), 
                        names_to = "datetype", 
                        values_to = "date") %>%
    # If get_year, then floor the decimal date
    dplyr::mutate(date = if_else(gy, floor(date), date)) %>%
    tidyr::nest(df = any_of(c("datetype", "date"))) %>%
    dplyr::group_by(entry) %>%
    dplyr::mutate(use_label = purrr::map_lgl(df, determine_labels),
                  use_label = use_label | nchar(label) > 0,
                  use_span = purrr::map_lgl(df, determine_span),
                  label_text = purrr::map2_chr(df, timeline, make_labels),
                  label = ifelse(nchar(label) == 0, label_text, label))  %>%
    tidyr::unnest(df) %>%
    dplyr::mutate(
      datechr = ifelse(date %% 1 == 0, as.character(date), fix_dd(date))
    ) %>%
    dplyr::select(-date) %>%
    tidyr::pivot_wider(names_from = "datetype", values_from = "datechr") %>%
    tidyr::nest(df = -entry) %>%
    dplyr::mutate(cmd = purrr::map(df, eval_date_command)) %>%
    tidyr::unnest(cmd) %>%
    dplyr::ungroup() %>%
    dplyr::select(-entry)
  
  return(res)
}

eval_date_command <- function(df) {
  df <- df %>%
    mutate(date_template = paste0(
      "\\",
      if_else(df$timeline, "tl", ""),
      if_else(df$use_span, "", "date"),
      if_else(df$use_label, "label", ""),
      "cventry",
      if_else(df$use_span, "{{ {start} }}{{ {end} }}", "{{ {start} }}"),
      if_else(df$use_label, "{{ {label} }}", ""),
      if_else(df$timeline, "", "{{ {label} }}")
    ))
  
  return(data.frame(date_template = df$date_template, 
                    date_filled = glue::glue_data(df, df$date_template)))
}

fix_dd <- function(x) {
  lubridate::date_decimal(x) %>%
    format.Date("%y/%m") %>%
    stringr::str_replace("/0", "/")
}

determine_labels <- function(num_datevec) {
  if (is.data.frame(num_datevec)) num_datevec <- num_datevec$date
  
  # Labels are really only necessary if the dates are not integers
  non_ints <- sum((na.omit(num_datevec) %% 1) != 0)
  return(non_ints > 0)
}

determine_span <- function(num_datevec) {
  if (is.data.frame(num_datevec)) num_datevec <- num_datevec$date
  
  length(unique(num_datevec)) > 1
}

make_labels <- function(df, timeline = T) {
  
  df <- df %>%
    select(date) %>%
    unique() %>%
    mutate(str = lubridate::date_decimal(date) %>% 
             format.Date(., "%b %Y"),
           int = floor(date),
           use_int = date%%1 == 0,
           use_str = if_else(use_int, as.character(int), str)
    ) %>%
    summarize(label = paste(use_str, collapse = ifelse(timeline, "///", " -- "))) %>%
    mutate(label = stringr::str_remove(label, " ?[/-]{1,} ?0"))
  
  df$label
}

check_date_format <- function(dates) {
  stopifnot(all(sapply(dates, mode) %in% c("numeric", "POSIXct", "POSIXlt")))
  if (length(dim(dates)) == 2) {
    if (ncol(dates) > 2) {
      warning("Extra date information provided; 
              only the first two columns of the 
              dates object will be used.")
      dates <- dates[,1:2]
    } else if (ncol(dates) == 1) {
      fixed_dates <- data.frame(start = dates[,1], end = dates[,1])
    } else {
      fixed_dates <- data.frame(start = dates[,1], end = dates[,2])
    }
  } else {
    if (length(dates) > 2) {
      warning("Extra date information provided; 
               dates will be made into a 2-column data frame.")
    }
    
    fixed_dates <- as.matrix(dates, ncol = 2) %>% 
      as.data.frame() %>%
      magrittr::set_names(c("start", "end"))
  }
  
  if (sum(is.na(fixed_dates)) > 0) {
    warning("Dates which are NA will be changed to 0, 
            which is interpreted as 'current' by moderncvtimeline")
  }
  fixed_dates
}
