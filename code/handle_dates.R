#' Handle date formatting more intelligently
#' 
#' @param dates one or two columns of numeric or POSIX data
#' @param label optional column of labels to use for dates
#' @param timeline default TRUE, whether to use timeline
#' @param span default TRUE, whether to use span (which requires two dates)
#' @param get_year default FALSE, whether only the year should be displayed.
#'                 This will result in dates being truncated to the year, instead
#'                 of the default behavior of using the label field and passing
#'                 month information into the label instead.
handle_dates <- function(dates, label, timeline = T, span = T, get_year = F) {
  stopifnot(is.logical(timeline))
  stopifnot(is.logical(span))
  stopifnot(is.logical(get_year))
  
  latexcall <- "\\"
  datefields <- ""
  use_label <- FALSE
  
  # Determine if label will be necessary
  if (!is.null(label)) {
    use_label <- T
  }
  if (is.POSIXct(dates)) {
    use_label <- T
  }
  
  if (!span) {
    latexcall <- paste0(latexcall, "date")
    datefields <- paste0(datefields, "{ date1 }")
  } else {
    datefields <- paste0(datefields, "{ date1 }{ date2 }")
  }
  if (timeline) {
    latexcall <- paste0(latexcall, "tl")
  }
  
}


examine_dates <- function(dates) {
  stopifnot(mode(dates) %in% c("numeric", "POSIXct", "POSIXlt"))
  if (dim(dates) == 2) {
    if (ncol(dates) > 2) {
      warning("Extra date information provided; only the first two columns of the dates object will be used.")
      dates <- dates[,1:2]
    }
    fixed_dates <- data.frame(start = dates[,1], end = dates[,2])
  } else {
    if (length(dates) > 2) warning("Extra date information provided; dates will be made into a 2-column data frame.")
    fixed_dates <- matrix(dates, ncol = 2, rowwise = T) %>% 
      as.data.frame() %>%
      magrittr::set_names(c("start", "end"))
    }
  
  
  if (sum(is.na(fixed_dates)) > 0) {
    warning("Dates which are NA will be changed to 0, which is interpreted as 'current' by moderncv")
  }
}
