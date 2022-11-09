
# Stolen from https://stackoverflow.com/questions/36338629/escaping-special-latex-characters-in-r
escapeLatexSpecials <- function(x) {
  # x <- gsub("\\", "$\\backslash$", x, fixed = T)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("$", "\\$", x, fixed = T)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("~", "\\\\~", x)
  x <- gsub("_", "\\\\_", x)
  # x <- gsub("^", "\\\\^", x)
  # x <- gsub("\\{", "\\\\{", x)
  # x <- gsub("\\}", "\\\\}", x)
  # x <- gsub(">", "$>$", x)
  # x <- gsub("<", "$<$", x)
  return(x)
}


print_heading <- function(type, data, 
                          heading_type = "\\subsection{%s}", spacing = "\\medskip", 
                          fn = print_talks) {
  c(
    sprintf(heading_type, type),
    fn(data),
    spacing, 
    ""
  )
}
