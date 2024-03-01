
# Stolen from https://stackoverflow.com/questions/36338629/escaping-special-latex-characters-in-r
escapeLatexSpecials <- function(x) {
  x[!is.na(x)] <- gsub("#", "\\\\#", x[!is.na(x)])
  x[!is.na(x)] <- gsub("$", "\\$", x[!is.na(x)], fixed = T)
  x[!is.na(x)] <- gsub("%", "\\\\%", x[!is.na(x)])
  x[!is.na(x)] <- gsub("&", "\\\\&", x[!is.na(x)])
  x[!is.na(x)] <- gsub("~", "\\\\~", x[!is.na(x)])
  x[!is.na(x)] <- gsub("_", "\\\\_", x[!is.na(x)])
  return(x)
}
