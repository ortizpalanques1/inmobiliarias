# Functions
# Scraper for text ####
scraper <- function(list, numerator, elementHTML){
  x <- html_nodes(list[[numerator]],elementHTML ) %>% 
    html_text()
  return(x)
}

# Replacement when there is no text ####
no_data <- function(nt){
  x <- ifelse(length(nt) == 0, "No Data", nt)
}

# First letter is Capital ####
title_form <- function(s){
  disolve <- strsplit(s, " ")
  capitalize <- unlist(lapply(disolve, function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))))
  rejoin <- paste(capitalize, collapse = " ")
  return(rejoin)
}
