# Transfermarkt scraper
# Market values of teams

# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest)

# Use rvest to scrape the html-table from transfermarkt.de
## ---------------------------------------------------------------------- ##

# Create a vector of URLs to scrape along
pages <- vector(mode = "character", length = 13)
years <- seq(2005, 2017, 1)

for (i in seq_along(years)) {
  pages[[i]] <- paste0("https://www.transfermarkt.de/1-bundesliga/startseite/wettbewerb/L1/plus/?saison_id=", years[i])
}

# Wrap the approach for a single URL into a function
transfer_scraper <- function(x) {
  table <- read_html(x) %>%
    html_nodes(css = "#yw1 > table:nth-child(2)") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    .[[1]]
}

# Apply the function using purrr::map
multi <- map(.x = pages, .f = ~transfer_scraper(.))

# Data cleaning
## ---------------------------------------------------------------------- ##

# Function that transforms character strings into numeric market values:
# Function transforms the format "XX,XX Mio. €" into numerical values
mil_to_num <- function(x) {
  digits <- stringr::str_extract_all(x, "[:digit:]") 
  map_dbl(digits, ~strtoi(paste(str_c(., collapse = ""), "0000", sep = "")))
}

clean_fun <- function(x) {
  # (1) Remove unused columns/rows
  table <- x[,c(-1, -9:-11)]
  table <- table[-1,]
  
  # (2) Rename columns
  colnames(table) <- c("team", "team.abb", "squad.size", "age.mean", "foreign.no", "mv.tot", "mv.mean")
  
  # (b) Apply the function
  table <- table %>%
    mutate_at(vars(mv.tot, mv.mean), mil_to_num) %>%
    mutate(foreign.no = strtoi(foreign.no)) %>%
    mutate(age.mean = as.numeric(stringr::str_replace_all(age.mean, ",", ".")))
}

# Apply the function using purrr::map
multi.clean <- map(multi, ~clean_fun(.))
