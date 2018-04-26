# Transfermarkt scraper
# League results (Bundesliga)

# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest)

# Use rvest to scrape the html-table from transfermarkt.de
## ---------------------------------------------------------------------- ##

# Create a vector of URLs to scrape along
pages <- vector(mode = "character", length = 13)
years <- seq(2005, 2017, 1)

for (i in seq_along(years)) {
  pages[[i]] <- paste0("https://www.transfermarkt.de/1-bundesliga/tabelle/wettbewerb/L1?saison_id=", years[i])
}

# Wrap the approach for a single URL into a function
transfer_scraper <- function(x) {
  table <- read_html(x) %>%
    html_nodes(css = ".responsive-table > table:nth-child(1)") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    .[[1]]
}

# Apply the function using purrr::map
league <- map(.x = pages, .f = ~transfer_scraper(.))

# Data cleaning
## ---------------------------------------------------------------------- ##

clean_fun <- function(x) {
  # (1) Remove unused columns/rows
  table <- x[,-2]

  # (2) Rename columns
  table <- setNames(table, c("place", "team", "matches.no", "win", "draw", "lose", "goals", "goaldiff", "points"))
}

# Apply the function using purrr::map
league.clean <- map(league, ~clean_fun(.))
