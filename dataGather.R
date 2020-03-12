library(tidyverse)
library(reticulate)
library(lubridate)


load.data.bno.news <- function() {
  read_csv(
    "https://raw.githubusercontent.com/alext234/coronavirus-stats/master/data/bnonews-international.csv"
  ) %>%
    gather(-datetime, key = "country", value = "cases")
}

load.data.jhu <- function() {
  reticulate::py_run_file("src/import.py")
}


get.confirmed.jhu <- function() {
  confirmed <- read_csv("data/Confirmed.csv")
  
  confirmed %>% gather(
    -`Province/State`,
    -`Country/Region`,
    -Lat,
    -Long,
    key = "date",
    value = "confirmed"
  ) %>%
    mutate(
      confirmed = replace_na(confirmed, 0),
      `Province/State` = as.factor(`Province/State`),
      `Country/Region` = as.factor(`Country/Region`),
      date = as.Date(date, format = "%m/%d/%Y")
    ) %>%
    group_by(`Province/State`, `Country/Region`, date) %>%
    top_n(1) %>%
    arrange(`Country/Region`, `Province/State`, date) %>%
    group_by(`Country/Region`, `Province/State`) %>%
    mutate(confirmed_chg_pct = confirmed / dplyr::lag(confirmed)) %>%
    ungroup() %>%
    mutate(date = date %m+% years(2000))
}





get.deaths.jhu <- function() {
  death <- read_csv("data/Deaths.csv")
  
  death %>% gather(
    -`Province/State`,
    -`Country/Region`,
    -Lat,
    -Long,
    key = "date",
    value = "death"
  ) %>%
    mutate(
      death = replace_na(death, 0),
      `Province/State` = as.factor(`Province/State`),
      `Country/Region` = as.factor(`Country/Region`),
      date = as.Date(date, format = "%m/%d/%Y")
    ) %>%
    group_by(`Province/State`, `Country/Region`, date) %>%
    top_n(1) %>%
    arrange(`Country/Region`, `Province/State`, date) %>%
    group_by(`Country/Region`, `Province/State`) %>%
    mutate(death_chg_pct = death / dplyr::lag(death)) %>%
    ungroup()%>%
    mutate(date = date %m+% years(2000))
}


get.recovered.jhu <- function() {
  recovered <-  read_csv("data/Recovered.csv")
  
  recovered %>% gather(
    -`Province/State`,
    -`Country/Region`,
    -Lat,
    -Long,
    key = "date",
    value = "recovered"
  ) %>%
    mutate(
      recovered = replace_na(recovered, 0),
      `Province/State` = as.factor(`Province/State`),
      `Country/Region` = as.factor(`Country/Region`),
      date = as.Date(date, format = "%m/%d/%Y")
    ) %>%
    group_by(`Province/State`, `Country/Region`, date) %>%
    top_n(1) %>%
    arrange(`Country/Region`, `Country/Region`,  date) %>%
    group_by(`Country/Region`, `Province/State`) %>%
    mutate(recovered_chg_pct = recovered / dplyr::lag(recovered)) %>%
    ungroup() %>%
    mutate(date = date %m+% years(2000))
}

get.data.jhu <- function() {
  load.data.jhu()
  data <- get.confirmed.jhu() %>%
    left_join(get.deaths.jhu() %>% select(-Lat, -Long)) %>%
    left_join(get.recovered.jhu() %>% select(-Lat, -Long)) 
  data %>%
    select(-recovered_chg_pct, -death_chg_pct, -confirmed_chg_pct) %>%
    write_csv("data/COVID19.csv")
  
  data
}
