library(tidyverse)
library(reticulate)
library(lubridate)

PYTHON.PATH <- "/home/jens/anaconda3/bin/python"


load.mobility.google <- function() {
  filename <- "data/google_mobitilty.csv"
  download.file(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
    filename
  )
  read_csv(filename)
}

load.hospital.beds <- function() {
  read_csv("data/hospital_beds.csv") %>%
    select(-`Series Name`,-`Series Code`,-`Country Code`) %>%
    gather(-`Country Name`, key = "year", value = "beds.per.1k.inhabitants") %>%
    mutate(beds.per.1M.inhabitants = as.numeric(beds.per.1k.inhabitants) *
             1000) %>%
    mutate(
      year = substring(year, 1, 4),
      year = as.numeric(year),
      year = as.factor(year)
    ) %>%
    rename(name = `Country Name`) %>%
    mutate(
      name = ifelse(name == "Korea, Rep.", "South Korea", name),
      name = ifelse(name == "Egypt, Arab Rep.", "Egypt", name),
      name = ifelse(name == "Iran, Islamic Rep.", "Iran", name),
      name = ifelse(name == "Russian Federation", "Russia", name)
    ) %>%
    group_by(name) %>%
    na.omit() %>%
    slice(n()) %>%
    ungroup() %>%
    select(-beds.per.1k.inhabitants)
}

load.populations <- function() {
  read_csv("data/population.csv") %>%
    mutate(Value = Value / 1000000) %>%
    rename(name = `Country Name`, population.mio = Value) %>%
    group_by(name) %>%
    arrange(Year) %>%
    slice(n()) %>%
    ungroup() %>%
    select(name, population.mio)
}

load.data.bno.news <- function() {
  read_csv(
    "https://raw.githubusercontent.com/alext234/coronavirus-stats/master/data/bnonews-international.csv"
  ) %>%
    gather(-datetime, key = "country", value = "cases") %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(country, date) %>%
    arrange(desc(country, date)) %>%
    slice(n()) %>%
    rename(name = `country`) %>%
    ungroup() %>%
    mutate(type = "historical") %>%
    filter(date < today())
}

load.data.jhu <- function() {
  reticulate::py_install(c("requests", "pandas"))
  reticulate::py_run_file("src/import.py")
}


get.confirmed.jhu <- function() {
  confirmed <- read_csv("data/Confirmed.csv")
  
  confirmed %>% gather(
    -`Province/State`,-`Country/Region`,-Lat,-Long,
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
    -`Province/State`,-`Country/Region`,-Lat,-Long,
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
    ungroup() %>%
    mutate(date = date %m+% years(2000))
}


get.recovered.jhu <- function() {
  recovered <-  read_csv("data/Recovered.csv")
  
  recovered %>% gather(
    -`Province/State`,-`Country/Region`,-Lat,-Long,
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
    left_join(get.deaths.jhu() %>% select(-Lat,-Long)) %>%
    left_join(get.recovered.jhu() %>% select(-Lat,-Long))
  data %>%
    select(-recovered_chg_pct,-death_chg_pct,-confirmed_chg_pct) %>%
    write_csv("data/COVID19.csv")
  
  data
}

load.german.county.cases <- function() {
  laender <-
    c(
      "Baden-Württemberg",
      "Bayern",
      "Berlin",
      "Brandenburg",
      "Bremen",
      "Hamburg",
      "Hessen",
      "Mecklenburg-Vorpommern",
      "Niedersachsen",
      "Nordrhein-Westfalen",
      "Rheinland-Pfalz",
      "Saarland",
      "Sachsen",
      "Sachsen-Anhalt",
      "Schleswig-Holstein",
      "Thüringen"
    )
  
  laender %>% map( ~ .get.cases.for.county(.)) %>%
    bind_rows() %>%
    mutate(name = as.factor(name), type = "historical") %>%
    rename(date = time) %>%
    select(date, name, cases, deaths, type)
}

.get.cases.for.county <- function(land) {
  read_tsv("data/Germany/DEU-{land}.tsv" %>% glue(), skip = 3) %>%
    mutate(name = as.character("{land}" %>% glue()))
}
