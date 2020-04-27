library(tsbox)
library(forecast)
library(tidyverse)
library(bbplot)
library(ggthemes)
library(ggrepel)
library(scales)
library(glue)
library(tsbox)
library(forecast)

.add.forecast <- function(data, .name, .h = 5) {
  data %>%
    filter(name == .name) %>%
    select(cases) %>%
    auto.arima() %>%
    forecast(h = .h) %>%
    as_tibble() %>%
    mutate(name = .name,
           day = row_number(),
           type = "forecast") %>%
    rename(cases = `Point Forecast`) %>%
    select(name, `cases`, type)
}


preprocess.data <- function(data) {
  data <- data %>%
    group_by(name) %>%
    filter(!(date == max(date) &
               cases / lag(cases) == 1)) %>%
    ungroup() %>%
    bind_rows(data %>% distinct(name)
              %>%
                pull(name) %>%
                map(~ .add.forecast(data, (.), 7))) %>%
    group_by(name) %>%
    mutate(day = row_number()) %>%
    ungroup()
  
  thresholds <- data %>%
    group_by(name) %>%
    filter(cases >= 100) %>%
    slice(1) %>%
    select(name, day) %>%
    rename(threshold = day) %>%
    ungroup()
  
  data <- data %>%
    inner_join(thresholds) %>%
    mutate(day = day - threshold) %>%
    select(-threshold) %>%
    ungroup()
  
  data <- data %>%
    group_by(name) %>%
    mutate(diff = cases / lag(cases)) %>%
    mutate(double.time = log10(2) / log10(diff)) %>%
    ungroup()
  
  data
}


preprocess.mobility.data <- function(data) {
  data %>%
    select(-sub_region_2) %>%
    mutate(
      timestamp = as.numeric(as.POSIXct(date)),
      overall_percent_change_from_baseline = (
        retail_and_recreation_percent_change_from_baseline + grocery_and_pharmacy_percent_change_from_baseline + parks_percent_change_from_baseline + transit_stations_percent_change_from_baseline + workplaces_percent_change_from_baseline + residential_percent_change_from_baseline
      ) / 6
    ) %>%
    mutate_if(str_detect(names(.), "change_from_baseline"),
              funs(diff = (.) / lag(abs(.))))
}

fit.loess.model <-
  function(data,
           .feature1,
           .feature2,
           .id = "country_region") {
    models <- data %>%
      filter(!is.na(!!sym(.feature1))) %>%
      mutate(num = n()) %>%
      filter(num > 1) %>%
      select(-num) %>%
      nest() %>%
      mutate(mdl = map(data,
                       ~ loess(
                         !!sym(.feature1) ~ !!sym(.feature2), data = .
                       )),
             fit = map(mdl, `[[`, "fitted")) %>%
      ungroup()
    
    list(
      "data" = models %>% select(-mdl) %>% unnest(),
      "models" = models %>% select(!!sym(.id), mdl)
    )
  }



lag.data <-
  function(cases, mobility, .lag, .feature, .group = "country_region") {
    cases %>%
      inner_join(mobility) %>%
      mutate(lag = as.integer(.lag)) %>%
      group_by(!!sym(.group)) %>%
      mutate(!!.feature := lag(!!sym(.feature), .lag)) %>%
      ungroup() %>%
      na.omit()
  }
