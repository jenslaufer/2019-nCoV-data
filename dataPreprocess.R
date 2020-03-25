

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
    ungroup()
  
  data
}