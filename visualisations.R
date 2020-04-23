cases.timelime <- function(data, .trans = "log10") {
  days4 <-
    tibble(day = seq(0, 40)) %>% mutate(y4 = 100 * (2 ^ (1 / 4)) ^
                                          day)
  days3 <-
    tibble(day = seq(0, 40)) %>% mutate(y3 = 100 * (2 ^ (1 / 3)) ^
                                          day)
  days7 <-
    tibble(day = seq(0, 40)) %>% mutate(y7 = 100 * (2 ^ (1 / 7)) ^
                                          day)
  days20 <-
    tibble(day = seq(0, 40)) %>% mutate(y20 = 100 * (2 ^ (1 / 20)) ^
                                          day)
  
  
  .palette = "Tableau 20"
  if (data %>% pull(name) %>% unique() %>% length() < 10) {
    .palette = "Tableau 10"
  }
  plot <- data %>%
    left_join(days3, by = "day") %>%
    left_join(days4, by = "day") %>%
    left_join(days7, by = "day") %>%
    left_join(days20, by = "day") %>%
    filter(cases > 0 & diff != 1) %>%
    ggplot(aes(x = day, y = cases, color = name)) +
    geom_line(aes(linetype = type), size = 2) +
    geom_hline(yintercept = 100) +
    geom_vline(xintercept = 0) +
    geom_line(aes(x = day, y = y3), color = "darkgrey") +
    geom_line(aes(x = day, y = y4), color = "darkgrey") +
    geom_line(aes(x = day, y = y7), color = "darkgrey") +
    geom_line(aes(x = day, y = y20), color = "darkgrey") +
    scale_color_tableau(palette = .palette) +
    scale_y_continuous(trans = .trans, label = comma) +
    scale_linetype_manual(
      values = c("solid", "dotted"),
      limits = c("historical", "forecast")
    ) +
    scale_x_continuous(breaks = seq(min(data$day), max(data$day), 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Confirmed COVID-19 Cases",
         subtitle = "Cases on a certain day of outbreak. Days are centered on the first past after 100 cases.",
         caption = "Source: Jens Laufer (https://jenslaufer.com) Data: BNO News") +
    
    bbc_style()
  
  plot
  
}

changes.plot <- function(data, .name) {
  data %>% filter(name == .name & type == "historical") %>%
    select(name, cases, day, diff) %>%
    ggplot(aes(x = day, y = diff)) +
    geom_bar(stat = "identity") +
    geom_smooth() +
    labs(title = "Growth Rate Confirmed COVID-19 cases", x = "Day of Outbreak", y =
           "Daily Growth Factor") +
    bbc_style()
}

german <- function(plot) {
  plot +
    labs(title = "Bestätigte COVID-19 Fälle", subtitle = "Fallzahlen an einem bestimmten Tag nach Erreichen von 100 Fällen")
}

plot.model.data <-
  function(data,
           .fit.feature = "fit",
           .feature1,
           .feature2,
           is.date = T) {
    plot <- data %>%
      ggplot(aes(
        x = !!sym(.feature1),
        y = !!sym(.fit.feature),
        color =
          country_region
      )) +
      geom_line(aes(y = !!sym(.feature2))) +
      geom_line(size = 2) +
      geom_hline(yintercept = 0) +
      scale_color_tableau()
    facet_wrap( ~ country_region, scales = "free") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (is.date) {
      plot <- plot + scale_x_date(date_breaks = "2 day")
    }
    plot
  }