




cases.timelime <- function(data, .trans = "log10") {
  
  days7 <- tibble(x7=seq(1,40)) %>% mutate(y7=100*(2^(1/7))^x)
  days30 <- tibble(x=seq(1,40)) %>% mutate(y=100*(2^(1/20))^x)
  days3 <- tibble(x=seq(1,40)) %>% mutate(y=100*(2^(1/3))^x)
  
  .palette = "Tableau 20"
  if (data %>% pull(name) %>% unique() %>% length() < 10) {
    .palette = "Tableau 10"
  }
  plot <- data %>%
    filter(cases > 0 & diff != 1) %>%
    ggplot(aes(x = day, y = cases, color = name)) +
    geom_line(aes(linetype = type), size = 2) +
    geom_line(mapping = aes(x=x7, y=y7), data = days7) +
    geom_hline(yintercept = 100) +
    geom_vline(xintercept = 0) +
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