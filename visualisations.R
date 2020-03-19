


cases.timelime <- function(data, .trans = "log10") {
  .palette = "Tableau 20"
  if (data %>% pull(name) %>% unique() %>% length() < 10) {
    .palette = "Tableau 10"
  }
  plot <- data %>%
    filter(cases > 0 & diff != 1) %>%
    ggplot(aes(x = day, y = cases, color = name)) +
    geom_line(aes(linetype = type), size = 2) +
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
         subtitle = "Cases on logarithmic scale on a certain day of outbreak. Days are centered on the first past after 100 cases.",
         caption = "Source: Jens Laufer (https://jenslaufer.com) Data: BNO News") +
    
    bbc_style()
  
  plot
  
}

german <- function(plot) {
  plot +
    labs(title = "Bestätigte COVID-19 Fälle", subtitle = "Fallzahlen auf logarithmischer Skala an einem bestimmten Tag nach Erreichen von 100 Fällen")
}