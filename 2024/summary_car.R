library(tidyverse)
library(ggtext)
A64_traffic <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')

data <- A64_traffic |> 
  drop_na() |> 
  group_by(SiteId, `Time Period Ending`) |> 
  summarise(avg_vehicle = mean(`Total Volume`),
            avg_speed = mean(`Avg mph`),
            .groups = "drop") |> 
  mutate(
    `Time Period Ending` = hms::as_hms(`Time Period Ending`), # Ensure it's time format
    time_numeric = as.numeric(`Time Period Ending`)           # Convert to numeric for polar plotting
  )


midnight_data <- data.frame(
  `Time Period Ending` = rep(hms::hms(seconds = 0), 4),  # Midnight (00:00)
  time_numeric = 0,  # Midnight in seconds
  avg_speed = c(35.28571,46.00000, 47.48000, 53.66667),  # Example average speeds for four sites
  SiteId = c(6867, 7035, 7042, 7058)  # Site IDs for four sites
)

bind_rows(midnight_data, data) -> data

p <- ggplot(data, aes(x = time_numeric, 
                 y = avg_speed, 
                 colour = as.factor(SiteId), 
                 group = SiteId)) +
  geom_path(size = 1.2) +
  annotate("label", x = 0.5, y = seq(30, 50, 10), 
           label = seq(30, 50, 10), size = 3, ,
           color = "grey40", label.padding = unit(0.1, "lines"), label.size = 0) +
  scale_x_continuous(
    breaks = c(0, seq(7200, 86400, by = 14400)),  # Include 0 for midnight (00:00) and 2-hour intervals
    labels = function(x) {
      sapply(x, function(t) {
        if (t == 0) "" else sprintf("%02d:%02d", t %/% 3600, (t %% 3600) %/% 60)  # Skip midnight label
      })
    },
    expand = c(0, 0)  # No padding around the x-axis
  ) +
  coord_polar(theta = "x") +  
  labs(x = "The time of day", 
       y = "Average Speed (mph)", 
       colour = "Site ID", 
       title = "Average Speed Over Time in Four Different Sites", 
       subtitle = "The changes in the average speed reveals an intersting pattern", 
       caption = "Source: WebTRIS Traffic Flow API . Created by MhKirmizi") +
  theme_minimal() +
  theme(
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 25, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_markdown(hjust = 0.5, size = 14, lineheight = 1.4), 
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0)),
    legend.position = "bottom", 
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank(),
  ) 
  
ggsave("summary_car.pdf", plot = p, width = 12, height = 8)

