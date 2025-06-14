library(tidyverse)
library(dlookr)
library(lubridate)
library(packcircles)
library(showtext)
## Fonts
font_add_google("EB Garamond")
showtext_auto()
## Text
title <- "Your Supreme Justice"
subtitle <- "Serving on the United States Supreme Court is the pinnacle of judicial achievement. The Voronoi chart below visualizes every justice in the Court’s history. \nEach tile’s brightness reflects the age of the justice, while its size represents the length of their active service in years"
### Data Wrangling
judges_appointments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv')
judges_people <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_people.csv')
diagnose(judges_people)
diagnose(judges_appointments)
df <- judges_appointments |> 
  left_join(judges_people, by = "judge_id")


df <- df |> 
  filter(court_type == "USSC") |> 
  mutate(
    commission_date = as.Date(commission_date, format = "%m/%d/%Y"),
    termination_date = as.Date(termination_date, format = "%m/%d/%Y")
  ) |> 
  mutate(year_service_start = year(commission_date), 
         year_service_end = year(termination_date), 
         age = year_service_start - birth_date,
         active_service_year = year_service_end - year_service_start) 

data_circle <- circleProgressiveLayout(df$active_service_year*2, 
                                     sizetype = "area")
df2 <- cbind(df, data_circle)

### Plotting

ggplot(df2, aes(x= x, y =y)) +
  ggforce::geom_voronoi_tile(aes(fill = age), 
                             colour = "white", size = .5)+
  geom_text(aes(label = name_last), size = 4, fontface = "bold", colour = "white") +
  scale_fill_gradient(low = "#7DA7A6", high = "#053B3B") +
  labs(x = "", 
       y = "", 
       title = title,
       subtitle = subtitle,
       caption = "Data: {historydata} | Vis: MhKirmizi",
       fill = "Age")+
  theme_void(base_family ="EB Garamond", base_size = 12 )+
  theme(
    legend.position = "bottom", 
    plot.background = element_rect(fill = "black", colour = "black"), 
    panel.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(size = 36, face = "bold", hjust = .5, color = "white"),
    plot.subtitle = element_text(size = 20, hjust = .5, color = "white"),
    plot.caption = element_text(size = 14, color = 'white'),
    legend.title = element_text(color = "white", size = 14), 
    legend.text = element_text(color = "white", size = 14),
    )
ggsave("supreme_court.png", width = 1920, height = 1080, units = "px", dpi = 132)





