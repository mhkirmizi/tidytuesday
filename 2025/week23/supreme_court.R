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
subtitle <- "The Voronoi chart captures every justice in the Court’s history. Tile brightness reveals their age at appointment—\nbrighter tiles mark younger justices—while tile size represents how long they served on the bench."
### Data Wrangling
judges_appointments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv')
judges_people <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_people.csv')
diagnose(judges_people)
diagnose(judges_appointments)
df <- judges_appointments |> 
  left_join(judges_people, by = "judge_id")

df <- df |> 
  filter(court_type == "USSC") |> 
  select(name_first, name_last, commission_date, 
         termination_date, birth_date) |> 
  mutate(
    termination_date = if_else(name_last == 'Stevens', 
                                    '06/30/2010', termination_date)) |> 
  mutate(termination_date = if_else(is.na(termination_date), 
                                    '07/28/2025', 
                                    termination_date)) 

df <- df |> 
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
  geom_text(aes(label = name_last), size = 3.5, fontface = "bold", 
            colour = "white") +
  scale_fill_gradient(low = "#7DA7A6", high = "#053B3B") +
  labs(x = "", 
       y = "", 
       title = title,
       subtitle = subtitle,
       caption = "Data: {historydata} | Vis: MhKirmizi",
       fill = "Age")+
  theme_void(base_family ="EB Garamond", base_size = 12 )+
  theme(
    legend.position = "none", 
    plot.background = element_rect(fill = "black", colour = "black"), 
    panel.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(size = 36, face = "bold", hjust = .5, color = "white"),
    plot.subtitle = element_text(size = 20, hjust = .5, color = "white"),
    plot.caption = element_text(size = 14, color = 'white')
    )
ggsave("supreme_court.png", width = 1920, height = 1080, units = "px", dpi = 132)





