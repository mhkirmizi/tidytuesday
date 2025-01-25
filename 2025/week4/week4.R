library(tidyverse)
library(ggalt)
library(showtext)

## Resolution
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

## Fonts
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
font_add_google("Shadows Into Light", regular.wt = 400, family = "anotation")
showtext_auto(enable = TRUE)

## Colors
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "#3d3d3d"           
subtitle_col <- "#3d3d3d"     
caption_col  <- "gray30" 

## Data
water_insecurity_2023 <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')
water_insecurity_2022 <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')

water2022 <- water_insecurity_2022 |> 
  separate(name, into = c("county", "state"), sep = ",", remove = FALSE) |> 
  filter(state == " Texas") |> 
  select(c(geoid, total_pop, percent_lacking_plumbing, plumbing))

water2023 <- water_insecurity_2023 |> 
  separate(name, into = c("county", "state"), sep = ",", remove = FALSE) |> 
  filter(state == " Texas") |> 
  select(-state)
water2023 <- water2023 |> 
  rename_with(~paste0(.,"_2023")) |> 
  rename(c("geoid" = "geoid_2023", "county" = "county_2023")) |> 
  select(-c("name_2023", "geometry_2023", "year_2023"))
water2022 <-water2022 |> 
  rename_with(~paste0(.,"_2022")) |> 
  rename("geoid" = "geoid_2022")
texas_water <- water2023 |> 
  left_join(water2022, by = c("geoid" = "geoid"))

### 
title = "Water Insecurities Across Texas"
subtitle =   "Access to water among Texans varies significantly depending on various factors. In some counties, over 40% of the population \nlacks access to proper plumbing, while in counties like Orange, more than 60% of plumbing infrastructure has been lost in just a year."
caption = "Data: USGS | Vis: MhKirmizi"


## Plotting
texas_water <-  texas_water |> 
  mutate(diff = percent_lacking_plumbing_2023 - percent_lacking_plumbing_2022, 
         sign_diff = sign(diff), 
         abs_diff = abs(diff)) |> 
  drop_na()  |> filter(abs_diff >= 5.388339e-02) 
texas_water$county <- gsub(" County$", "", texas_water$county)

texas_water$county <- trimws(texas_water$county, which = "right")
    
texas_water |>
  ggplot(aes(y = reorder(county, percent_lacking_plumbing_2022))) +
  geom_dumbbell(aes(x = percent_lacking_plumbing_2022, xend = percent_lacking_plumbing_2023),
                size = .75,
                color = "grey66") +
  geom_point(aes(x = percent_lacking_plumbing_2022, color = "2022"), size =2) +
  geom_point(aes(x= percent_lacking_plumbing_2023, color = "2023"), size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("2022" = "#adadff", "2023" = "#00008B")) +
  labs(y = "", 
       x = "Percent Lacking Plumbing", 
       color = "Year", 
       title = title, 
       subtitle = subtitle, 
       caption = caption) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 32, face= "bold", hjust = .5, family = "Oswald", color = "#3d3d3d"),
    plot.subtitle = element_text(size = 18, hjust =.5, family = "Merriweather Sans",  color = "#3d3d3d"),
    plot.caption = element_text(size = 12, family = "Noto Sans" , color = "gray30"),
    axis.title = element_text(size = 12, family = "text", color = colorspace::darken("#8e8a7b" , 0.2)),
    legend.text = element_text(size = 12, family = "text", color = colorspace::darken("#8e8a7b" , 0.2)),
    axis.text = element_text(size = 10, family = "text", color = colorspace::darken("#8e8a7b" , 0.2)),
    legend.position = "bottom", 
    axis.line.x =  element_line(color = "gray40", linewidth = .15),
    panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.5, color = 'gray'),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    plot.margin  = margin(t = 20, r = 25, b = 20, l = 25),
    plot.background   = element_rect(fill = colorspace::lighten('#f7f5e9', 0.05), 
                                     color = colorspace::lighten('#f7f5e9', 0.05)),
    panel.background  = element_rect(fill = colorspace::lighten('#f7f5e9', 0.05), 
                                     color = colorspace::lighten('#f7f5e9', 0.05))
  )
ggsave("week4.pdf", height = 12, width = 18)
