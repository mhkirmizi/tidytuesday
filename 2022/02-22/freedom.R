library(tidyverse)
library(tidytuesdayR)
library(sf)
library(janitor)
library(rnaturalearth)


tt <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tt$freedom %>% 
  clean_names()
rm(tt)

## Colors
color =  c("Strong Gain" = "green", "Gain" = "blue", "No Change" = "grey66", "Loss" = "orange", "Strong Loss" = "red")

freedom |> 
  filter(year  %in% c(1996, 2020)) |> 
  select(country, year, status) |> 
  group_by(country) |> 
  summarise(count = n()) |> filter(count == 1) |> pull(country)



freedom2 <- freedom |> 
  filter(year  %in% c(1996, 2020)) |> 
  select(country, year, status) 
freedoom2$status <- as.factor(freedoom2$status) 
freedom2 <- freedom2 |> 
  mutate(
    status_numeric = case_when(
      status == "NF" ~ 1, 
      status == "PF" ~ 2, 
      status == "F" ~ 3
    )
  ) |> select(-status) |> 
  pivot_wider(names_from = year, values_from = status_numeric) |> 
  drop_na() |> 
  rename(
    year_1996 = `1996`,
    year_2020 = `2020`) |> 
  mutate(change_in_freedom = year_2020 - year_1996) |> 
  mutate(
    country = if_else(country =="CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire", "Ivory Cost", country)
  ) 
  
freedom2<- freedom2 |> 
  select(c(country, change_in_freedom))
## Vis

world <- ne_countries(scale = "medium", returnclass = "sf")
world2 <- world |> 
   left_join(freedom2, by = c("name_long" = "country")) 
world2 |> 
  mutate(change_in_freedom = ifelse(is.na(change_in_freedom), 0, change_in_freedom)) |> 
  mutate(change_in_freedom = case_when(
    change_in_freedom == -2 ~ "Strong Loss", 
    change_in_freedom == -1 ~ "Loss", 
    change_in_freedom == 0 ~ "No Change", 
    change_in_freedom == 1 ~ "Gain", 
    change_in_freedom == 2 ~ "Strong Gain"
  )) |> 
  ggplot() +
  geom_sf(aes(fill = as.factor(change_in_freedom))) +
  scale_fill_manual(values = color, name = "Change in Freedom", 
                    limits = c("Strong Loss", "Loss", "No Change", "Gain", "Strong Gain")) +
  labs(title = "Change in Freedom", 
       subtitle = "Difference in freedom status around the globe from 1996 to 2020 around globe reveals an interesting pattern \nsome countries like Russia and Turkey lost their freedom where as Brazil and some other countries gained more freeedom.",
       caption = "Data: Freedom House | Vis: MhKirmizi") +
  theme_minimal()+
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 42, face = "bold", hjust = .5), 
        plot.subtitle = element_text(size = 20, hjust = .5), 
        plot.caption = element_text(size = 10, hjust = 1))
ggsave("change_in_freedom.pdf", dpi = 360, width = 16.5, height = 11.7)

