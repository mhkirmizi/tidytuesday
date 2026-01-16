library(tidyverse)
library(ggforce)
library(WeightedTreemaps)
library(showtext)
## Fonts
font_add_google('Solway', 'sol')
showtext_auto()
### Data 
africa <- tidytuesdayR::tt_load('2026-01-13')
df <- africa$africa

### Data Cleaning
## Recoding the family
df <- df |> 
  mutate(family = case_when(family == 'Afro-Asiatic' ~ 'Afroasiatic',
                            family == 'Arabic-based' ~ 'Afroasiatic',
                            family == 'Portuguese' ~ 'Indo-European', 
                            family == 'French' ~ 'Indo-European', 
                            family == 'English' ~ 'Indo-European',
                            family == 'Language' ~ NA,
                            TRUE ~ family)) 

df_languages <- df |> 
  select(family, language, country, native_speakers) |> 
  group_by(family, language) |> 
  summarise(n_country = n_distinct(country))

df_plot <- df_languages |> 
  left_join(df, by = c('family', 'language')) |> 
  select(-country) |> 
  group_by(family, language) |> 
  summarise(total = sum(native_speakers), 
            count = n(), 
            total_speaker  = total/count, 
            .groups = 'drop') |> 
  select(family, language, total_speaker)

 top_3_family <- df_plot |> 
  group_by(family) |> 
  count() |> 
  arrange(-n) |> 
  filter(n > 20) |> 
  pull(family)

top_100<- df_plot |> 
  filter(family %in% top_3_family) |> 
  arrange(-total_speaker) |> 
  top_n(100) 

### Calculating the area of each tile
result <- voronoiTreemap(
  data = top_100, 
  levels = c('family', 'language'), 
  cell_size = 'total_speaker', 
  shape = 'circle'
)
### 
polygons <- WeightedTreemaps::get_polygons(result)

poly_df <- imap_dfr(polygons, ~ {
  if (is.null(.x)) return(NULL)
  coords <- sf::st_coordinates(.x)
  data.frame(
    x = coords[,1],
    y = coords[,2],
    id = .y
  )
})

set.seed(1234)
groups <- poly_df |> 
  filter(str_starts(id, "LEVEL1")) |> 
  mutate(id = str_remove(id, "LEVEL1_"))
subgroups <- poly_df |> 
  filter(str_starts(id, "LEVEL2")) |> 
  mutate(id = str_remove(id,"LEVEL2_")) |> 
  group_by(id) |> 
  mutate(alpha = runif(1, .4, .7)) |> 
  ungroup()
### Langauage names
## getting top 30 most widely spoken languages
name_30_language <- top_100 |> 
  top_n(30) |> 
  pull(language)
subgroup_labels <- subgroups |> 
  filter(id %in% name_30_language) |> 
  group_by(id) |> 
  summarise(
    x = mean(x),
    y = mean(y),
    .groups = "drop"
  )

### Plotting time
ggplot() +
  geom_polygon(data = groups, aes(x =x, y= y, 
                                  group = id, 
                                  fill = id),
               color = 'white') +
  geom_polygon(data = subgroups, aes(x =x, y =y, 
                                     group = id, 
                                     alpha = alpha), 
               show.legend = FALSE,
               color = 'white', fill = 'white', linewidth = .3) +
  geom_text(data = subgroup_labels, 
            aes(x, y, label = id), size = 4.5) +
  coord_equal() +
  paletteer::scale_fill_paletteer_d("trekcolors::starfleet") +
  labs(title = 'Languages of Africa', 
       subtitle = "Africa’s linguistic diversity spans hundreds of languages.\n These are the most widely spoken.", 
       caption= 'Data: Wikipedia | Vis: MhKirmizi', 
      fill = NULL) +
  theme_void(base_size = 14, base_family = 'sol') +
  theme(
    text = element_text(color = "#82889B"), 
    legend.position = 'bottom',
    plot.title = element_text(size = 28, hjust = .5, face = 'bold'), 
    plot.subtitle = element_text(size = 21, hjust = .5),
    plot.caption = element_text(hjust = .5), 
    plot.background = element_rect(fill = "#1F2133", color = "#1F2133"), 
    panel.background = element_rect(fill = "#1F2133", color = "#1F2133")
  )
ggsave("week2.png", width = 1100, height = 1080, units = "px", dpi = 132)





