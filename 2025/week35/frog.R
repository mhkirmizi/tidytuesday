library(tidyverse)
library(ozmaps)
library(ggpointdensity)
library(sf)
library(showtext)
### Fonts
font_add_google("Montserrat", "mont")
font_add_google('Inter')
showtext_auto()

### Data and Wrangling 
frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')
top_6_names<- frogID_data |> 
  group_by(scientificName) |> 
  count(scientificName, sort = TRUE) |> 
  arrange(-n) |> 
  head(n = 6) |> 
  pull(scientificName)

### 
top_6_spotted <- frogID_data |> 
  filter(scientificName %in% top_6_names) 
top_6_spotted <- top_6_spotted |> 
  left_join(
    frog_names, by =c('scientificName' = 'scientificName')
  )

top_6_spotted_plot <- top_6_spotted |> 
  select(decimalLatitude, decimalLongitude, commonName)

### Getting the maps of australia

sf_oz <- ozmap("states")



#### 

ggplot(data = sf_oz) + 
  geom_sf() +
  geom_pointdensity(data = top_6_spotted_plot, 
                  aes(decimalLongitude, decimalLatitude),
                  method = "kde2d",
                  size = 1,
                  adjust = 4)+
  scale_color_viridis_c() +
  facet_wrap(~commonName) +
  coord_sf(expand = FALSE, clip = 'off')+
  labs(
    title = 'Common Frogs and Where to Find Them', 
    caption = 'Data = {FrogID} | Vis: MhKirmizi'
  )+
  theme_void(base_size = 14, base_family = "Inter") +
  theme(
    plot.title = element_text(size = 32, hjust = .5, 
                              face = 'bold', family = 'mont', 
                              colour ="#2a475e", margin = margin(t = 20, b = 10)), 
    plot.title.position = 'plot',
    plot.caption = element_text(size = 12, hjust = .5), 
    legend.position = 'none', 
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10) 
  )
ggsave("frog.png", width = 1920, height = 1080, units = "px", dpi = 132)

