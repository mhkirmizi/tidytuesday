library(tidyverse)
library(ggmosaic)
library(showtext)
library(paletteer)
## Fonts
font_add_google("Alegreya")
showtext_auto()
## Colors

## data
gutenberg_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')
## Data wrangling
df <- gutenberg_metadata |> 
  filter(language == 'en') |> 
  select(gutenberg_bookshelf) |> 
  filter(!is.na(gutenberg_bookshelf)) |> 
  separate_rows(gutenberg_bookshelf, sep = "/") |> 
  mutate(gutenberg_bookshelf =  str_trim(gutenberg_bookshelf)) |> 
  count(gutenberg_bookshelf, sort = TRUE) |> 
  mutate(
    gutenberg_bookshelf = str_remove(gutenberg_bookshelf, "^Browsing:\\s*")) 
  
df2 <- df |> 
  slice_max(n, n = 10, with_ties = FALSE) |> 
  arrange(desc(n)) |>
  mutate(
    proportion = n / sum(n),
    xmin = lag(cumsum(proportion), default = 0),
    xmax = cumsum(proportion),
    xcenter = (xmin + xmax) / 2)  

df2 <- df2 |> 
  mutate(gutenberg_bookshelf = factor(gutenberg_bookshelf, 
                                      levels = gutenberg_bookshelf))

### Plotting
  ggplot() +
  geom_mosaic(data = df2, 
              aes(x = product(gutenberg_bookshelf),weight = n,
                  fill = gutenberg_bookshelf)) +
    annotate("text", 
             x = 0.103, 
             y = .5, 
             label = "Literature", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0)  +
    annotate("text", 
             x = 0.295, 
             y = .5, 
             label = "Fiction", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.44, 
             y = .5, 
             label = "Culture", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.555, 
             y = .5, 
             label = "Civilization", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.667, 
             y = .5, 
             label = "Society", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.757, 
             y = .5, 
             label = "History - General", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.83, 
             y = .5, 
             label = "Children & Young Adult Reading", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.895, 
             y = .5, 
             label = "History - American", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.945, 
             y = .5, 
             label = "Travel & Geography", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
    annotate("text", 
             x = 0.99, 
             y = .5, 
             label = "Religion", 
             angle = 90, 
             size = 10, 
             hjust = 0.5, 
             vjust = 0) +
  labs(x = "", 
       y = "", 
       title = "The Bookshelf of Project Gutenberg", 
       caption = "Data:{gutenbergr} | Vis: MhKirmizi") +
    scale_fill_paletteer_d("rcartocolor::Antique")+
  theme_bw(base_family = "Alegreya", base_size = 14) +
  theme(legend.position = 'none', 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank(), 
        plot.title = element_text(size= 56, hjust = .5, face = "bold"), 
        plot.caption = element_text(size = 14, face = "bold"))

### 
  ggsave("project_gutenberg.png", width = 1920, height = 1080, units = "px", dpi = 132)
  