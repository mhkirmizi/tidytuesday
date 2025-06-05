library(tidyverse)
library(hrbrthemes)
library(dlookr)
library(showtext)
### Fonts
font_add_google('Oswald', 'Oswald')
showtext_auto()


## Colors
bg_color <- 'white'
c("#063B41FF","#FE7F9DFF", "#09979BFF", "grey50")

## Data Wrangling 
tuesdata <- tidytuesdayR::tt_load(2024, week = 32)
olympics <- tuesdata$olympics

dim(olympics)
diagnose(olympics)
diagnose_numeric(olympics)
diagnose_outlier(olympics) |> 
  filter(outliers_cnt>0)
describe(olympics)
normality(olympics)

data_for_plot <- olympics |> 
  select(-id) |> 
  group_by(sex, year) |> 
  summarise_if(is.numeric, mean, na.rm = TRUE) |> 
  mutate(sex = case_when(sex == "F" ~ "Female", 
                   sex == "M" ~ "Male")) |> 
  rename(Sex = sex, 
         Age = age, 
         Year = year, 
         Height = height)
### Plot
ggplot(data_for_plot, aes(Year,Age, color = Sex)) +
  geom_point(size = 4, aes(shape = Sex))+
  geom_line()+
  geom_hline(yintercept = 26, linetype = "dashed", 
             color = "grey50", linewidth = 0.8) +
  scale_color_manual(values = c("Female" = "#FE7F9DFF", "Male" = "#09979BFF")) +
  scale_x_continuous(breaks = seq(1890, 2016, by= 20)) +
  labs(
    x=  '',
    y = "Age",
    title = "Age of Athletes", 
    caption = "Data = www.sports-reference.com, Vis by MhKirmizi"
  ) +
  theme_ipsum(base_family = 'Oswald', base_size = 12) +
theme(
  legend.position = c(.9,.8), 
  legend.justification = c(.9,.8),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 20, hjust = .5, face = 'bold', color = "#063B41FF"),
  plot.background = element_rect(fill = bg_color, colour = bg_color),
  panel.background = element_rect(fill = bg_color, colour = bg_color),
  panel.grid.major.x = element_blank(), 
  panel.grid.minor.x = element_blank(), 
  panel.grid.minor.y = element_blank(), 
  axis.title.y = element_text(size = 16, hjust = .5),
  plot.title = element_text(size= 48, face = "bold", 
                            vjust = .5, hjust = .5, color = "#063B41FF" ), 
  plot.subtitle = element_text(size = 24, vjust = .5, hjust = .5), 
  plot.caption = element_text(size = 12)
)
ggsave("age_of_athletes.png", width = 1920, height = 1080, units = "px", dpi = 132)


  
  