library(tidyverse)
library(cowplot)
library(circlize)
library(showtext)
library(patchwork)


## Colors
pal<-c( "Teenager" = "#6497B1FF", "Young" = "#6A359CFF", "Middle Age" = "#FFB04FFF", "Late Middle Age" =  "#679C35FF", "Elderly" = "#CD1076FF")

age_gaps <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv') 
age_cat  <-  age_gaps |> 
    filter(character_1_gender != character_2_gender) |> 
  mutate(age_man = ifelse(character_1_gender == "man", actor_1_age, actor_2_age),
         age_woman = ifelse(character_1_gender == "woman", actor_1_age, actor_2_age)) |> 
  mutate(
    from = case_when(
      age_man >= 17 & age_man <= 19 ~ "Teenager",
      age_man >= 20 & age_man <= 29 ~ "Young",
      age_man >= 30 & age_man <= 39 ~ "Middle Age",
      age_man >= 40 & age_man <= 59 ~ "Late Middle Age", 
      age_man >= 6 ~ "Elderly",
      TRUE ~ NA_character_ # Prevents NA issues
    ),  
    to = case_when(
      age_woman >= 17 & age_woman <= 19 ~ "Teenager", 
      age_woman >= 20 & age_woman <= 29 ~ "Young",
      age_woman >= 30 & age_woman <= 39 ~ "Middle Age",
      age_woman >= 40 & age_woman <= 59 ~ "Late Middle Age",
      age_woman >= 60 ~ "Elderly",
      TRUE ~ NA_character_ # Prevents NA issues
    )) |> 
  select(from, to)
 
age_cat <- age_cat |> 
  group_by(from, to) |> 
  count() |> 
  arrange(-n)
 age_cat_matrix <- age_cat |> 
   pivot_wider(names_from = to, values_from = n, values_fill = list(n = 0)) |> 
   column_to_rownames("from") |> 
   as.matrix()
 ## ordering the variable
row.names(age_cat_matrix)
order <- c('Teenager', 'Young', 'Middle Age', 'Late Middle Age', 'Elderly')
age_cat_matrix <- age_cat_matrix[order, order]

circlize::chordDiagram(age_cat, transparency = .6, grid.col = pal,
                       annotationTrack = c("name", "grid"))
# Record the plot
p <- recordPlot()

# Replay and draw the plot
replayPlot(p)

# Now, you can add titles and captions if desired
ggplotify::as.ggplot(ggdraw(p)) +
  labs(
    title = "The Diagram of Hollywood Love Interest ", 
    caption = "Data: TidyTuesday | Vis: MhKirmizi") +
  theme(
    text=element_text(family="Arial"), 
    plot.title=element_text(hjust=.5, face="bold", size=18), 
    plot.caption=element_text(size=10, hjust=0.95, margin=margin(b=12)), 
    plot.margin =margin(t=20))
ggsave("love_diagram.png", width = 1344, height = 1080, units = "px", dpi = 132)


