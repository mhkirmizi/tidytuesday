library(tidyverse)
library(worldfootballR)
library(ggsoccer)
library(ggtext)

## Data
messi<- understat_player_shots(player_url = "https://understat.com/player/2097") |> 
  mutate(X = 100 -  X *100, 
         Y = 100 -  Y * 100) |> 
  filter(result == "Goal") 

messi |> mutate(G90 = 253 / 298, Sh90 = 1462 /298, A90 = )
ronaldo <- understat_player_shots(player_url = "https://understat.com/player/2371") |>
  mutate(X = X *100, 
         Y = Y * 100) |> 
  filter(result == "Goal") 

## Aes 
colors <- c("Head" =  "red", 
            "RightFoot"  =  "yellow", 
            "LeftFoot"  =  "blue",  
            "OtherBodyPart" = "purple")
label <- c("Head" = "Head", 
           "RightFoot" = "Right Foot", 
           "LeftFoot" = "Left Foot", 
           "OtherBodyPart" = "Other Body Part")
## Text
title <- "Two Legends, Two Styles: A Tale of Goal-Scoring Brilliance"
subtitle <- "Messi's goals paint a picture of finesse and tight-angle finishes from his left foot, clustering around the penalty box with clinical precision. \nIn contrast, Ronaldo's goals reflect his athletic prowess and adaptability, with a notable share from headers, long-range strikes outside the box & other body parts. \nThe distribution tells a tale of two legends, each mastering the art of scoring in their own unique way."
caption <- "Data: Understat | Vis: MhKirmizi"


  
## Plot
  ggplot() +
  annotate_pitch(colour = "white", fill   = "springgreen4", limits = FALSE) +
  geom_point(data = messi, aes(x =X, y = Y, colour = as.factor(shotType)), alpha = .7, size = 4) +
  geom_point(data = ronaldo, aes(x =X, y = Y, colour = as.factor(shotType)), alpha = .7 , size = 4)+
  theme_pitch() +
  geom_textbox(aes(x = c(15, 85) , y = c(95, 95), label = c("Messi", "Ronaldo")), 
               fill = "springgreen4", box.colour = "springgreen4" , size = 9.5, fontface = "bold"
               ) +
  scale_color_manual(values = colors, 
                     labels = label) +
  labs(title = title, 
       subtitle = subtitle,
       caption = caption, 
       colour = "Shot Type") +
  theme(panel.background = element_rect(fill = "springgreen4"),
        plot.title = element_text(size = 28, face = "bold", hjust = .2),
        plot.subtitle = element_text(size = 14, hjust = .5), 
        plot.caption = element_text(size  = 12, hjust = 1),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom") 
ggsave("week1.pdf", dpi =360, width = 12, height = 9)  



 




  
   
  



