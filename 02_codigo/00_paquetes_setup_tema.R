### Paquetes ----
library(pacman)
p_load(broom, cowplot, gganimate, GGally, ggeffects, ggrepel, glue, ggtext, Hmisc, janitor, lme4, lmerTest, lubridate, RColorBrewer, readxl, scales, shadowtext, stargazer, tidyverse, viridis, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow", color = "grey15"), 
        plot.title = element_text(size = 28, face = "plain", margin = margin(10,0,20,0), family = "Arial Narrow", color = "grey25"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, face = "plain", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Arial Narrow"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot",
        plot.background = element_rect(color = "white", fill = "white"),
        panel.grid = element_line(linetype = 3, color = "grey90"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "plain", family = "Arial Narrow"),
        legend.text = element_text(size = 14, family = "Arial Narrow"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "plain", margin = margin(0,0,0,0), family = "Arial Narrow"),
        axis.text = element_text(size = 16, face = "plain", family = "Arial Narrow"),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(color = "white", size = 14))

