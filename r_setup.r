library(tidyverse)
library(sf)
library(lubridate)
library(tidycensus)
library(tigris)
library(gganimate)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(FNN)
library(spdep)
library(caret)
library(ckanr)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(scales)
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(mapview)
census_api_key("c16daad4da8c67d74b58b082c036a84ba5a7d861", overwrite = TRUE)
crs = 'EPSG:2272' # State Plain
options(scipen = 999)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
brightRed = '#f2727f'
darkBlue = '#315d7f'
purpleBlue = "#353795"
green = "#7cbfa4"

palette = read.csv("https://raw.githubusercontent.com/Leejere/python-visualization-preset/main/palette.csv")

plot_theme <- function(base_size = 10, title_size = 12){
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )
}

map_theme <- function(base_size = 9, title_size = 10){
  theme(
    text = element_text(family = 'font', color = "black"),
    plot.title = element_text(family = 'font',
                              size = title_size, colour = "black", hjust = 0.5), 
    plot.subtitle = element_text(family = 'font', face = 'italic',
                                 size = base_size, colour = "black", hjust = 0.5),
    plot.caption = element_text(family = 'font', hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=base_size),
    axis.title = element_text(family = 'font', size=9),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(family = 'font', colour = "black", face = "italic", size = 9),
    legend.text = element_text(family = 'font', colour = "black", face = "italic", size = 7),
    strip.text.x = element_text(size=base_size),
    legend.key.size = unit(.5, 'line')
  )
}

# identify small object against region: which region does the small object lie in
spatial_identify = function(identifiable, # sf, to be identified against region
                            identifier, # sf, region
                            key_column, # the key column of the sf to be identified
                            identifier_name # the column of the region sf to be transferred into the identified sf
){
  identified = 
    identifiable %>% 
    left_join(., # The original sf
              
              # Below, create a table (not sf) that has every entry in Identifiable identified by the identifier
              st_join(identifiable %>% st_centroid() %>% dplyr::select(paste0(key_column)),
                      identifier %>% dplyr::select(paste0(identifier_name)),
                      join = st_within) %>%
                st_drop_geometry(), # Make this a pure table
              by = key_column
    )
  identified[[paste0(identifier_name)]] = 
    replace(identified[[paste0(identifier_name)]], 
            is.na(identified[[paste0(identifier_name)]]), 
            "other")
  return(identified)
}
