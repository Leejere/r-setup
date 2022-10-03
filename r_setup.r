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

plot_theme <- function(title_size = 12, subtitle_size = 10.5, label_size = 10, tick_size = 8.5){
  theme_minimal() +
  theme(axis.text.x = element_text(color = "grey60", size = tick_size),
        axis.text.y = element_text(color = "grey60", size = tick_size),  
        axis.title.x = element_text(color = "grey20", size = label_size),
        axis.title.y = element_text(color = "grey20", size = label_size),
        plot.title = element_text(color = "gray20", size = title_size),
        plot.subtitle = element_text(color = "gray40", size = subtitle_size),
        axis.line.x = element_line(size = 0.5, colour = "gray10"))
}

# Customize x labels
x_label_unit = function(typical_scale){
  if(typical_scale >= 1000000){
    scale_x_continuous(labels = label_number(suffix = "M", scale = 1e-6))
  }else if(typical_scale >= 1000){
    scale_x_continuous(labels = label_number(suffix = "K", scale = 1e-3))
  }
}

y_label_unit = function(typical_scale){
  if(typical_scale >= 1000000){
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))
  }else if(typical_scale >= 1000){
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))
  }
}

map_theme <- function(title_size = 12, subtitle_size = 10.5, label_size = 10, tick_size = 8.5){
  theme_minimal() +
  theme(plot.title = element_text(color = "gray20", size = title_size),
        plot.subtitle = element_text(color = "gray40", size = subtitle_size),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.text = element_blank(),
        legend.background = element_blank(),
        legend.title = element_text(size = label_size),
        legend.text = element_text(size = tick_size),
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

# Get the specific color values
for(column in colnames(palette[2:4])){
  assign(paste0("palette_", column), palette[[paste0(column)]])
  for(row in palette[["role"]]){
    if(column == "regular") {
      assign(paste0("palette_", row), palette[palette$role == row,][[paste0(column)]])
    } else {
      assign(paste0("palette_", row, "_", column), palette[palette$role == row,][[paste0(column)]])
    }
  }
}
