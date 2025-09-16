###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): create a map for figure one
###date(s): November 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
# remotes::install_github('m-clark/mixedup')
### install necessary packages ---
librarian::shelf(tidyverse, readr, scales, ggplot2, dataRetrieval, janitor, readxl, writexl, zoo,
                 grid, ggalt, factoextra, cluster, paran, data.table, viridis, vegan,
                 ggspatial, sf, cowplot, patchwork, openintro, shapefiles, broom, ggspatial, GISTools,
                 maps, ggmap, ggthemes)


### read in necessary data ---
map_dt <- read_csv("local_data/LTER_Site_Coords_w_information.csv") |> 
      filter(Site %in% c("FCE", "MCR", "SBC", "VCR")) |> 
      rename(program = Site,
             lat = Latitude,
             long = Longitude) |> 
      dplyr::select(program, lat, long)
glimpse(dt)

pisco <- data.frame(
      program = c("PCCC", "PCCS"),
      lat = c(38.47409, 35.5000),
      long = c(-123.2485, -121.019020)
)

dt <- rbind(map_dt, pisco)

### set up color palette ---
palette <- c("Overall"="#000000", "FCE"="#64a988", "MCR"="#ff967d", 'PCCC'="#2A788EFF", "PCCS"="#8b6b93",
             'SBC'='#ff3f4c', "VCR"="#9b9254")

### set theme ---
theme_set(theme_minimal())

### read in shapefiles ----
world <- st_read('../../qgis/continent/world-continents.shp')
glimpse(world)

### read in coordinates ----
coords <- read_csv("local_data/LTER_Site_Coords_w_information.csv") |> 
      filter(Site %in% c("FCE", "MCR", "SBC", "VCR")) |> 
      rename(program = Site,
             y = Latitude,
             x = Longitude) |> 
      dplyr::select(program, y, x)
glimpse(coords)
pisco <- data.frame(
      program = c("PCCC", "PCCS"),
      y = c(38.47409, 35.5000),
      x = c(-123.2485, -121.019020)
)
dt <- rbind(coords, pisco) |> 
      mutate(program = as.factor(program)) |> 
      st_as_sf(coords = c("x", "y"), crs = 4326) |> 
      st_transform(crs = 26917)

### set up color palette ---
palette <- c("Overall"="#000000", "FCE"="#64a988", "MCR"="#ff967d", 'PCCC'="#2A788EFF", "PCCS"="#8b6b93",
             'SBC'='#ff3f4c', "VCR"="#9b9254")

all <- ggplot() +
      geom_sf(data = world, fill = "grey", color = NA) +
      geom_sf(data = dt, color = "white", size = 5) +
      geom_sf(data = dt, aes(color = program), size = 4) +  
      annotation_scale(location = "br", width_hint = 0.3, 
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "tl", which_north = "true", 
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ), 
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = palette, breaks = levels(dt$program)) +
      coord_sf(xlim = c(-148.7, -70),
               ylim = c(-20.0, 45.0)) +
      theme_bw() +
      theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "aliceblue", color = NA),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = c(0.11,0.5),
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold')
      )

all
# ggsave("output/figs/sitemap.png", units = "in", width = 5,
#        height = 5, dpi =  600)