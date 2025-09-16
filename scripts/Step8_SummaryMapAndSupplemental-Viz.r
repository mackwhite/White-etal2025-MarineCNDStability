###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): MW, WRJ, & NL
###goal(s): Visualizing summary, site, and supplemental information
###date(s): November 2024 @ nceas
###note(s): 
## Section 1 - Summary/Additional Figure(s)
## Section 2 - Map Figure(s)
## Section 3 - Supplemental Figure(s)

### load necessary libraries
### install.packages("librarian")
librarian::shelf(
      tidyverse,
      readxl,
      sjPlot,
      corrplot,
      performance,
      ggeffects,
      ggpubr,
      parameters,
      ggstats,
)

###########################################################################
# Section One - Summary/Additional Figure(s)-------------------------------
###########################################################################

#  Boxplot of CND Stability by Ecosystem ----------------------------------
dat <- read_csv('local_data/dsr-eco-org-raw-all.csv') |>
      dplyr::rename(
            Program = program,
            Trophic_Group = troph_group,
            Species = scientific_name,
            Habitat = habitat,
            Site = site
      ) |>
      dplyr::select(
            Program,
            Habitat,
            Site,
            comm_mean_max_ss,
            comm_mean_skew_ss,
            comm_mean_bm,
            comm_sd_bm,
            comm_bm_stability,
            comm_mean_n,
            comm_sd_n,
            comm_n_stability,
            comm_mean_p,
            comm_sd_p,
            comm_p_stability,
            mean_species_richness,
            mean_species_diversity,
            mean_trophic_richness,
            mean_trophic_diversity,
            beta_time,
            synch,
            troph_beta_time,
            troph_synch
      ) |>
      distinct()

program_palette <- c("Overall"="#000000", 
                     "FCE"="#64a988", 
                     "MCR"="#ff967d", 
                     'PCCC'="#2A788EFF", 
                     "PCCS"="#8b6b93",
                     'SBC'='#ff3f4c', 
                     "VCR"="#9b9254")

### stability ~ program ----
summ_test <- dat |>  group_by(Program) |> summarize(mean = mean(comm_n_stability),
                                                    median = median(comm_n_stability))

dat |> 
      mutate(Program = factor(
            Program,
            levels = c("MCR", "PCCS", "FCE", "PCCC", "SBC", "VCR")
      )) |>
      ggplot(aes(x=Program, y=comm_n_stability, fill = Program)) +
      geom_jitter(aes(color = Program), shape = 16, size = 2, width = 0.2, alpha = 1.0)+
      geom_boxplot(outlier.shape = NA, alpha = 0.35) +
      # geom_jitter(aes(fill = Program), shape = 21, stroke = 0.75, color = "black", width = 0.2, alpha = 1.0)+
      # facet_wrap(~program, scales = 'free')+
      scale_fill_manual(values = program_palette) + 
      scale_color_manual(values = program_palette) +
      labs(y = "CND Stability", 
           fill = "Program") +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_text(face = "bold", color = "black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"),
            strip.text = element_text(face = "bold", color = "black"))

# ggsave("output/figs/figure-one.png", units = "in", width = 6,
#        height = 5, dpi =  600)

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls())

### read in data ---
dat <- read_csv('local_data/dsr-eco-org-raw-all.csv') |>
      dplyr::rename(
            Program = program,
            Trophic_Group = troph_group,
            Species = scientific_name,
            Habitat = habitat,
            Site = site
      ) |>
      dplyr::select(
            Program,
            Habitat,
            Site,
            comm_mean_max_ss,
            comm_mean_skew_ss,
            comm_mean_bm,
            comm_sd_bm,
            comm_bm_stability,
            comm_mean_n,
            comm_sd_n,
            comm_n_stability,
            comm_mean_p,
            comm_sd_p,
            comm_p_stability,
            mean_species_richness,
            mean_species_diversity,
            mean_trophic_richness,
            mean_trophic_diversity,
            beta_time,
            synch,
            troph_beta_time,
            troph_synch
      ) |>
      distinct()

#  Simple Richness Stability Regression -----------------------------------
model <- lm(log1p(comm_n_stability) ~ log1p(mean_species_richness), data = dat)
summary(model)$r.squared 
summary(model)
r2 <- summary(model)$r.squared

summ <- dat |>
      group_by(Program) |> 
      mutate(stability = mean(comm_n_stability),
             richness = mean(mean_species_richness))

summ_model <- lm(log1p(stability) ~ log1p(richness), data = summ)
summary(summ_model)$r.squared 
summary(summ_model)
r2_summ <- summary(summ_model)$r.squared

dat |>
      ggplot(aes(x = log1p(mean_species_richness), y = log1p(comm_n_stability))) +
      geom_smooth(method = "lm", size = 1.5, color = "black", linetype = "solid", se = FALSE) +
      geom_point(aes(color = Program), size = 1.5, alpha = 0.30) +
      geom_point(aes(x = log1p(richness), y = log1p(stability), color = Program), size = 5, dat = summ) +
      labs(x = "log(Species Richness)",
           y = "log(CND Stability)") +
      theme_classic() +
      annotate('text', 
               x = 0.74, y = 1.8,
               label = bquote({R^2} == 0.90),
               size = 5) +
      annotate('text', 
               x = 0.89, y = 1.7,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 5) +
      scale_color_manual(values = program_palette) +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 14),
            axis.text.y = element_text(face = "bold", color = "black", size = 14),
            axis.title.x = element_text(face = "bold", color = "black", size = 16),
            axis.title.y = element_text(face = "bold", color = "black", size = 16),
            legend.position = c(0.95, 0.05),
            legend.justification = c(1, 0),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

# ggsave("output/figs/fig2-panelb.png", units = "in", width = 4.2,
#        height = 4.2, dpi =  600)

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 

###########################################################################
# Section Two - Map Figure(s)----------------------------------------------
###########################################################################

#  Main Map ---------------------------------------------------------------
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

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls())

#  Map Time Series --------------------------------------------------------


# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 

###########################################################################
# Section Three - Supplemental Figure(s)-----------------------------------
###########################################################################

# Kelp Empirical Excretion Comparison -------------------------------------


# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 


# Period of Record Influence Figure ---------------------------------------


# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 