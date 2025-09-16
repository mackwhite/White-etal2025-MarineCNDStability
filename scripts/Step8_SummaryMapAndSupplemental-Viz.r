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
### read in necessary data ---
ann_dt <- read_csv("local_data/annual-dt-for-summary.csv")

### set color schemes ---

program_palette <- c("Overall"="#000000", 
                     "FCE"="#64a988", 
                     "MCR"="#ff967d", 
                     'PCCC'="#2A788EFF", 
                     "PCCS"="#8b6b93",
                     'SBC'='#ff3f4c', 
                     "VCR"="#9b9254")

# map insets --------------------------------------------------------------

ann_dt |> 
      filter(program == "SBC") |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-2~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000)) +
      scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/sbc-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

ann_dt |> 
      filter(program == "MCR") |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-2~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000))+
      scale_x_continuous(breaks = c(2006,2010,2014,2018,2022)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/mcr-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

ann_dt |> 
      filter(program == "VCR") |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-2~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,250,500,750,1000,1250))+
      scale_x_continuous(breaks = c(2012,2014,2016,2018)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/vcr-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

ann_dt|> 
      filter(program == "FCE") |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-1~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,1500,3000,4500,6000,7500,9000,10500,12000,13500))+
      scale_x_continuous(breaks = c(2005,2008,2011,2014,2017,2020,2023)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/fce-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

ann_dt |> 
      filter(program == "PCCC") |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-2~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,2000,4000,6000,8000)) +
      scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/pccc-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

ann_dt |> 
      filter(program == "PCCS"&total_nitrogen_ann <15000) |> 
      ggplot(aes(x = year, y = total_nitrogen_ann, group = site, color = program)) +
      geom_line(alpha = 0.8, linewidth = 1) +
      labs(x = 'Year',
           y = expression(bold('Aggregate Nitrogen Supply (μg '*~m^-2~""~hr^-1*')'))) +
      theme_classic() +
      scale_color_manual(values = program_palette) +
      scale_y_continuous(breaks = c(0,3000,6000,9000,12000))+
      scale_x_continuous(breaks = c(2000,2005,2010,2015,2020)) +
      theme(
            axis.text = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 14, color = "black"),
            axis.title.x = element_blank(),
            axis.line = element_line("black"),
            legend.position = "none",
            legend.text = element_text(face = "bold", size = 14, color = "black"),
            legend.title = element_text(face = "bold", size = 14, color = "black"),
            panel.background = element_rect(fill = "white"),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 12, color = "black"))

# ggsave("output/figs/map_insets/pccs-timeseries.png", units = "in", width = 5,
#        height = 5, dpi =  600)

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 

###########################################################################
# Section Three - Supplemental Figure(s)-----------------------------------
###########################################################################

# Kelp Empirical Excretion Comparison -------------------------------------
emp <- read_csv('local_data/empirical_excretion_kelp.csv') |> 
      dplyr::rename(order = TAXON_ORDER,
                    family = TAXON_FAMILY,
                    genus = TAXON_GENUS,
                    species = TAXON_SPECIES,
                    wetmass_g = WM_g,
                    diet_cat = `Functional group`,
                    nind_umol_hr = `exc_rate_NH4_umol_hour-1`,
                    region = Region) |> 
      mutate(scientific_name = paste(genus, species, sep = " "),
             nind_ug_hr = 18.042*nind_umol_hr) |> 
      dplyr::select(scientific_name, wetmass_g, nind_ug_hr, nind_umol_hr, order, family, genus, species, diet_cat, region)
glimpse(emp)

species_list <- emp |> dplyr::select(scientific_name, order, family, genus, diet_cat, region) |> distinct()
# write_csv(species_list, "../../../to be filed/kelp_empirical_species_list.csv")

# dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =".") |> 
#       janitor::clean_names()
# glimpse(dt)
# 
# dt_test1 <- dt |> 
#       select(project, scientific_name, order, family, genus, diet_cat) |>
#       filter(project %in% c('CoastalCA', 'SBC')) |> 
#       distinct() |> 
#       select(-project) |> 
#       distinct()
# write_csv(dt_test1, "../../../to be filed/kelp_modeled_species_list.csv")

### read in empirical species list with diet categorized for models ----
kelp_diet <- read_csv('local_data/kelp_empirical_species_list_updated.csv')
glimpse(kelp_diet)      
kelp <- emp |> dplyr::select(-diet_cat)
glimpse(kelp)
kelp_all <- kelp |> left_join(kelp_diet) |> distinct()

### clean up environment
keep <- c("kelp_all")
rm(list = setdiff(ls(), keep))

### format model data ----
kelp1 <- kelp_all |> 
      rename(nind_ug_hr_emp = nind_ug_hr,
             nind_umol_hr_emp = nind_umol_hr) |> 
      mutate(drymass_g = wetmass_g*dm_conv,
             phylum = "Chordata") |> 
      mutate(temp = case_when(
            region == "Central California" ~ 13,
            region == "Southern California" ~ 17
      ))
glimpse(kelp1)

### generate coefficients from vanni and mcintyre model ----
kelp2 <- kelp1 |> 
      ### vertebrate coefficient classification
      mutate(vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
             vert_coef_upper = if_else(phylum == "Chordata", 0.7804 + 0.0655, 0),
             vert_coef_lower = if_else(phylum == "Chordata", 0.7804 - 0.0655, 0)) |> 
      ### diet coefficient classification
      mutate(diet_coef = case_when(
            diet_cat == "algae_detritus" ~ -0.0389,
            diet_cat == "invert" ~ -0.2013,
            diet_cat == "fish" ~ -0.0537,
            diet_cat == "fish_invert" ~ -0.1732,
            diet_cat == "algae_invert" ~ 0,
            TRUE ~ NA)) |> 
      mutate(diet_coef_upper = case_when(
            diet_cat == "algae_detritus" ~ -0.0389 + 0.0765,
            diet_cat == "invert" ~ -0.2013 + 0.0771,
            diet_cat == "fish" ~ -0.0537 + 0.2786,
            diet_cat == "fish_invert" ~ -0.1732 + 0.1384,
            diet_cat == "algae_invert" ~ 0,
            TRUE ~ NA)) |> 
      mutate(diet_coef_lower = case_when(
            diet_cat == "algae_detritus" ~ -0.0389 - 0.0765,
            diet_cat == "invert" ~ -0.2013 - 0.0771,
            diet_cat == "fish" ~ -0.0537 - 0.2786,
            diet_cat == "fish_invert" ~ -0.1732 - 0.1384,
            diet_cat == "algae_invert" ~ 0,
            TRUE ~ NA)) |> 
      ### temperature coefficient classification
      mutate(temp_coef = 0.0246,
             temp_coef_upper = 0.0246 + 0.0014,
             temp_coef_lower = 0.0246 - 0.0014) |> 
      ### dry mass coefficient classification
      mutate(dm_coef = 0.6840,
             dm_coef_upper = 0.6840 + 0.0177,
             dm_coef_lower = 0.6840 - 0.0177) |> 
      ### intercept coefficient classification
      mutate(int_coef = 1.4610,
             int_coef_upper = 1.4610 + 0.0897,
             int_coef_lower = 1.4610 - 0.0897)
glimpse(kelp2)

### calculate excretion rates ----
kelp3 <- kelp2 |> 
      mutate(n10 = int_coef + dm_coef*(log10(drymass_g)) + temp_coef*temp + diet_coef + vert_coef,
             n10_lower = int_coef_lower + dm_coef_lower*(log10(drymass_g)) + temp_coef_lower*temp + diet_coef_lower + vert_coef_lower,
             n10_upper = int_coef_upper + dm_coef_upper*(log10(drymass_g)) + temp_coef_upper*temp + diet_coef_upper + vert_coef_upper) |> 
      mutate(nind_ug_hr = 10^n10,
             nind_ug_hr_lower = 10^n10_lower,
             nind_ug_hr_upper = 10^n10_upper)

### format model data for visualization ----
kelp4 <- kelp3 |> 
      dplyr::select(scientific_name, wetmass_g, drymass_g, diet_cat,
                    nind_ug_hr_emp, nind_ug_hr, nind_ug_hr_lower, nind_ug_hr_upper,
                    phylum, order, family, genus, species) |> 
      mutate(
            n10_emp = log10(nind_ug_hr_emp),
            n10_mod = log10(nind_ug_hr),
            n10_mod_low = log10(nind_ug_hr_lower),
            n10_mod_upp = log10(nind_ug_hr_lower),
            n10_dm = log10(drymass_g)
      ) |> 
      group_by(scientific_name) |> mutate(n = n()) |> ungroup() |> 
      filter(n > 2) |> 
      dplyr::select(-n) |> 
      rename(diet = diet_cat) |> 
      group_by(scientific_name) |> mutate(species_n = n()) |> ungroup() |> 
      group_by(genus) |> mutate(genus_n = n()) |> ungroup() |> 
      group_by(family) |> mutate(family_n = n()) |> ungroup()

### evaluate correlation ----
mod <- glmmTMB(
      n10_emp ~ n10_mod, family = gaussian(), data = kelp4
)
summary(mod)
performance::performance(mod)

kelp4 |> 
      ggplot(aes(x = n10_mod, y = n10_emp)) +
      geom_point(size = 2) +
      geom_smooth(method = lm, color = "#003153", fill = "#003153") +
      annotate('text', 
               x = 1.45, y = 5.2,
               label = bquote({R^2} == 0.76),
               size = 6) +
      annotate('text', 
               x = 1.57, y = 4.95,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 6) +
      theme_classic() +
      scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1.13,5.233)) +
      scale_x_continuous(breaks = c(1,2,3,4,5), limits = c(1.13,5.233)) +
      ylab(expression(bold("Empirical Log" [10] * " N Excretion (" * mu * "g" %.% ind^-1 %.% hr^-1 * ")"))) +
      xlab(expression(bold("Modeled Log" [10] * " N Excretion (" * mu * "g" %.% ind^-1 %.% hr^-1 * ")"))) +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 14),
            axis.text.y = element_text(face = "bold", color = "black", size = 14),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

# ggsave("output/figs/supplemental-model-validation.png", units = "in", width = 6,
#        height = 5, dpi =  600)

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 

# Period of Record Influence Figure ---------------------------------------
ann_dt <- read_csv("local_data/annual-dt-for-summary.csv")

program_palette <- c("Overall"="#000000", 
                     "FCE"="#64a988", 
                     "MCR"="#ff967d", 
                     'PCCC'="#2A788EFF", 
                     "PCCS"="#8b6b93",
                     'SBC'='#ff3f4c', 
                     "VCR"="#9b9254")

summary <- ann_dt |> 
      group_by(program, site) |> 
      summarize(years = n_distinct(year)) |> 
      rename(Program = program,
             Site = site)

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

dat_summary <- dat |> 
      dplyr::select(Site, comm_n_stability)

all <- left_join(dat_summary, summary, by = "Site")  

model <- lm(comm_n_stability ~ years, data = all)
summary(model)$r.squared 
r2 <- summary(model)$r.squared

summary(model)

a <- all |>
      # filter(Program != "VCR") |> 
      ggplot(aes(x = years, y = comm_n_stability)) +
      geom_point(aes(color = Program), size = 2) +  # Adds the scatter plot points
      geom_smooth(method = "lm", size = 2, color = "black", linetype = "solid", se = FALSE) +
      # geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Period of Record (years)",
           y = "CND Stability") +
      theme_classic() +
      annotate('text',
               x = 8, y = 5,
               label = bquote({R^2} == 0.04),
               size = 4) +
      scale_color_manual(values = program_palette) +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_text(face = "bold", color = "black"),
            legend.position = "right",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

all_short <- all |> filter(Program != "VCR")

model <- lm(comm_n_stability ~ years, data = all_short)
summary(model)$r.squared 
r2 <- summary(model)$r.squared

b <- all |>
      filter(Program != "VCR") |>
      ggplot(aes(x = years, y = comm_n_stability)) +
      geom_point(aes(color = Program), size = 2) +  # Adds the scatter plot points
      geom_smooth(method = "lm", size = 2, color = "black", linetype = "dashed", se = FALSE) +
      # geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
      labs(x = "Period of Record (years)",
           y = "CND Stability") +
      theme_classic() +
      annotate('text',
               x = 13.5, y = 5,
               label = bquote({R^2} == 0.004),
               size = 4) +
      scale_color_manual(values = program_palette) +
      theme(axis.text.x = element_text(face = "bold", color = "black"),
            axis.text.y = element_text(face = "bold", color = "black"),
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_text(face = "bold", color = "black"),
            legend.position = "right",
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

ggarrange(a, b,
          labels = c('a)','b)'),
          ncol = 2, vjust = 1.3, align = "h",
          common.legend = TRUE, legend = 'right')

# ggsave("output/figs/smf2.png", units = "in", width = 8,
#        height = 4, dpi =  600)

# CLEAN ENVIRONMENT -------------------------------------------------------
rm(list = ls()) 