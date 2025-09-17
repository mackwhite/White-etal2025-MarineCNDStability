###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): MW & NL
###goal(s): Conduct path analysis for across ecosystem drivers of CND stability
###date(s): November 2024
###note(s):
## Models originally ran in Python, but redone in R for consistency
## with other wrangling, cleaning, modeling, and visualization scripts

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, readr, lavaan)

### read in necessary data ---
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

dat_scaled <- dat |>
      select(Program, Habitat, Site, comm_n_stability, everything()) |>
      mutate(comm_n_stability = scale(comm_n_stability)) |>
      # group_by(Program) |>
      mutate(across(comm_mean_max_ss:troph_synch, \(x) scale(x, center = TRUE))) |>
      ungroup()

glimpse(dat_scaled)
dat_ready <- dat_scaled

path_model <- '
  # Structural equations for the path model
  comm_n_stability ~ mean_species_richness + troph_beta_time + beta_time + synch    # Stability regressed on Richness, Trophic Turnover, Pop. Turnover, Synchrony
  beta_time        ~ mean_species_richness + mean_trophic_richness                  # Population Turnover regressed on Species Richness and Trophic Richness
  synch            ~ mean_species_diversity + mean_species_richness + mean_trophic_diversity  # Population Synchrony on Species Evenness, Species Richness, Trophic Evenness

  # Label specific paths to calculate indirect effects
  synch ~ a_se*mean_species_diversity   # a_se: effect of Species Evenness on Synchrony
  comm_n_stability ~ b_syn*synch        # b_syn: effect of Synchrony on Stability

  # Define the indirect effect of Species Evenness on Stability via Synchrony
  indirect_evenness := a_se * b_syn
'

fit <- sem(path_model, data = dat_ready)
summary(
      fit,
      standardized = TRUE,
      fit.measures = TRUE,
      rsquare = TRUE
)

### visualization generated in MS PowerPoint using summary data here