###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): MW, WRJ, & NL
###goal(s): Conduct Bayesian hierarchical models to evaluate within-ecosystem drivers
###date(s): October 2024
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(
      tidyverse,
      readxl,
      MuMIn,
      sjPlot,
      lme4,
      corrplot,
      performance,
      ggeffects,
      ggpubr,
      parameters,
      ggstats,
      brms,
      mixedup,
      lterpalettefinder
)

### read in necessary data ---
dat <- read_csv('local_data/dsr-eco-org-raw-all.csv') |>
      rename(
            Program = program,
            Trophic_Group = troph_group,
            Species = scientific_name,
            Habitat = habitat,
            Site = site
      ) |>
      select(
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
      group_by(Program) |>
      mutate(across(comm_mean_max_ss:troph_synch, \(x) scale(x, center = TRUE))) |>
      ungroup()

glimpse(dat_scaled)

dat_ready <- dat_scaled

### set priors following Lemoine (2019, Ecology)
pr = prior(normal(0, 1), class = 'b')

###########################################################################
# full models -------------------------------------------------------------
###########################################################################
test_corr <- dat_ready |> select(mean_species_richness,mean_species_diversity,
                                 mean_trophic_richness,mean_trophic_diversity,
                                 comm_mean_max_ss,comm_mean_skew_ss,
                                 beta_time,synch,
                                 troph_beta_time,troph_synch)

matrix <- cor(test_corr, use = 'complete.obs')

corrplot(matrix, method = "number", type = "lower", tl.col = "black", tl.srt = 45)

### following correlations to be aware of
# all of the richness and diversity metrics
# comm_mean_skew_ss ~ both richness metrics
# troph_synch ~ synch
# regardless, once synchrony or turnover is selected at a level of organization
# will quit dropping from the model selection process

rm(matrix,test_corr)

### round one ---
m1 <- brm(
      comm_n_stability ~ mean_trophic_richness + (mean_trophic_richness |
                                                        Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m4 <- brm(
      comm_n_stability ~ synch + (synch | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

### best fit single term model
# saveRDS(m4, file = 'local_data/rds-single-synchrony.rds')

m5 <- brm(
      comm_n_stability ~ beta_time + (beta_time | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m6 <- brm(
      comm_n_stability ~ troph_synch + (troph_synch | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m7 <- brm(
      comm_n_stability ~ troph_beta_time + (troph_beta_time | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m8 <- brm(
      comm_n_stability ~ mean_species_richness + (mean_species_richness | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

### model of interest, given across-ecosystem importance
# saveRDS(m8, file = 'local_data/rds-single-richness.rds')

m9 <- brm(
      comm_n_stability ~ mean_species_diversity + (mean_species_diversity | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m10 <- brm(
      comm_n_stability ~ mean_trophic_diversity + (mean_trophic_diversity | Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

model_table_all <- performance::compare_performance(m1,m4,m5,m6,m7,m8,m9,m10)

model_selection <- model_table_all |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "output/tables/brms-fullmodel-selection-table-roundone.csv")

rm(list = setdiff(ls(), c("dat_ready", "pr", "palette", 'm4')))

### round two ---
m41 <- brm(
      comm_n_stability ~ mean_trophic_richness + synch + (mean_trophic_richness + synch |
                                                                Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m45 <- brm(
      comm_n_stability ~ beta_time + synch + (beta_time + synch |
                                                    Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m47 <- brm(
      comm_n_stability ~ troph_beta_time + synch + (troph_beta_time + synch |
                                                          Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m48 <- brm(
      comm_n_stability ~ mean_species_richness + synch + (mean_species_richness + synch |
                                                                Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m49 <- brm(
      comm_n_stability ~ mean_trophic_diversity + synch + (mean_trophic_diversity + synch |
                                                                 Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m40 <- brm(
      comm_n_stability ~ mean_species_diversity + synch + (mean_species_diversity + synch |
                                                                 Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

model_table_all <- performance::compare_performance(m41,m4,m45,m47,m48,m49,m40)

model_selection <- model_table_all |>
      mutate(dWAIC = WAIC - min(WAIC))
# write_csv(model_selection, "output/tables/brms-fullmodel-selection-table-roundtwo.csv")

rm(list = setdiff(ls(), c("dat_ready", "pr", "palette",'m47')))

### round three - 

m471 <- brm(
      comm_n_stability ~ mean_trophic_richness + troph_beta_time + synch + (troph_beta_time + mean_trophic_richness + synch |
                                                                                  Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m475 <- brm(
      comm_n_stability ~ beta_time + synch + troph_beta_time + (troph_beta_time + beta_time + synch |
                                                                      Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)


m478 <- brm(
      comm_n_stability ~ mean_species_richness + troph_beta_time + synch + (troph_beta_time + mean_species_richness + synch |
                                                                                  Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m479 <- brm(
      comm_n_stability ~ mean_trophic_diversity + synch + troph_beta_time + (troph_beta_time + mean_trophic_diversity + synch |
                                                                                   Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

m470 <- brm(
      comm_n_stability ~ mean_species_diversity + synch + troph_beta_time + (troph_beta_time + mean_species_diversity + synch |
                                                                                   Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

model_table <- performance::compare_performance(m47,m471,m475,m478,m479,m470)
model_selection_full <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))
# write_csv(model_selection_full, "output/tables/brms-fullmodel-selection-table-roundthree.csv")
rm(list = setdiff(ls(), c("dat_ready", "pr", "program_palette",'m470')))

### round four ---
m4705 <- brm(
      comm_n_stability ~ mean_species_diversity + beta_time + synch + troph_beta_time + (mean_species_diversity+ troph_beta_time + beta_time + synch |
                                                                                               Program),
      data = dat_ready,
      prior = pr,
      warmup = 1000,
      iter = 10000,
      chains = 4
)

model_table <- performance::compare_performance(m470,m4705)
model_selection_full <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

rm(list = setdiff(ls(), c("dat_ready", "pr", "program_palette",'m4705')))
# write_csv(model_selection_full, "output/tables/brms-fullmodel-selection-table-roundfour.csv")

full_model <- m4705
# saveRDS(full_model, file = 'local_data/rds-full-model.rds')
