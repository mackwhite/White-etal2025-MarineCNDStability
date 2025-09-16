###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): additional visualizations
###date(s): October 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
# remotes::install_github('m-clark/mixedup')
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
      lterpalettefinder,
      multcompView
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
      ## this is a function syntax
      mutate(across(comm_mean_max_ss:troph_synch, \(x) scale(x, center = TRUE))) |>
      ungroup()

glimpse(dat_scaled)

dat_ready <- dat_scaled
### clean env (optional) ---
rm(dat_scaled)

### set color schemes ---
habitat_palette <- c("Overall"="#000000",
                     "Back Reef"="#fde725",
                     "Bay"="#addc30",
                     "Fore Reef"="#ff967d",
                     "Fringing Reef"="#21918c",
                     "Marine Protected Area"="#2c728e",
                     "MPA"="#2c728e",
                     "Reference"="#5ec962",
                     "Riverine"="#8b6b93",
                     "Sand"="#D2B48C",
                     "Seagrass"="#64a988")

program_palette <- c("Overall"="#000000", 
                     "FCE"="#64a988", 
                     "MCR"="#ff967d", 
                     'PCCC'="#2A788EFF", 
                     "PCCS"="#8b6b93",
                     'SBC'='#ff3f4c', 
                     "VCR"="#9b9254")

disturbance_palette <- c("cold snap"="#44AA99",
                         "drought"="#D55E00",
                         "cyclone"="#117733",
                         "COT"="#8b6b93",
                         "heat wave"="#CC6677",
                         "disease"="#999933")

thermal_palette <- c('thermal'='#CC6677',
                     'nonthermal'='black')

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

ggsave("output/figs/smf1.png", units = "in", width = 6,
       height = 5, dpi =  600)

### stability ~ richness ----

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

ggsave("output/figs/fig2-panelb.png", units = "in", width = 4.2,
       height = 4.2, dpi =  600)

### updated version --- 