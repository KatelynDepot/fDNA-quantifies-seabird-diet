
# SETUP -----------------------------------------------------------------------
# setwd("..")
library(ggplot2)
source("R/my_palette.R")

#------------------------------------------------------------------------------
# FECAL DNA DIET SUMMARY FOR PLOTTING  - COMBINED
#------------------------------------------------------------------------------

# group together prey that don't make up >5% by any method in any group
combined_rra <- read.csv('processed_data/TUPU_rra_combined.csv') |>
  dplyr::mutate(Fish = Fish + 
                  Sablefish + Red_Irish_lord + Sandfish + Medusafish + Sand_lance +
                  Sculpin + Buffalo_sculpin + Grunt_sculpin +
                  Sailfin_sculpin + Longfin_sculpin + Sharpnose_sculpin +
                  Rockfish + Stenobrachius + Lampanyctus,
                Pollock = Gadus,
                Tomcod = Gadid,
                Invert = Ctenophore + Squid,
                .keep = "unused")

# foo table
combined_foo <- combined_rra |>
  dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# sample sizes
sample_sizes <- combined_foo |> dplyr::count(type)

# intermediate tables
foo_sum <- combined_foo |>
  tidyr::pivot_longer(cols = Copepod:Invert, names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = dplyr::case_when(type == "Adult_CHR" ~ n / sample_sizes$n[1],
                                                 type == "Adult_INC" ~ n / sample_sizes$n[2],
                                                 type == "Chick" ~ n / sample_sizes$n[3])) |>
  dplyr::select(-n)
rra_sum <- combined_rra |>
  tidyr::pivot_longer(cols = Copepod:Invert, names_to = "Prey", values_to = "rra") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(mean_rra = mean(rra)) |>
  dplyr::ungroup()

# summary table
diet_sum <- foo_sum |>
  dplyr::left_join(rra_sum, dplyr::join_by(type, Prey))

# re-arrange table
diet_sum <- diet_sum |>
  tidyr::pivot_longer(cols = 3:4, names_to = "Method", values_to = "Proportion")

#------------------------------------------------------------------------------
# BARPLOT
#------------------------------------------------------------------------------

# specify order factors will appear
diet_sum$Method <- factor(diet_sum$Method,
                          levels = c("fDNA_prop_occ", "mean_rra"))
diet_sum$type <- factor(diet_sum$type,
                        levels = c("Adult_INC", "Adult_CHR", "Chick"))
diet_sum$Prey <- factor(diet_sum$Prey,
                        levels = c("Copepod", "Jellyfish", "Invert",
                                   "Pollock", "Tomcod", "Herring", "Capelin",
                                   "Prowfish", "Greenling", "Fish"))

# plot
barplots <- ggplot(data = diet_sum, aes(y = Proportion, x = Method, fill = Prey, pattern = Prey)) +
  geom_bar(position = "fill", stat = "identity", width = 0.9) +
  scale_fill_manual(name = "Prey", values = combined_palette) +
  ylab("Proportion of diet") +
  scale_x_discrete(labels=c("FOO", "RRA")) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_classic(base_size = 10) +
  facet_wrap(~ type, labeller = labeller(type = c("Adult_INC" = "Incubating adults\nn = 38",
                                                  "Adult_CHR" = "Chick-rearing adults\nn = 87",
                                                  "Chick" = "Chicks\nn = 26"))) +
  theme(legend.key.size = unit(1, "line"),
        legend.location = "plot",
        legend.margin = margin(0,0,0,0),
        strip.clip = "off",
        strip.background = element_rect(linewidth = 0.5))

ggsave("barplots_tupu_stage_age.png", plot = barplots,
       width = 119, height = 89, units = "mm", dpi = 300)
# -----------------------------------------------------------------------------
