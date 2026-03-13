
# SETUP -----------------------------------------------------------------------
# setwd("..")
library(ggplot2)
source("R/my_palette.R")

#------------------------------------------------------------------------------
# BLKI
#------------------------------------------------------------------------------

gurge_rel <- read.csv('processed_data/Gurge_rel_fish.csv')
rra <- read.csv('processed_data/BLKI_rra_fish.csv') |>
  dplyr::select(-dep_id)

# need to have same prey categories for gurge and fDNA
colnames(gurge_rel)
colnames(rra)

gurge_rel <- gurge_rel |>
  dplyr::mutate(Gadid = Pollock,
                Salmon = Salmon + Sockeye_salmon + Chum_salmon + Pink_salmon,
                Herring = Herring + Herring0,
                Sandlance = Sand_lance,
                .keep = "unused")
rra <- rra |>
  dplyr::mutate(Myctophid = California_headlightfish + Stenobrachius + Lampanyctus,
                Stichaeid = Daubed_shanny,
                Gadid = Gadid + Gadus,
                Fish =
                  Red_Irish_lord + Prowfish + Sandfish + Crested_bigscale +
                  Searcher + Scaly_paperbone + Eelpout + Scalyhead_sculpin +
                  Cockscomb + Lobefin_snailfish + Wrymouth + Rockfish +
                  Arrowtooth_flounder + Buffalo_sculpin + Tidepool_snailfish +
                  Sole + Atlantic_salmon,
                Salmon = Pacific_salmon,
                Sandlance = Sand_lance,
                .keep = "unused")

# foo tables
gurge_foo <- gurge_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
  # 1% threshold to match fDNA filtering
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# intermediate tables
gurge_foo_sum <- gurge_foo |>
  tidyr::pivot_longer(cols = 2:13, names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(gurge_prop_occ = n / 83) |>
  dplyr::select(-n)
gurge_rel_sum <- gurge_rel |>
  tidyr::pivot_longer(cols = 2:13, names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()
foo_sum <- foo |>
  tidyr::pivot_longer(cols = 2:13, names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = n / 178) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = 2:13, names_to = "Prey", values_to = "rra") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rra = mean(rra)) |>
  dplyr::ungroup()

# summary table
gurge_sum <- gurge_foo_sum |>
  dplyr::left_join(gurge_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Regurgitates") |>
  dplyr::rename(Occ = gurge_prop_occ, Rel = mean_rel_mass)
feces_sum <- foo_sum |>
  dplyr::left_join(rra_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Feces") |>
  dplyr::rename(Occ = fDNA_prop_occ, Rel = mean_rra)
diet_sum <- rbind(gurge_sum, feces_sum) |>
  tidyr::pivot_longer(cols = Occ:Rel, names_to = "Method2", values_to = "Proportion")

# specify order factors will appear (ordered by RRA)
diet_sum$Prey <- factor(diet_sum$Prey,
                        levels = c("Herring", "Capelin", "Myctophid", "Eulachon",
                                   "Gadid", "Greenling", "Sandlance", "Salmon",
                                   "Stichaeid", "Sablefish", "Lingcod", "Fish"))
diet_sum$Method1 <- factor(diet_sum$Method1,
                           levels = c("Regurgitates", "Feces"))

# plot
blki_barplots <- ggplot(data = diet_sum, aes(y = Proportion, x = Method2, fill = Prey)) +
  geom_bar(position = "fill", stat = "identity", width = 0.9) +
  scale_fill_manual(name = "Prey", values = combined_palette) +
  ylab("Proportion of diet") + xlab("Method") +
  labs(subtitle = "Black-legged kittiwake") +
  scale_x_discrete(labels = c("FOO", #"RRA\n "
                              # make 2 versions then manually edit
                              "Relative\nbiomass"
  )) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_classic(base_size = 10) +
  facet_wrap(~ Method1,
             labeller = labeller(Method1 = c("Regurgitates" = "Regurgitates\nn = 83",
                                             "Feces" = "Feces\nn = 178"))) +
  theme(legend.key.size = unit(1, "line"),
        legend.location = "plot",
        legend.margin = margin(0,0,0,0),
        strip.clip = "off",
        strip.background = element_rect(linewidth = 0.5))
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# TUPU
#------------------------------------------------------------------------------

# FECAL DNA DATA --------------------------------------------------------------

# group together prey that don't make up >5% by any method in any group
rra <- read.csv('processed_data/TUPU_rra_fish.csv') |>
  dplyr::mutate(Sculpin = Sculpin + Buffalo_sculpin + Grunt_sculpin +
                  Sailfin_sculpin + Longfin_sculpin + Sharpnose_sculpin,
                Lanternfish = Stenobrachius + Lampanyctus,
                Other_fish = Sablefish + Red_Irish_lord + Sandfish + Medusafish,
                .keep = "unused") |>
  #### further grouping to match cam data ####
dplyr::mutate(Gadid = Gadid + Gadus,
              Other_fish = Lanternfish + Rockfish + Sculpin + Other_fish,
              .keep = "unused")

# foo table
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# sample sizes
sample_sizes <- foo |> dplyr::count(type)

# intermediate tables
foo_sum <- foo |>
  tidyr::pivot_longer(cols = Herring:Other_fish, names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = dplyr::case_when(type == "Adult_CHR" ~ n / sample_sizes$n[1],
                                                 type == "Adult_INC" ~ n / sample_sizes$n[2],
                                                 type == "Chick" ~ n / sample_sizes$n[3])) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = Herring:Other_fish, names_to = "Prey", values_to = "rra") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(mean_rra = mean(rra)) |>
  dplyr::ungroup()

# CAMERA DATA -----------------------------------------------------------------

# Change single obs of Herring1 and all Herring0 to Herring
# Change Sablefish and Sandfish to Other_fish
cam_rel <- read.csv('processed_data/Cam_rel_fish.csv') |>
  dplyr::mutate(Herring = Herring0 + Herring1,
                Sand_lance = Sandlance,
                Other_fish = Sablefish + Sandfish,
                .keep = "unused")

cam_foo <- cam_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
  # 1% threshold to match fDNA filtering

# intermediate tables
cam_foo_sum <- cam_foo |>
  tidyr::pivot_longer(cols = 2:8, names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(cam_prop_occ = n / 543) |>
  dplyr::select(-n)
cam_rel_sum <- cam_rel |>
  tidyr::pivot_longer(cols = 2:8, names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()

# COMBINED --------------------------------------------------------------------

# summary table
feces_sum <- foo_sum |>
  dplyr::left_join(rra_sum, dplyr::join_by(Prey, type)) |>
  dplyr::mutate(Method1 = "Feces") |>
  dplyr::relocate(type, .after = Method1) |>
  dplyr::rename(Occ = fDNA_prop_occ, Rel = mean_rra)
cam_sum <- cam_foo_sum |>
  dplyr::left_join(cam_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Cameras",
                type = "Chick") |>
  dplyr::rename(Occ = cam_prop_occ, Rel = mean_rel_mass)
diet_sum = rbind(cam_sum, feces_sum) |>
  tidyr::pivot_longer(cols = Occ:Rel, names_to = "Method2", values_to = "Proportion")

# BARPLOTS --------------------------------------------------------------------

# change some names
diet_sum <- diet_sum |>
  dplyr::mutate(Prey = forcats::fct_recode(Prey,
                                           Sandlance = "Sand_lance",
                                           Fish = "Other_fish"))

# specify order factors will appear  (ordered by RRA)
diet_sum$Prey <- factor(diet_sum$Prey,
                        levels = c("Gadid", "Herring", "Capelin", "Prowfish",
                                   "Greenling", "Sandlance", "Fish"))
diet_sum$Method1 <- factor(diet_sum$Method1,
                        levels = c("Cameras", "Feces"))

# chicks only
diet_sum_chicks <- diet_sum |> dplyr::filter(type == "Chick")

# plot
tupu_barplots <- ggplot(data = diet_sum_chicks, aes(y = Proportion, x = Method2, fill = Prey)) +
  geom_bar(position = "fill", stat = "identity", width = 0.9) +
  scale_fill_manual(name = "Prey", values = combined_palette) +
  ylab("Proportion of diet") + xlab("Method") +
  labs(subtitle = "Tufted puffin") +
  scale_x_discrete(labels = c("FOO", "RRA\n "
                              # make 2 versions then manually edit
                              #"Relative\nbiomass"
  )) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_classic(base_size = 10) +
  facet_wrap(~ Method1,
             labeller = labeller(Method1 = c("Cameras" = "Cameras\nn = 543",
                                             "Feces" = "Feces\nn = 28"))) +
  theme(legend.key.size = unit(1, "line"),
        legend.location = "plot",
        legend.margin = margin(0,0,0,0),
        strip.clip = "off",
        strip.background = element_rect(linewidth = 0.5))
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# combined plots
library(patchwork)

method_barplots = blki_barplots + tupu_barplots +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')

ggsave("method_barplots.png", plot = method_barplots,
       width = 183, height = 89, units = "mm", dpi = 300)
#------------------------------------------------------------------------------
