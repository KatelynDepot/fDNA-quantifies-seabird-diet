
# SETUP -----------------------------------------------------------------------
# setwd("..")
library(ggplot2)

#------------------------------------------------------------------------------
# STAGE/AGE PERMANOVA & NMDS
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
                .keep = "unused") |>
  dplyr::select(-c(index:Stage))

stage_rra <- combined_rra |>
  dplyr::filter(type %in% c("Adult_INC", "Adult_CHR"))
age_rra <- combined_rra |>
  dplyr::filter(type %in% c("Adult_CHR", "Chick"))

# PERMANOVA -------------------------------------------------------------------

# stage rra
stage_rra_m <- stage_rra[,2:ncol(stage_rra)] |> as.matrix()
set.seed(123)
vegan::adonis2(stage_rra_m ~ type, data = stage_rra, 
               permutations = 999, method = "bray")

# age rra
age_rra_m <- age_rra[,2:ncol(age_rra)] |> as.matrix()
set.seed(123)
vegan::adonis2(age_rra_m ~ type, data = age_rra, 
               permutations = 999, method = "bray")

# SIMPER (but documentation cautions against using this)
sim_stage <- vegan::simper(stage_rra_m, group = stage_rra$type)
summary(sim_stage) # jellyfish, copepod, and prowfish
sim_age <- vegan::simper(age_rra_m, group = age_rra$type)
summary(sim_age) # gadus

# NMDS ------------------------------------------------------------------------

# make community matrix
com_rra <- combined_rra[,2:ncol(combined_rra)] |> as.matrix()

# run NMDS
set.seed(123)
nmds_rra <- vegan::metaMDS(com_rra, distance = "bray")
nmds_rra # stress 0.14

# extract scores (x and y coords)
site.scores_rra <- as.data.frame(vegan::scores(nmds_rra)$sites)
species.scores_rra <- as.data.frame(vegan::scores(nmds_rra)$species)

# add columns back to data frame 
site.scores_rra$type <- combined_rra$type

# specify order factors will appear
site.scores_rra$type <- factor(site.scores_rra$type,
                           levels = c("Adult_INC", "Adult_CHR", "Chick"))

# plot
nmds_rra <- ggplot() + 
  geom_point(data = site.scores_rra, 
             aes(x = NMDS1, y = NMDS2, color = type), 
             position = position_jitter(0.1, 0.1, seed = 1), size = 2, alpha = 0.3) + 
  geom_point(data = species.scores_rra, aes(x = NMDS1, y = NMDS2), alpha = 0) +
  scale_color_manual(values = c("red", "blue", "green")) +
  scale_x_continuous(breaks = c(-2,-1,0,1,2)) +
  scale_y_continuous(breaks = c(-2,-1,0,1,2)) +
  stat_ellipse(data = site.scores_rra, aes(x = NMDS1, y = NMDS2, color = type), alpha = 0.5) +
  ggrepel::geom_text_repel(data = species.scores_rra, aes(x = NMDS1, y = NMDS2, 
                                                      label = row.names(species.scores_rra)), size = 3.5) +
  theme_classic(base_size = 10) + 
  guides(x.sec = "axis", y.sec = "axis") +
  theme(legend.position = "none",
        axis.ticks.x.top = element_blank(), axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(), axis.text.y.right = element_blank())

ggsave("nmds_plots_tupu_stage_age.png", plot = nmds_rra,
       width = 89, height = 89, units = "mm", dpi = 300)
#------------------------------------------------------------------------------
