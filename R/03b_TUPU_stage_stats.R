
# SETUP -----------------------------------------------------------------------
# setwd("..")
library(ggplot2)
library(shadowtext)

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
  dplyr::select(-Fecal.Sample.ID)

stage_rra <- combined_rra |>
  dplyr::filter(type %in% c("Adult_INC", "Adult_CHR"))
age_rra <- combined_rra |>
  dplyr::filter(type %in% c("Adult_CHR", "Chick"))

# PERMANOVA & PERMDISP --------------------------------------------------------

# PERMANOVA

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

# PERMDISP

# stage rra
dist_stage_rra <- vegan::vegdist(stage_rra_m, method = "bray")
beta_stage_rra <- vegan::betadisper(d = dist_stage_rra, group = stage_rra$type,
                                    type = "centroid")
set.seed(123)
permd_stage_rra <- vegan::permutest(beta_stage_rra, permutations = 999)

# age rra
dist_age_rra <- vegan::vegdist(age_rra_m, method = "bray")
beta_age_rra <- vegan::betadisper(d = dist_age_rra, group = age_rra$type,
                                    type = "centroid")
set.seed(123)
permd_age_rra <- vegan::permutest(beta_age_rra, permutations = 999)

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
species.scores_rra <- species.scores_rra |>
  dplyr::mutate(Label = stringr::str_replace_all(row.names(species.scores_rra), 
                                                 c("Fish" = "Other fish")))

# add columns back to data frame 
site.scores_rra$type <- combined_rra$type

# specify order factors will appear
site.scores_rra$type <- factor(site.scores_rra$type,
                           levels = c("Adult_INC", "Adult_CHR", "Chick"))

# plot
nmds_rra <- ggplot() + 
  geom_point(data = site.scores_rra, 
             aes(x = NMDS1, y = NMDS2, color = type), 
             position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.05, seed = 1),
             size = 2, alpha = 0.3) + 
  geom_point(data = species.scores_rra, aes(x = NMDS1, y = NMDS2), alpha = 0) +
  scale_color_manual(values = c("red", "blue", "green")) +
  scale_x_continuous(breaks = c(-2,-1,0,1,2)) +
  scale_y_continuous(breaks = c(-2,-1,0,1,2)) +
  stat_ellipse(data = site.scores_rra, aes(x = NMDS1, y = NMDS2, color = type), 
               type = "norm", alpha = 0.5) +
  ggrepel::geom_text_repel(data = species.scores_rra, aes(x = NMDS1, y = NMDS2, 
                                                      label = Label), size = 3.5,
                           color = "black", bg.color = "white") +
  theme_classic(base_size = 10) + 
  guides(x.sec = "axis", y.sec = "axis") +
  theme(legend.position = "none",
        axis.ticks.x.top = element_blank(), axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(), axis.text.y.right = element_blank())

ggsave("nmds_plots_tupu_stage_age.png", plot = nmds_rra,
       width = 89, height = 89, units = "mm", dpi = 300)
#------------------------------------------------------------------------------
