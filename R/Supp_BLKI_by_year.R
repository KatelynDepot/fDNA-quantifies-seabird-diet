
#------------------------------------------------------------------------------
# BLKI BY YEAR
#------------------------------------------------------------------------------
# setwd("..")
library(ggplot2)
library(shadowtext)

# data prep
link_ids <- read.csv('raw_data/BLKI_deployments.csv') |>
  dplyr::mutate(Year = as.factor(lubridate::year(time_recaptured)), .keep = "unused")

gurge_rel <- read.csv('processed_data/Gurge_rel_fish.csv') |>
  dplyr::left_join(link_ids)
blki_rra <- read.csv('processed_data/BLKI_rra_fish.csv') |>
  dplyr::left_join(link_ids)

# make same prey categories
gurge_rel <- gurge_rel |>
  dplyr::mutate(Gadid = Pollock,
                Salmon = Salmon + Sockeye_salmon + Chum_salmon + Pink_salmon,
                Herring = Herring + Herring0,
                .keep = "unused")
blki_rra <- blki_rra |>
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
                .keep = "unused")

# remove individuals that don't have both Methods and combine
gurge_rel <- gurge_rel |>
  dplyr::filter(Fecal.Sample.ID %in% blki_rra$Fecal.Sample.ID) |>
  dplyr::mutate(Method = "Regurgitates") |>
  dplyr::select(dep_id, Year, Method, Herring, Capelin, Myctophid, Eulachon,
                Sand_lance, Gadid, Lingcod, Greenling, Salmon,
                Sablefish, Stichaeid, Fish)
blki_rra <- blki_rra |>
  dplyr::filter(dep_id %in% gurge_rel$dep_id) |>
  dplyr::mutate(Method = "Feces") |>
  dplyr::select(dep_id, Year, Method, Herring, Capelin, Myctophid, Eulachon,
                Sand_lance, Gadid, Lingcod, Greenling, Salmon,
                Sablefish, Stichaeid, Fish)
blki_combined_rra <- rbind(gurge_rel, blki_rra) |>
  # make sure rows sum to 1
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# convert to FOO
blki_combined_foo <- blki_combined_rra |>
  dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# by year
blki_combined_rra_23 <- blki_combined_rra |> dplyr::filter(Year == "2023")
blki_combined_rra_24 <- blki_combined_rra |> dplyr::filter(Year == "2024")
blki_combined_foo_23 <- blki_combined_foo |> dplyr::filter(Year == "2023")
blki_combined_foo_24 <- blki_combined_foo |> dplyr::filter(Year == "2024")

# PERMANOVA & PERMDISP --------------------------------------------------------

# PERMANOVA
# Significant result means either centroid or dispersion is different.

blki_com_rra_23 <- blki_combined_rra_23[,4:ncol(blki_combined_rra_23)] |> as.matrix()
h1_rra_23 <- with(blki_combined_rra_23, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
blki_perm_rra_23 <- vegan::adonis2(blki_com_rra_23 ~ Method, data = blki_combined_rra_23, 
                                permutations = h1_rra_23, method = "bray")

blki_com_rra_24 <- blki_combined_rra_24[,4:ncol(blki_combined_rra_24)] |> as.matrix()
h1_rra_24 <- with(blki_combined_rra_24, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
blki_perm_rra_24 <- vegan::adonis2(blki_com_rra_24 ~ Method, data = blki_combined_rra_24, 
                                   permutations = h1_rra_24, method = "bray")

blki_com_foo_23 <- blki_combined_foo_23[,4:ncol(blki_combined_foo_23)] |> as.matrix()
h1_foo_23 <- with(blki_combined_foo_23, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
blki_perm_foo_23 <- vegan::adonis2(blki_com_foo_23 ~ Method, data = blki_combined_foo_23, 
                                   permutations = h1_foo_23, method = "bray")

blki_com_foo_24 <- blki_combined_foo_24[,4:ncol(blki_combined_foo_24)] |> as.matrix()
h1_foo_24 <- with(blki_combined_foo_24, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
blki_perm_foo_24 <- vegan::adonis2(blki_com_foo_24 ~ Method, data = blki_combined_foo_24, 
                                   permutations = h1_foo_24, method = "bray")

# PERMDISP

blki_dist_rra_23 <- vegan::vegdist(blki_com_rra_23, method = "bray")
blki_beta_rra_23 <- vegan::betadisper(d = blki_dist_rra_23, group = blki_combined_rra_23$Method,
                              type = "centroid")
set.seed(123)
blki_permd_rra_23 <- vegan::permutest(blki_beta_rra_23, permutations = h1_rra_23)

blki_dist_rra_24 <- vegan::vegdist(blki_com_rra_24, method = "bray")
blki_beta_rra_24 <- vegan::betadisper(d = blki_dist_rra_24, group = blki_combined_rra_24$Method,
                                      type = "centroid")
set.seed(123)
blki_permd_rra_24 <- vegan::permutest(blki_beta_rra_24, permutations = h1_rra_24)

blki_dist_foo_23 <- vegan::vegdist(blki_com_foo_23, method = "bray")
blki_beta_foo_23 <- vegan::betadisper(d = blki_dist_foo_23, group = blki_combined_foo_23$Method,
                              type = "centroid")
set.seed(123)
blki_permd_foo_23 <- vegan::permutest(blki_beta_foo_23, permutations = h1_foo_23)

blki_dist_foo_24 <- vegan::vegdist(blki_com_foo_24, method = "bray")
blki_beta_foo_24 <- vegan::betadisper(d = blki_dist_foo_24, group = blki_combined_foo_24$Method,
                                      type = "centroid")
set.seed(123)
blki_permd_foo_24 <- vegan::permutest(blki_beta_foo_24, permutations = h1_foo_24)

# Summary table
blki_perm_df <- data.frame(Year = c("2023","2023","2024","2024"),
                           Metric = c("FOO","RRA","FOO","RRA"),
                           df = c(rep(paste0(blki_perm_foo_23$Df[1],", ",blki_perm_foo_23$Df[2]),2),
                                  rep(paste0(blki_perm_foo_24$Df[1],", ",blki_perm_foo_24$Df[2]),2)),
                           PERMANOVA_R2 = c(blki_perm_foo_23$R2[1],blki_perm_rra_23$R2[1],
                                            blki_perm_foo_24$R2[1],blki_perm_rra_24$R2[1]),
                           PERMANOVA_PseudoF = c(blki_perm_foo_23$F[1],blki_perm_rra_23$F[1],
                                                 blki_perm_foo_24$F[1],blki_perm_rra_24$F[1]),
                           PERMANOVA_p = c(blki_perm_foo_23$`Pr(>F)`[1],blki_perm_rra_23$`Pr(>F)`[1],
                                           blki_perm_foo_24$`Pr(>F)`[1],blki_perm_rra_24$`Pr(>F)`[1]),
                           PERMDISP_PseudoF = c(blki_permd_foo_23$tab$F[1],blki_permd_rra_23$tab$F[1],
                                                blki_permd_foo_24$tab$F[1],blki_permd_rra_24$tab$F[1]),
                           PERMDISP_p = c(blki_permd_foo_23$tab$`Pr(>F)`[1],blki_permd_rra_23$tab$`Pr(>F)`[1],
                                          blki_permd_foo_24$tab$`Pr(>F)`[1],blki_permd_rra_24$tab$`Pr(>F)`[1])) |>
  dplyr::mutate(dplyr::across(PERMANOVA_R2:PERMDISP_p, round, 3))

# NMDS ------------------------------------------------------------------------

# run NMDS
set.seed(123)
blki_nmds_rra_23 <- vegan::metaMDS(blki_com_rra_23, distance = "bray")
blki_nmds_rra_23 # stress 0.073

set.seed(123)
blki_nmds_rra_24 <- vegan::metaMDS(blki_com_rra_24, distance = "bray")
blki_nmds_rra_24 # stress 0.107

set.seed(123)
blki_nmds_foo_23 <- vegan::metaMDS(blki_com_foo_23, distance = "bray")
blki_nmds_foo_23 # stress 0.072

set.seed(123)
blki_nmds_foo_24 <- vegan::metaMDS(blki_com_foo_24, distance = "bray")
blki_nmds_foo_24 # stress 0.099

# extract scores (x and y coords)
blki_site.scores_rra_23 <- as.data.frame(vegan::scores(blki_nmds_rra_23)$sites)
blki_species.scores_rra_23 <- as.data.frame(vegan::scores(blki_nmds_rra_23)$species)

blki_site.scores_rra_24 <- as.data.frame(vegan::scores(blki_nmds_rra_24)$sites)
blki_species.scores_rra_24 <- as.data.frame(vegan::scores(blki_nmds_rra_24)$species)

blki_site.scores_foo_23 <- as.data.frame(vegan::scores(blki_nmds_foo_23)$sites)
blki_species.scores_foo_23 <- as.data.frame(vegan::scores(blki_nmds_foo_23)$species)

blki_site.scores_foo_24 <- as.data.frame(vegan::scores(blki_nmds_foo_24)$sites)
blki_species.scores_foo_24 <- as.data.frame(vegan::scores(blki_nmds_foo_24)$species)

# add columns back to data frame
# site scores
blki_site.scores_rra_23$dep_id <- blki_combined_rra_23$dep_id
blki_site.scores_rra_23$Method <- blki_combined_rra_23$Method
blki_site.scores_rra_23$Year <- "2023"
blki_site.scores_rra_23$foo_rra <- "rra"

blki_site.scores_foo_23$dep_id <- blki_combined_foo_23$dep_id
blki_site.scores_foo_23$Method <- blki_combined_foo_23$Method
blki_site.scores_foo_23$Year <- "2023"
blki_site.scores_foo_23$foo_rra <- "foo"

blki_site.scores_rra_24$dep_id <- blki_combined_rra_24$dep_id
blki_site.scores_rra_24$Method <- blki_combined_rra_24$Method
blki_site.scores_rra_24$Year <- "2024"
blki_site.scores_rra_24$foo_rra <- "rra"

blki_site.scores_foo_24$dep_id <- blki_combined_foo_24$dep_id
blki_site.scores_foo_24$Method <- blki_combined_foo_24$Method
blki_site.scores_foo_24$Year <- "2024"
blki_site.scores_foo_24$foo_rra <- "foo"

site.scores <- rbind(blki_site.scores_foo_23, blki_site.scores_rra_23,
                     blki_site.scores_foo_24, blki_site.scores_rra_24)

# species scores
blki_species.scores_foo_23$foo_rra <- "foo"
blki_species.scores_foo_23$Year <- "2023"
blki_species.scores_foo_23$Prey <- row.names(blki_species.scores_foo_23)
rownames(blki_species.scores_foo_23) <- NULL

blki_species.scores_rra_23$foo_rra <- "rra"
blki_species.scores_rra_23$Year <- "2023"
blki_species.scores_rra_23$Prey <- row.names(blki_species.scores_rra_23)
rownames(blki_species.scores_rra_23) <- NULL

blki_species.scores_foo_24$foo_rra <- "foo"
blki_species.scores_foo_24$Year <- "2024"
blki_species.scores_foo_24$Prey <- row.names(blki_species.scores_foo_24)
rownames(blki_species.scores_foo_24) <- NULL

blki_species.scores_rra_24$foo_rra <- "rra"
blki_species.scores_rra_24$Year <- "2024"
blki_species.scores_rra_24$Prey <- row.names(blki_species.scores_rra_24)
rownames(blki_species.scores_rra_24) <- NULL

species.scores <- rbind(blki_species.scores_foo_23, blki_species.scores_rra_23,
                        blki_species.scores_foo_24, blki_species.scores_rra_24)

# plot
combined_plots <- ggplot() + 
  geom_point(data = subset(site.scores, site.scores$Year == "2023"), 
             aes(x = NMDS1, y = NMDS2, color = Method, group = dep_id), 
             position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.05, seed = 1),
             size = 2, alpha = 0.3) + 
  geom_point(data = subset(site.scores, site.scores$Year == "2024"), 
             aes(x = NMDS1, y = NMDS2, color = Method, group = dep_id), 
             position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.05, seed = 1),
             size = 2, alpha = 0.3) + 
  geom_path(data = subset(site.scores, site.scores$Year == "2023"), aes(x = NMDS1, y = NMDS2, group = dep_id), 
            position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.05, seed = 1), alpha = 0.2) +
  geom_path(data = subset(site.scores, site.scores$Year == "2024"), aes(x = NMDS1, y = NMDS2, group = dep_id), 
            position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.05, seed = 1), alpha = 0.2) +
  geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2), alpha = 0) +
  scale_color_manual(values = c("Regurgitates" = "red", "Camera" = "red",
                                "Feces" = "blue")) +
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  stat_ellipse(data = site.scores, aes(x = NMDS1, y = NMDS2, color = Method), 
               type = "norm", alpha = 0.5) +
  ggrepel::geom_text_repel(data = species.scores, aes(x = NMDS1, y = NMDS2, 
                                                      label = Prey), size = 3.5,
                           color = "black", bg.color = "white") +
  theme_bw(base_size = 10) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 10)) +
  ggh4x::facet_grid2(rows = vars(Year), cols = vars(foo_rra), 
             labeller = labeller(foo_rra = c('foo' = "Frequency of occurrence",
                                             'rra' = "Relative biomass / read abundance")),
             scales = "free", independent = "all")

ggsave("nmds_plots_blki_year.png", plot = combined_plots,
       width = 180, height = 150, units = "mm", dpi = 300)
#------------------------------------------------------------------------------
