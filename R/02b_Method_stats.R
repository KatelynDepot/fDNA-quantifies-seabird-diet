
# SETUP -----------------------------------------------------------------------
# setwd("..")
library(ggplot2)

#------------------------------------------------------------------------------
# BLKI
#------------------------------------------------------------------------------

link_ids <- read.csv('raw_data/BLKI_deployments.csv') |>
  dplyr::select(dep_id, Fecal.Sample.ID, Gurge.Sample.ID)

gurge_rel <- read.csv('processed_data/Gurge_rel_fish.csv') |>
  dplyr::left_join(link_ids)
blki_rra <- read.csv('processed_data/BLKI_rra_fish.csv') |>
  dplyr::left_join(link_ids, dplyr::join_by(dep_id, index == Fecal.Sample.ID)) |>
  dplyr::rename(Fecal.Sample.ID = index)

# make same prey categories
gurge_rel <- gurge_rel |>
  dplyr::mutate(Gadid = Pollock,
                Salmon = Salmon + Sockeye_salmon + Chum_salmon + Pink_salmon,
                Herring = Herring + Herring0,
                Sandlance = Sand_lance,
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
                Sandlance = Sand_lance,
                .keep = "unused")

# remove individuals that don't have both Methods and combine
gurge_rel <- gurge_rel |>
  dplyr::filter(Fecal.Sample.ID %in% blki_rra$Fecal.Sample.ID) |>
  dplyr::mutate(Method = "Regurgitates") |>
  dplyr::select(dep_id, Method, Herring, Capelin, Myctophid, Eulachon,
                Sandlance, Gadid, Lingcod, Greenling, Salmon,
                Sablefish, Stichaeid, Fish)
blki_rra <- blki_rra |>
  dplyr::filter(dep_id %in% gurge_rel$dep_id) |>
  dplyr::mutate(Method = "Feces") |>
  dplyr::select(dep_id, Method, Herring, Capelin, Myctophid, Eulachon,
                Sandlance, Gadid, Lingcod, Greenling, Salmon,
                Sablefish, Stichaeid, Fish)
blki_combined_rra <- rbind(gurge_rel, blki_rra) |>
  # make sure rows sum to 1
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# convert to FOO
blki_combined_foo <- blki_combined_rra |>
  dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# PERMANOVA -------------------------------------------------------------------

# RRA
blki_com_rra <- blki_combined_rra[,3:ncol(blki_combined_rra)] |> as.matrix()
h1_rra <- with(blki_combined_rra, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
vegan::adonis2(blki_com_rra ~ Method, data = blki_combined_rra, 
               permutations = h1_rra, method = "bray")

# FOO
blki_com_foo <- blki_combined_foo[,3:ncol(blki_combined_foo)] |> as.matrix()
h1_foo <- with(blki_combined_foo, permute::how(nperm = 999, blocks = dep_id))
set.seed(123)
vegan::adonis2(blki_com_foo ~ Method, data = blki_combined_foo, 
               permutations = h1_foo, method = "bray")

# Significant result means either centroid or dispersion is different.

# PERMDISP --------------------------------------------------------------------

blki_dist_rra <- vegan::vegdist(blki_com_rra, method = "bray")
blki_beta_rra <- vegan::betadisper(d = blki_dist_rra, group = blki_combined_rra$Method,
                              type = "centroid")
set.seed(123)
vegan::permutest(blki_beta_rra, permutations = h1_rra)

blki_dist_foo <- vegan::vegdist(blki_com_foo, method = "bray")
blki_beta_foo <- vegan::betadisper(d = blki_dist_foo, group = blki_combined_foo$Method,
                              type = "centroid")
set.seed(123)
vegan::permutest(blki_beta_foo, permutations = h1_foo)

# Paired (stratified) Bray-Curtis dissimilarities -----------------------------

# matrix of indices to extract from dissimilarity matrix
pairs <- blki_combined_rra |> # combined_foo indices are identical
  dplyr::mutate(rn = dplyr::row_number()) |>
  dplyr::group_by(dep_id) |>
  dplyr::summarize(pair1 = rn[[1]],
                   pair2 = rn[[2]]) |>
  dplyr::select(-dep_id) |> as.matrix()

# extract dissimilarities
pair_dists_rra <- as.matrix(blki_dist_rra)[pairs]
pair_dists_foo <- as.matrix(blki_dist_foo)[pairs]
pair_dists <- data.frame(method = c(rep("RRA", 71), rep("FOO", 71)),
                         dist = c(pair_dists_rra, pair_dists_foo))

# plot dissimilarities
dist_plots <- ggplot(pair_dists, aes(y = dist, x = method)) +
  geom_violin(trim = FALSE) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.02, dotsize = 1) +
  scale_x_discrete(labels = c("FOO" = "Frequency of\noccurrence",
                              "RRA" = "Relative biomass /\nread abundance")) +
  ylab("Bray-Curtis dissimilarity") + xlab("Method") +
  theme_classic(base_size = 10)

ggsave("dist_plots_blki.png", plot = dist_plots,
       width = 89, height = 89, units = "mm", dpi = 300)

# RV coefficients -------------------------------------------------------------

matrix1_rra <- blki_combined_rra[c(pairs[,1]),3:ncol(blki_combined_rra)] |> as.matrix()
matrix2_rra <- blki_combined_rra[c(pairs[,2]),3:ncol(blki_combined_rra)] |> as.matrix()
FactoMineR::coeffRV(matrix1_rra, matrix2_rra)

matrix1_foo <- blki_combined_foo[c(pairs[,1]),3:ncol(blki_combined_foo)] |> as.matrix()
matrix2_foo <- blki_combined_foo[c(pairs[,2]),3:ncol(blki_combined_foo)] |> as.matrix()
FactoMineR::coeffRV(matrix1_foo, matrix2_foo)

# NMDS ------------------------------------------------------------------------

# run NMDS
set.seed(123)
blki_nmds_rra <- vegan::metaMDS(blki_com_rra, distance = "bray")
blki_nmds_rra # stress 0.098

set.seed(123)
blki_nmds_foo <- vegan::metaMDS(blki_com_foo, distance = "bray")
blki_nmds_foo # stress 0.092

# extract scores (x and y coords)
blki_site.scores_rra <- as.data.frame(vegan::scores(blki_nmds_rra)$sites)
blki_species.scores_rra <- as.data.frame(vegan::scores(blki_nmds_rra)$species)

blki_site.scores_foo <- as.data.frame(vegan::scores(blki_nmds_foo)$sites)
blki_species.scores_foo <- as.data.frame(vegan::scores(blki_nmds_foo)$species)

# add columns back to data frame
blki_site.scores_rra$dep_id <- blki_combined_rra$dep_id
blki_site.scores_foo$dep_id <- blki_combined_rra$dep_id
blki_site.scores_rra$Method <- blki_combined_rra$Method
blki_site.scores_foo$Method <- blki_combined_rra$Method
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# TUPU
#------------------------------------------------------------------------------

# FECAL DNA DATA --------------------------------------------------------------

# group together prey that don't make up >5% by any method in any group
tupu_rra <- read.csv('processed_data/TUPU_rra_fish.csv') |>
  dplyr::mutate(Sculpin = Sculpin + Buffalo_sculpin + Grunt_sculpin +
                  Sailfin_sculpin + Longfin_sculpin + Sharpnose_sculpin,
                Lanternfish = Stenobrachius + Lampanyctus,
                Fish = Sablefish + Red_Irish_lord + Sandfish + Medusafish,
                .keep = "unused") |>
  #### further grouping to match cam data ####
dplyr::mutate(Gadid = Gadid + Gadus,
              Fish = Lanternfish + Rockfish + Sculpin + Fish,
              .keep = "unused") |>
  dplyr::mutate(Method = "Feces") |>
  dplyr::select(type, Method, Capelin, Gadid, Greenling, Herring,
                Prowfish, Sandlance = Sand_lance, Fish) |>
  dplyr::filter(type == "Chick") # chicks only for comparison to cam data

# CAMERA DATA -----------------------------------------------------------------

# Change single obs of Herring1 and all Herring0 to Herring
# Change Sablefish and Sandfish to Other_fish
cam_rel <- read.csv('processed_data/Cam_rel_fish.csv') |>
  dplyr::mutate(Herring = Herring0 + Herring1,
                Fish = Sablefish + Sandfish,
                .keep = "unused") |>
  dplyr::mutate(type = "Chick",
                Method = "Camera") |>
  dplyr::select(type, Method, Capelin, Gadid, Greenling, Herring,
                Prowfish, Sandlance, Fish)

# COMBINED --------------------------------------------------------------------

tupu_combined_rra <- rbind(tupu_rra, cam_rel)

tupu_combined_foo <- tupu_combined_rra |>
  dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# PERMANOVA -------------------------------------------------------------------

# RRA
tupu_com_rra <- tupu_combined_rra[,3:ncol(tupu_combined_rra)] |> as.matrix()
set.seed(123)
vegan::adonis2(tupu_com_rra ~ Method, data = tupu_combined_rra, 
               permutations = 999, method = "bray")

# FOO
tupu_com_foo <- tupu_combined_foo[,3:ncol(tupu_combined_foo)] |> as.matrix()
set.seed(123)
vegan::adonis2(tupu_com_foo ~ Method, data = tupu_combined_foo, 
               permutations = 999, method = "bray")

# PERMDISP --------------------------------------------------------------------

tupu_dist_rra <- vegan::vegdist(tupu_com_rra, method = "bray")
tupu_beta_rra <- vegan::betadisper(d = tupu_dist_rra, group = tupu_combined_rra$Method,
                                   type = "centroid")
set.seed(123)
vegan::permutest(tupu_beta_rra, permutations = 999)

tupu_dist_foo <- vegan::vegdist(tupu_com_foo, method = "bray")
tupu_beta_foo <- vegan::betadisper(d = tupu_dist_foo, group = tupu_combined_foo$Method,
                                   type = "centroid")
set.seed(123)
vegan::permutest(tupu_beta_foo, permutations = 999)

# NMDS ------------------------------------------------------------------------

# run NMDS
set.seed(123)
tupu_nmds_rra <- vegan::metaMDS(tupu_com_rra, distance = "bray")
tupu_nmds_rra # stress 0.043

set.seed(123)
tupu_nmds_foo <- vegan::metaMDS(tupu_com_foo, distance = "bray")
tupu_nmds_foo # stress 0.029

# extract scores (x and y coords)
tupu_site.scores_rra <- as.data.frame(vegan::scores(tupu_nmds_rra)$sites)
tupu_species.scores_rra <- as.data.frame(vegan::scores(tupu_nmds_rra)$species)

tupu_site.scores_foo <- as.data.frame(vegan::scores(tupu_nmds_foo)$sites)
tupu_species.scores_foo <- as.data.frame(vegan::scores(tupu_nmds_foo)$species)

# add columns back to data frame 
tupu_site.scores_rra$Method <- tupu_combined_rra$Method
tupu_site.scores_foo$Method <- tupu_combined_rra$Method
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# COMBINED NMDS PLOTS WITH FACET GRID
# -----------------------------------------------------------------------------

# species scores
blki_species.scores_foo <- blki_species.scores_foo |>
  dplyr::mutate(foo_rra = "foo")
blki_species.scores_foo$Prey <- row.names(blki_species.scores_foo)
rownames(blki_species.scores_foo) <- NULL

blki_species.scores_rra <- blki_species.scores_rra |>
  dplyr::mutate(foo_rra = "rra")
blki_species.scores_rra$Prey <- row.names(blki_species.scores_rra)
rownames(blki_species.scores_rra) <- NULL

tupu_species.scores_foo <- tupu_species.scores_foo |> 
  dplyr::mutate(foo_rra = "foo")
tupu_species.scores_foo$Prey <- row.names(tupu_species.scores_foo)
rownames(tupu_species.scores_foo) <- NULL

tupu_species.scores_rra <- tupu_species.scores_rra |>
  dplyr::mutate(foo_rra = "rra")
tupu_species.scores_rra$Prey <- row.names(tupu_species.scores_rra)
rownames(tupu_species.scores_rra) <- NULL

blki_species.scores <- rbind(blki_species.scores_foo, blki_species.scores_rra) |>
  dplyr::mutate(species = "BLKI")
tupu_species.scores <- rbind(tupu_species.scores_foo, tupu_species.scores_rra) |>
  dplyr::mutate(species = "TUPU")
species.scores <- rbind(blki_species.scores, tupu_species.scores)

# site scores
blki_site.scores_foo <- blki_site.scores_foo |>
  dplyr::mutate(foo_rra = "foo")
blki_site.scores_rra <- blki_site.scores_rra |>
  dplyr::mutate(foo_rra = "rra")

tupu_site.scores_foo <- tupu_site.scores_foo |>
  dplyr::mutate(foo_rra = "foo")
tupu_site.scores_rra <- tupu_site.scores_rra |>
  dplyr::mutate(foo_rra = "rra")

blki_site.scores <- rbind(blki_site.scores_foo, blki_site.scores_rra) |>
  dplyr::mutate(species = "BLKI")
tupu_site.scores <- rbind(tupu_site.scores_foo, tupu_site.scores_rra) |>
  dplyr::mutate(species = "TUPU") |>
  dplyr::mutate(dep_id = NA, .after = NMDS2)

site.scores <- rbind(blki_site.scores, tupu_site.scores)

# plot
combined_plots <- ggplot() + 
  geom_point(data = subset(site.scores, site.scores$species == "TUPU"), 
             aes(x = NMDS1, y = NMDS2, color = Method, group = dep_id), 
             position = position_jitter(0.01, 0.01, seed = 1),
             size = 2, alpha = 0.3) + 
  geom_point(data = subset(site.scores, site.scores$species == "BLKI"), 
             aes(x = NMDS1, y = NMDS2, color = Method, group = dep_id), 
             position = position_jitter(0.1, 0.1, seed = 1),
             size = 2, alpha = 0.3) + 
  geom_path(data = subset(site.scores, site.scores$species == "BLKI"), aes(x = NMDS1, y = NMDS2, group = dep_id), 
            position = position_jitter(0.1, 0.1, seed = 1), alpha = 0.2) +
  geom_point(data = species.scores, aes(x = NMDS1, y = NMDS2), alpha = 0) +
  scale_color_manual(values = c("Regurgitates" = "red",
                                "Camera" = "red",
                                "Feces" = "blue")) +
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
  stat_ellipse(data = site.scores, aes(x = NMDS1, y = NMDS2, color = Method), alpha = 0.5) +
  ggrepel::geom_text_repel(data = species.scores, aes(x = NMDS1, y = NMDS2, 
                                                      label = Prey), size = 3.5) +
  theme_bw(base_size = 10) + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 10)) +
  ggh4x::facet_grid2(rows = vars(species), cols = vars(foo_rra), 
             labeller = labeller(species = c('BLKI' = "Black-legged kittiwake",
                                            'TUPU' = "Tufted puffin"),
                                 foo_rra = c('foo' = "Frequency of occurrence",
                                             'rra' = "Relative biomass / read abundance")),
             scales = "free", independent = "all")

ggsave("nmds_plots_method.png", plot = combined_plots,
       width = 180, height = 150, units = "mm", dpi = 300)
#------------------------------------------------------------------------------
