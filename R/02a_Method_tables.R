
#------------------------------------------------------------------------------
# BLKI - METAZOA
#------------------------------------------------------------------------------

# load data
gurge_rel <- read.csv('processed_data/Gurge_rel.csv')
rra <- read.csv('processed_data/BLKI_rra_metazoa.csv')

# group all fish in gurge to match 18S categories
gurge_rel <- gurge_rel |>
  dplyr::mutate(Fish = Herring + Capelin + Myctophid + Herring0 + Eulachon +
                  Sand_lance + Lingcod + Pollock + Sockeye_salmon + Chum_salmon +
                  Pink_salmon + Salmon + Greenling + Sablefish + Stichaeid + Fish,
                Eucarid = Euphausiid + Shrimp,
                Peracarid = Amphipod,
                .keep = "unused")

# foo tables (1% threshold to match fDNA filtering)
gurge_foo <- gurge_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# intermediate tables
gurge_foo_sum <- gurge_foo |>
  tidyr::pivot_longer(cols = 2:ncol(gurge_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(gurge_prop_occ = n / nrow(gurge_foo)) |>
  dplyr::select(-n)
gurge_rel_sum <- gurge_rel |>
  tidyr::pivot_longer(cols = 2:ncol(gurge_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()
foo_sum <- foo |>
  tidyr::pivot_longer(cols = 2:ncol(foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = n / nrow(foo)) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = 2:ncol(rra), names_to = "Prey", values_to = "rra") |>
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
  tidyr::pivot_wider(names_from = Method1, values_from = c(Occ,Rel),
                     names_glue = "{Method1}_{.value}") |>
  dplyr::select(Prey, Feces_Occ, Feces_Rel, Regurgitates_Occ, Regurgitates_Rel) |>
  dplyr::mutate(dplyr::across(Feces_Occ:Regurgitates_Rel, round, 3))

#------------------------------------------------------------------------------
# BLKI - FISH
#------------------------------------------------------------------------------

# load data
gurge_rel <- read.csv('processed_data/Gurge_rel_fish.csv')
rra <- read.csv('processed_data/BLKI_rra_fish.csv')

# foo tables (1% threshold to match fDNA filtering)
gurge_foo <- gurge_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))

# intermediate tables
gurge_foo_sum <- gurge_foo |>
  tidyr::pivot_longer(cols = 2:ncol(gurge_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(gurge_prop_occ = n / nrow(gurge_foo)) |>
  dplyr::select(-n)
gurge_rel_sum <- gurge_rel |>
  tidyr::pivot_longer(cols = 2:ncol(gurge_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()
foo_sum <- foo |>
  tidyr::pivot_longer(cols = 2:ncol(foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = n / nrow(foo)) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = 2:ncol(rra), names_to = "Prey", values_to = "rra") |>
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
  tidyr::pivot_wider(names_from = Method1, values_from = c(Occ,Rel),
                     names_glue = "{Method1}_{.value}") |>
  dplyr::select(Prey, Feces_Occ, Feces_Rel, Regurgitates_Occ, Regurgitates_Rel) |>
  dplyr::mutate(dplyr::across(Feces_Occ:Regurgitates_Rel, round, 3))

#------------------------------------------------------------------------------
# TUPU - METAZOA - FECES BY STAGE/AGE
#------------------------------------------------------------------------------

# load data
rra <- read.csv('processed_data/TUPU_rra_metazoa.csv')
# convert to foo
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))
# sample sizes by stage/age
sample_sizes <- foo |> dplyr::count(type)

# intermediate tables
foo_sum <- foo |>
  tidyr::pivot_longer(cols = 3:ncol(foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = dplyr::case_when(
    type == "Adult_CHR" ~ n / sample_sizes$n[sample_sizes$type == "Adult_CHR"],
    type == "Adult_INC" ~ n / sample_sizes$n[sample_sizes$type == "Adult_INC"],
    type == "Chick" ~ n / sample_sizes$n[sample_sizes$type == "Chick"])) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = 3:ncol(rra),
                      names_to = "Prey", values_to = "rra") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(mean_rra = mean(rra)) |>
  dplyr::ungroup()

# summary table
diet_sum <- foo_sum |>
  dplyr::left_join(rra_sum, dplyr::join_by(type, Prey)) |>
  dplyr::rename(Occ = fDNA_prop_occ, Rel = mean_rra) |>
  tidyr::pivot_wider(names_from = type, values_from = c(Occ,Rel),
                     names_glue = "{type}_{.value}") |>
  dplyr::select(Prey, Adult_INC_Occ, Adult_INC_Rel, Adult_CHR_Occ, Adult_CHR_Rel,
                Chick_Occ, Chick_Rel) |>
  dplyr::mutate(dplyr::across(Adult_INC_Occ:Chick_Rel, round, 3))

#------------------------------------------------------------------------------
# TUPU - FISH - FECES BY STAGE/AGE
#------------------------------------------------------------------------------

# load data
rra <- read.csv('processed_data/TUPU_rra_fish.csv')
# convert to foo
foo <- rra |> dplyr::mutate_if(is.numeric, ~1 * (. > 0))
# sample sizes by stage/age
sample_sizes <- foo |> dplyr::count(type)

# intermediate tables
foo_sum <- foo |>
  tidyr::pivot_longer(cols = 3:ncol(foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(fDNA_prop_occ = dplyr::case_when(
    type == "Adult_CHR" ~ n / sample_sizes$n[sample_sizes$type == "Adult_CHR"],
    type == "Adult_INC" ~ n / sample_sizes$n[sample_sizes$type == "Adult_INC"],
    type == "Chick" ~ n / sample_sizes$n[sample_sizes$type == "Chick"])) |>
  dplyr::select(-n)
rra_sum <- rra |>
  tidyr::pivot_longer(cols = 3:ncol(rra),
                      names_to = "Prey", values_to = "rra") |>
  dplyr::group_by(type, Prey) |>
  dplyr::summarize(mean_rra = mean(rra)) |>
  dplyr::ungroup()

# summary table
diet_sum <- foo_sum |>
  dplyr::left_join(rra_sum, dplyr::join_by(type, Prey)) |>
  dplyr::rename(Occ = fDNA_prop_occ, Rel = mean_rra) |>
  tidyr::pivot_wider(names_from = type, values_from = c(Occ,Rel),
                     names_glue = "{type}_{.value}") |>
  dplyr::select(Prey, Adult_INC_Occ, Adult_INC_Rel, Adult_CHR_Occ, Adult_CHR_Rel,
                Chick_Occ, Chick_Rel) |>
  dplyr::mutate(dplyr::across(Adult_INC_Occ:Chick_Rel, round, 3))

#------------------------------------------------------------------------------
# TUPU - METAZOA - CAMERAS & DROPPED PREY
#------------------------------------------------------------------------------

# load data
cam_rel <- read.csv('processed_data/Cam_rel.csv')
drop_rel <- read.csv('processed_data/Dropped_rel.csv')

# group all fish to match 18S categories
cam_rel <- cam_rel |>
  dplyr::mutate(Fish = Gadid + Sand_lance + Herring0 + Prowfish + Capelin +
                  Greenling + Herring1 + Sablefish + Sandfish, 
                .keep = "unused")
drop_rel <- drop_rel |>
  dplyr::mutate(Fish = Pollock + Prowfish + Tomcod + Sandlance + Herring0 +
                  Capelin + Greenling + Herring1 + SaffronCod, 
                .keep = "unused")

# foo tables (1% threshold to match fDNA filtering)
cam_foo <- cam_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
drop_foo <- drop_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))

# intermediate tables
cam_foo_sum <- cam_foo |>
  tidyr::pivot_longer(cols = 2:ncol(cam_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(cam_prop_occ = n / nrow(cam_foo)) |>
  dplyr::select(-n)
cam_rel_sum <- cam_rel |>
  tidyr::pivot_longer(cols = 2:ncol(cam_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()
drop_foo_sum <- drop_foo |>
  tidyr::pivot_longer(cols = 2:ncol(drop_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(drop_prop_occ = n / nrow(drop_foo)) |>
  dplyr::select(-n)
drop_rel_sum <- drop_rel |>
  tidyr::pivot_longer(cols = 2:ncol(drop_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()

# summary table
cam_sum <- cam_foo_sum |>
  dplyr::left_join(cam_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Cameras") |>
  dplyr::rename(Occ = cam_prop_occ, Rel = mean_rel_mass)
drop_sum <- drop_foo_sum |>
  dplyr::left_join(drop_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Dropped") |>
  dplyr::rename(Occ = drop_prop_occ, Rel = mean_rel_mass)
diet_sum <- rbind(cam_sum, drop_sum) |>
  tidyr::pivot_wider(names_from = Method1, values_from = c(Occ,Rel),
                     names_glue = "{Method1}_{.value}") |>
  dplyr::select(Prey, Cameras_Occ, Cameras_Rel, Dropped_Occ, Dropped_Rel) |>
  dplyr::mutate(dplyr::across(Cameras_Occ:Dropped_Rel, round, 3))

#------------------------------------------------------------------------------
# TUPU - FISH - CAMERAS & DROPPED PREY
#------------------------------------------------------------------------------

# load data
cam_rel <- read.csv('processed_data/Cam_rel_fish.csv')
drop_rel <- read.csv('processed_data/Dropped_rel_fish.csv')
  
# Change single obs of Herring1 and all Herring0 to Herring (to match 12S)
cam_rel <- cam_rel |>
  dplyr::mutate(Herring = Herring0 + Herring1, .keep = "unused")
# In dropped prey, single sample with 2 Herring1, & single sample w/ SaffronCod
# (grouped w/ Tomcod in 12S data)
drop_rel <- drop_rel |>
  dplyr::mutate(Herring = Herring0 + Herring1, 
                Tomcod_SaffronCod = Tomcod + SaffronCod,
                Sand_lance = Sandlance, .keep = "unused")

# foo tables (1% threshold to match fDNA filtering)
cam_foo <- cam_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))
drop_foo <- drop_rel |>
  dplyr::mutate(across(where(is.numeric), function(x) ifelse(x >= 0.01, 1, 0)))

# intermediate tables
cam_foo_sum <- cam_foo |>
  tidyr::pivot_longer(cols = 2:ncol(cam_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(cam_prop_occ = n / nrow(cam_foo)) |>
  dplyr::select(-n)
cam_rel_sum <- cam_rel |>
  tidyr::pivot_longer(cols = 2:ncol(cam_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()
drop_foo_sum <- drop_foo |>
  tidyr::pivot_longer(cols = 2:ncol(drop_foo), names_to = "Prey", values_to = "foo") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(n = sum(foo)) |>
  dplyr::ungroup() |>
  dplyr::mutate(drop_prop_occ = n / nrow(drop_foo)) |>
  dplyr::select(-n)
drop_rel_sum <- drop_rel |>
  tidyr::pivot_longer(cols = 2:ncol(drop_rel), names_to = "Prey", values_to = "rel") |>
  dplyr::group_by(Prey) |>
  dplyr::summarize(mean_rel_mass = mean(rel)) |>
  dplyr::ungroup()

# summary table
cam_sum <- cam_foo_sum |>
  dplyr::left_join(cam_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Cameras") |>
  dplyr::rename(Occ = cam_prop_occ, Rel = mean_rel_mass)
drop_sum <- drop_foo_sum |>
  dplyr::left_join(drop_rel_sum, dplyr::join_by(Prey)) |>
  dplyr::mutate(Method1 = "Dropped") |>
  dplyr::rename(Occ = drop_prop_occ, Rel = mean_rel_mass)
diet_sum <- rbind(cam_sum, drop_sum) |>
  tidyr::pivot_wider(names_from = Method1, values_from = c(Occ,Rel),
                     names_glue = "{Method1}_{.value}") |>
  dplyr::select(Prey, Cameras_Occ, Cameras_Rel, Dropped_Occ, Dropped_Rel) |>
  dplyr::mutate(dplyr::across(Cameras_Occ:Dropped_Rel, round, 3))
#------------------------------------------------------------------------------
