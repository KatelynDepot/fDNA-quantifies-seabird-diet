
#------------------------------------------------------------------------------
# CALCULATE RRA
#------------------------------------------------------------------------------
# setwd("..")

# TUPU ------------------------------------------------------------------------

fish <- read.csv('raw_data/level-7_common_names.csv') |>
  dplyr::filter(Species == "TUPU") |>
  dplyr::select(index, Age, Stage, Stenobrachius:Sailfin_sculpin)
metazoa <- read.csv('raw_data/level-6_common_names.csv') |>
  dplyr::filter(Species == "TUPU") |>
  dplyr::select(index, Age, Stage, Copepod:Goose_barnacle)

fish_rra <- fish |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  dplyr::mutate_at(dplyr::vars(Stenobrachius:Sailfin_sculpin), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)
metazoa_rra <- metazoa |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  dplyr::mutate_at(dplyr::vars(Copepod:Goose_barnacle), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)

# remove prey only observed in BLKI (must make up >= 1% in >= 1 sample)
non_zero_fish <- fish_rra |>
  dplyr::summarize(dplyr::across(Stenobrachius:Sailfin_sculpin, max)) |>
  tidyr::pivot_longer(cols = Stenobrachius:Sailfin_sculpin) |>
  dplyr::filter(value >= 0.01) |>
  dplyr::pull(name)
fish_rra <- fish_rra |>
  dplyr::select(index, Age, Stage, all_of(non_zero_fish))

non_zero_metazoa <- metazoa_rra |>
  dplyr::summarize(dplyr::across(Copepod:Goose_barnacle, max)) |>
  tidyr::pivot_longer(cols = Copepod:Goose_barnacle) |>
  dplyr::filter(value >= 0.01) |>
  dplyr::pull(name)
metazoa_rra <- metazoa_rra |>
  dplyr::select(index, Age, Stage, all_of(non_zero_metazoa))

# mean hatch date 13 July
fish_rra <- fish_rra |>
  dplyr::mutate(type = dplyr::case_when(Age == "CH" ~ "Chick",
                                 Age == "AD" & Stage == "INC" ~ "Adult_INC",
                                 Age == "AD" & Stage == "CHR" ~ "Adult_CHR"),
                .before = Stenobrachius)
metazoa_rra <- metazoa_rra |>
  dplyr::mutate(type = dplyr::case_when(Age == "CH" ~ "Chick",
                                 Age == "AD" & Stage == "INC" ~ "Adult_INC",
                                 Age == "AD" & Stage == "CHR" ~ "Adult_CHR"),
                .before = Copepod)

# export fDNA tables
# write.csv(fish_rra, 'processed_data/TUPU_rra_fish.csv', row.names = FALSE)
# write.csv(metazoa_rra, 'processed_data/TUPU_rra_metazoa.csv', row.names = FALSE)

# -----------------------------------------------------------------------------
# combined:
library(magrittr)
rra <- metazoa_rra %>%
  dplyr::left_join(fish_rra) %>%
  # if 12S data available, replace "Fish" rra with species rras
  dplyr::mutate_at(dplyr::vars(Stenobrachius:Sailfin_sculpin), dplyr::funs(. * Fish)) %>%
  dplyr::mutate(Fish = dplyr::case_when(! is.na(Stenobrachius) ~ 0,
                                 is.na(Stenobrachius) ~ Fish)) %>%
  # make sure individual rras sum to 1
  replace(is.na(.), 0) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>%
  dplyr::mutate(sum = rowSums(across(where(is.numeric)))) %>%
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) %>%
  dplyr::select(-sum)

# export combined table
# write.csv(rra, 'processed_data/TUPU_rra_combined.csv', row.names = FALSE)
#------------------------------------------------------------------------------

# BLKI ------------------------------------------------------------------------

link_ids <- read.csv('raw_data/BLKI_deployments.csv') |>
  dplyr::select(dep_id, Fecal.Sample.ID)

fish <- read.csv('raw_data/level-7_common_names.csv') |>
  dplyr::left_join(link_ids, dplyr::join_by("index" == "Fecal.Sample.ID")) |>
  dplyr::filter(! is.na(dep_id)) # remove birds from other project
metazoa <- read.csv('raw_data/level-6_common_names.csv') |>
  dplyr::left_join(link_ids, dplyr::join_by("index" == "Fecal.Sample.ID")) |>
  dplyr::filter(! is.na(dep_id)) # remove birds from other project

fish_rra <- fish |>
  dplyr::mutate(sum = rowSums(dplyr::across(Stenobrachius:Sailfin_sculpin))) |>
  dplyr::mutate_at(dplyr::vars(Stenobrachius:Sailfin_sculpin), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)
metazoa_rra <- metazoa |>
  dplyr::mutate(sum = rowSums(dplyr::across(Copepod:Goose_barnacle))) |>
  dplyr::mutate_at(dplyr::vars(Copepod:Goose_barnacle), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)

# remove prey only observed in TUPU (must make up >= 1% in >= 1 sample)
non_zero_fish <- fish_rra |>
  dplyr::summarize(dplyr::across(Stenobrachius:Sailfin_sculpin, max)) |>
  tidyr::pivot_longer(cols = Stenobrachius:Sailfin_sculpin) |>
  dplyr::filter(value >= 0.01) |>
  dplyr::pull(name)
fish_rra <- fish_rra |>
  dplyr::select(index, dep_id, all_of(non_zero_fish))

non_zero_metazoa <- metazoa_rra |>
  dplyr::summarize(dplyr::across(Copepod:Goose_barnacle, max)) |>
  tidyr::pivot_longer(cols = Copepod:Goose_barnacle) |>
  dplyr::filter(value >= 0.01) |>
  dplyr::pull(name)
metazoa_rra <- metazoa_rra |>
  dplyr::select(index, dep_id, all_of(non_zero_metazoa))

# make sure rows sum to 1
fish_rra <- fish_rra |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)
metazoa_rra <- metazoa_rra |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# export fDNA tables
# write.csv(fish_rra, 'processed_data/BLKI_rra_fish.csv', row.names = FALSE)
# write.csv(metazoa_rra, 'processed_data/BLKI_rra_metazoa.csv', row.names = FALSE)

# -----------------------------------------------------------------------------
# combined:
library(magrittr)
rra <- metazoa_rra %>%
  dplyr::left_join(fish_rra) %>%
  dplyr::relocate(dep_id, .after = index) %>%
  # if 12S data available, replace "Fish" rra with species rras
  dplyr::mutate_at(dplyr::vars(Stenobrachius:Atlantic_salmon), dplyr::funs(. * Fish)) %>%
  dplyr::mutate(Fish = dplyr::case_when(! is.na(Stenobrachius) ~ 0,
                                 is.na(Stenobrachius) ~ Fish)) %>%
  # make sure individual rras sum to 1
  replace(is.na(.), 0) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>%
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) %>%
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) %>%
  dplyr::select(-sum)

# export combined table
# write.csv(rra, 'processed_data/BLKI_rra_combined.csv', row.names = FALSE)
# -----------------------------------------------------------------------------
