
#------------------------------------------------------------------------------
# GURGE RELATIVE BIOMASSES
#------------------------------------------------------------------------------
# setwd("..")

gurge <- read.csv('raw_data/Gurge_samples.csv')

# relative biomass table
gurge_rel <- gurge |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  dplyr::mutate_at(dplyr::vars(Herring:Polychaete), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)

# relative biomass table - fish only
gurge_rel_fish <- gurge_rel |>
  dplyr::select(-c(Amphipod:Polychaete)) |>
  dplyr::mutate(sum = rowSums(dplyr::across(Herring:Fish))) |>
  dplyr::mutate_at(dplyr::vars(Herring:Fish), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(Herring:Fish, round, 3)) |>
  dplyr::mutate(sum = rowSums(dplyr::across(Herring:Fish))) |>
  dplyr::filter(! is.na(sum)) |>
  dplyr::select(-sum)

# make sure rows sum to 1
gurge_rel <- gurge_rel |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)
gurge_rel_fish <- gurge_rel_fish |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# export gurge tables
# write.csv(gurge_rel, 'processed_data/gurge_rel.csv', row.names = FALSE)
# write.csv(gurge_rel_fish, 'processed_data/gurge_rel_fish.csv', row.names = FALSE)
#------------------------------------------------------------------------------
