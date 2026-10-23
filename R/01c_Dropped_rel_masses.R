
#------------------------------------------------------------------------------
# DROPPED PREY RELATIVE BIOMASSES
#------------------------------------------------------------------------------
# Dropped prey used for summary table but not for stats because most samples
# are not complete bill loads (11 full and 27 partial).

# setwd("..")

dropped <- read.csv('raw_data/Dropped_prey.csv') |>
  dplyr::select(SampleReference, Prey, Mass) |>
  tidyr::pivot_wider(names_from = Prey, values_from = Mass)
dropped[is.na(dropped)] <- 0

# relative biomass table
dropped_rel <- dropped |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  dplyr::mutate_at(dplyr::vars(Pollock:Squid), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-sum)

# relative biomass table - fish only
dropped_rel_fish <- dropped_rel |>
  dplyr::select(-Squid) |>
  dplyr::mutate(sum = rowSums(dplyr::across(Pollock:SaffronCod))) |>
  dplyr::mutate_at(dplyr::vars(Pollock:SaffronCod), dplyr::funs(. / sum)) |>
  dplyr::mutate(dplyr::across(Pollock:SaffronCod, round, 3)) |>
  dplyr::mutate(sum = rowSums(dplyr::across(Pollock:SaffronCod))) |>
  dplyr::filter(! is.na(sum)) |>
  dplyr::select(-sum)

# make sure rows sum to 1
dropped_rel <- dropped_rel |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)
dropped_rel_fish <- dropped_rel_fish |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# export dropped prey tables
# write.csv(dropped_rel, 'processed_data/Dropped_rel.csv', row.names = FALSE)
# write.csv(dropped_rel_fish, 'processed_data/Dropped_rel_fish.csv', row.names = FALSE)
#------------------------------------------------------------------------------
