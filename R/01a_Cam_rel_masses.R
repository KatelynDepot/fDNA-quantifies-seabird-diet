
#------------------------------------------------------------------------------
# CAMERA RELATIVE BIOMASSES
#------------------------------------------------------------------------------
# setwd("..")

# Unknowns --------------------------------------------------------------------
masses <- read.csv('processed_data/Prey_masses_by_type.csv')

# Total number of bill loads
n_billloads <- dplyr::n_distinct(masses$BL_ID)
# Total number of bill loads with identifiable prey
n_ided_billloads <- dplyr::n_distinct(subset(masses, !masses$Prey %in% 
                                               c("Unknown", "UnknownFish"))$BL_ID)
# Total number of bill loads with identifiable fish prey
n_ided_billloads_fish <- dplyr::n_distinct(subset(masses, !masses$Prey %in% 
                                               c("Unknown", "UnknownFish","Squid"))$BL_ID)

# Summary of unknowns
library(magrittr)
unknown_sum <- masses %>%
  dplyr::filter(Prey %in% c("Unknown", "UnknownFish")) %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarize(Freq_occur = dplyr::n(), Prop_occur = dplyr::n() / n_billloads) %>%
  rbind(., c("IdentifiedPrey", n_ided_billloads, n_ided_billloads / n_billloads)) %>%
  dplyr::mutate(Prop_occur = round(as.numeric(Prop_occur), digits = 3))

# Relative biomasses ----------------------------------------------------------

incomplete_BLs <- masses |>
  dplyr::filter(Prey %in% c("Unknown", "UnknownFish")) |> dplyr::pull(BL_ID)

# relative biomass table
cam_rel <- masses |>
  dplyr::filter(! BL_ID %in% incomplete_BLs, # complete BLs only
                # ! Prey == "Squid", # fish only
                ! is.na(TotalMass)) |> # measurable BLs only
  dplyr::group_by(BL_ID, Prey) |>
  dplyr::summarize(Mass = sum(TotalMass)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = Prey, values_from = Mass) |>
  dplyr::rowwise() |>
  dplyr::mutate(BL_mass = sum(dplyr::c_across(Gadid:Sandfish), na.rm = T)) |>
  dplyr::mutate(dplyr::across(Gadid:Sandfish, ~ .x / BL_mass)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-BL_mass)
cam_rel[is.na(cam_rel)] <- 0

# relative biomass table - fish only
cam_rel_fish <- masses |>
  dplyr::filter(! BL_ID %in% incomplete_BLs, # complete BLs only
                ! Prey == "Squid", # fish only
                ! is.na(TotalMass)) |> # measurable BLs only
  dplyr::group_by(BL_ID, Prey) |>
  dplyr::summarize(Mass = sum(TotalMass)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = Prey, values_from = Mass) |>
  dplyr::rowwise() |>
  dplyr::mutate(BL_mass = sum(dplyr::c_across(Gadid:Sandfish), na.rm = T)) |>
  dplyr::mutate(dplyr::across(Gadid:Sandfish, ~ .x / BL_mass)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) |>
  dplyr::select(-BL_mass)
cam_rel_fish[is.na(cam_rel_fish)] <- 0

# make sure rows sum to 1
cam_rel <- cam_rel |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)
cam_rel_fish <- cam_rel_fish |>
  dplyr::mutate(sum = rowSums(dplyr::across(where(is.numeric)))) |>
  assertr::assert(assertr::within_bounds(0.99,1.01), sum) |>
  dplyr::select(-sum)

# export cam tables
# write.csv(cam_rel, 'processed_data/Cam_rel.csv', row.names = FALSE)
# write.csv(cam_rel_fish, 'processed_data/Cam_rel_fish.csv', row.names = FALSE)
#------------------------------------------------------------------------------
