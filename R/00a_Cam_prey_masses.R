
################################################################################
#                            CALCULATE PREY MASSES                             #
################################################################################

# In the second round of data entry, measurements in pixels of bill height,
# headbill, prey head length, and prey tail length were entered.

# This script uses known data about prey proportions, bird morphometrics,
# and prey length-mass relationships to calculate masses of prey.

# Then, it re-formats the table to display results by individual prey item,
# by prey type in bill load, and by bill load.



#------------------------------------------------------------------------------
# STEP 1 - SPLIT INDIVIDUAL PREY INTO SEPARATE ROWS
#------------------------------------------------------------------------------

# setwd("..")

# Load prey measurements datasheet
measurements <- read.csv('raw_data/Cam_prey_measurements.csv')

# Duplicate rows n times according to "Count" column
measurements <- measurements |>
  tidyr::uncount(Count)

#------------------------------------------------------------------------------
# STEP 2 - CALCULATE TOTAL PREY LENGTHS IN PIXELS
#------------------------------------------------------------------------------

# Known prey proportions
prey_props <- read.csv('raw_data/Prey_props_summary.csv')

# Select and rename necessary columns from prey prop table
prey_props <- prey_props |>
  dplyr::select(Prey, Head_prop = HeadAv, Tail_prop = TailAv)

measurements <- measurements |>
  # Make measurements numeric (turns all "X" and blank entries to NA)
  dplyr::mutate_at(dplyr::vars(BillHeight_pix:TailLength_pix), as.numeric) |>
  # Add prey proportion information
  dplyr::left_join(prey_props) |>
  # Calculate total lengths in pixels
  dplyr::mutate(TotalLength_pix = dplyr::case_when(
    # If head length entered:
    HeadLength_pix > 0 ~ HeadLength_pix / Head_prop,
    # If tail length entered:
    TailLength_pix > 0 ~ TailLength_pix / Tail_prop))

#------------------------------------------------------------------------------
# STEP 3 - CALCULATE TOTAL PREY LENGTHS IN MM
#------------------------------------------------------------------------------

# Known average bird morphometrics in mm
BillHeight <- 34.5
Headbill <- 95.0

measurements <- measurements |>
  # Calculate total lengths in mm
  dplyr::mutate(TotalLength = dplyr::case_when(
    # If bill height entered:
    BillHeight_pix > 0 ~ TotalLength_pix * BillHeight / BillHeight_pix,
    # If headbill entered:
    Headbill_pix > 0 ~ TotalLength_pix * Headbill / Headbill_pix))

#------------------------------------------------------------------------------
# STEP 4 - CALCULATE PREY MASSES
#------------------------------------------------------------------------------

# Known prey length-mass relationships from RHAU data
coefs <- read.csv('raw_data/Prey_coefs.csv') |>
  dplyr::select(-c(n, Years))

measurements <- measurements |>
  # Add prey length-mass relationship coefficients
  dplyr::left_join(coefs) |>
  # Calculate masses
  dplyr::mutate(Mass = a * TotalLength ^ b)

# Drop unnecessary intermediate columns
measurements <- measurements |>
  dplyr::select(-c(Head_prop, Tail_prop, TotalLength_pix, a, b))

#------------------------------------------------------------------------------
# STEP 5 - FORMAT AND EXPORT
#------------------------------------------------------------------------------

# One row per individual prey item
masses_ind <- measurements |>
  dplyr::select(-c(Colony:Comment))
  # Dropped redundant columns related to bill loads
  # (can join later from BL_ID if needed)

# Export table
# write.csv(masses_ind, 'processed_data/Prey_masses_by_ind.csv', row.names = FALSE)
#------------------------------------------------------------------------------

# One row per prey type per bill load
masses_type <- measurements |>
  dplyr::group_by(BL_ID, Prey) |>
  dplyr::summarize(Count = dplyr::n(),
                   Confidence = mean(Confidence),
                   BirdMethod = dplyr::case_when(
                     mean(BillHeight_pix) > 0 ~ "Bill height",
                     mean(Headbill_pix) > 0 ~ "Headbill"),
                   PreyMethod = dplyr::case_when(
                     mean(HeadLength_pix) > 0 ~ "Head",
                     mean(TailLength_pix) > 0 ~ "Tail"),
                   n_measured = max(dplyr::n_distinct(HeadLength_pix, na.rm = T),
                                    dplyr::n_distinct(TailLength_pix, na.rm = T)),
                   TotalMass = sum(Mass),
                   AvMass = TotalMass/Count,
                   AvLength = sum(TotalLength)/Count) |>
  dplyr::ungroup()

# Export table
# write.csv(masses_type, 'processed_data/Prey_masses_by_type.csv', row.names = FALSE)
#------------------------------------------------------------------------------

# One row per bill load
library(magrittr)
final <- masses_type %>%
  dplyr::select(BL_ID, Prey, Count, Confidence, TotalMass) %>%
  tidyr::pivot_wider(names_from = Prey,
                     values_from = c(Count, Confidence, TotalMass), 
                     names_vary = "slowest") %>%
  dplyr::rename_all( ~ stringr::str_replace_all(., pattern = c(
    "Count" = "count", "Confidence" = "conf", "TotalMass" = "mass"))) %>%
  dplyr::rename_at(c(2:ncol(.)), ~ sub("(.*)_(.*)", "\\2_\\1", .))

# Join bill load data
bill_loads <- measurements |>
  dplyr::select(BL_ID:Comment) |>
  dplyr::filter(!duplicated(BL_ID))
final_bill_loads <- bill_loads |>
  dplyr::right_join(final)

# Set column order
final_bill_loads <- final_bill_loads |>
  dplyr::select(1:Comment, Unknown_count, UnknownFish_count,
                # No confidences or masses were calculated for unknowns
                dplyr::starts_with("Capelin"), dplyr::starts_with("Gadid"),
                dplyr::starts_with("Herring0"), dplyr::starts_with("Sandlance"),
                dplyr::starts_with("Greenling"), dplyr::starts_with("Herring1"),
                dplyr::starts_with("Prowfish"), dplyr::starts_with("Sablefish"),
                dplyr::starts_with("Sandfish"), dplyr::starts_with("Squid"))

# Export table
# write.csv(final_bill_loads, 'processed_data/Final_bill_loads.csv', row.names = FALSE)
#------------------------------------------------------------------------------
