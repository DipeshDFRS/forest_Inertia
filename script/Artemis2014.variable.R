# ========================================================================== #
#           COMPILE ARTEMIS VARIABLES                     ####
# ========================================================================== #
# Date: 2026-06-02
# Description: Processes Permanent Ecological Plot (PEP) data for specific
#              Forest Management Units (FMUs) in the Gaspesie region.
# ========================================================================== #

# --- Clear Environment ---
rm(list = ls())

# --- Load Required Libraries ---
library(sf)
library(tidyverse)
library(terra)
library(readxl)
library(writexl)
library(lwgeom)

# --- Define Projections ---
#crs_wgs84 <- st_crs(4326)

# ========================================================================== #
#                            LOAD & PREPARE DATA                             ####
# ========================================================================== #

dendro_arbre   <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "DENDRO_ARBRES")
station_pe     <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "STATION_PE")
pee_ori_sond   <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "PEE_ORI_SOND")
placette_mes   <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "PLACETTE_MES")
placette       <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "PLACETTE")
classi_eco_pe  <- st_read("./data/PEP_GPKG/PEP.gpkg", layer = "classi_eco_pe")

# --- Prepare Required Data ---
trees <- dendro_arbre %>%
  dplyr::select(id_pe, no_mes, id_pe_mes, no_arbre, id_arbre, id_arb_mes,
                essence, dhp, tige_ha, etat) %>% 
  filter(etat %in% c(10, 12, 30, 32, 40, 42, 50, 52)) %>%
  na.omit()

plot_station <- station_pe %>%
  dplyr::select(id_pe, no_mes, id_pe_mes, cl_drai, 
                type_eco, veg_pot, exposition, pc_pent, altitude) 

plot_ori <- pee_ori_sond %>%
  dplyr::select(id_pe, no_mes, id_pe_mes, cl_drai, type_eco) 

plot_dates <- placette_mes %>%
  dplyr::select(id_pe, no_mes, id_pe_mes, version, date_sond)

plot_locations <- placette %>%
  dplyr::select(no_prj, no_viree, no_pe, id_pe, latitude, longitude, dern_sond) %>%
  mutate(cruise_line = paste(no_prj, no_viree, sep = ""))

plot_eco_class <- classi_eco_pe %>%
  dplyr::select(id_pe, reg_eco, dom_bio, sdom_bio)

# ========================================================================== #
#                        DATA INTEGRATION & PROCESSING                       ####
# ========================================================================== #

plot_info_merged <- plot_station %>%
  left_join(plot_ori, by = c("id_pe", "no_mes", "id_pe_mes"), suffix = c("", ".ori")) %>%
  left_join(plot_dates, by = c("id_pe", "no_mes", "id_pe_mes")) %>%
  left_join(plot_eco_class, by = "id_pe") %>%
  left_join(plot_locations, by = "id_pe")

plot_info_coalesced <- plot_info_merged %>%
  mutate(
    cl_drai = coalesce(cl_drai, cl_drai.ori),
    type_eco = coalesce(type_eco, type_eco.ori)
  ) %>%
  dplyr::select(
    no_prj, no_viree, cruise_line, no_pe,
    id_pe, no_mes, id_pe_mes,
    latitude, longitude, altitude,
    type_eco, veg_pot, exposition, pc_pent,
    reg_eco, dom_bio, sdom_bio, 
    cl_drai, version, dern_sond, date_sond
  )

plot_info_final <- plot_info_coalesced %>%
  mutate(
    date_sond = as.Date(date_sond), 
    year_sond = if_else(!is.na(date_sond), as.numeric(format(date_sond, "%Y")), NA_real_)
  )

# ========================================================================== #
#                    TREE-LEVEL DATA PREPARATION & MERGE                    ####
# ========================================================================== #

trees_final <- trees %>%
  mutate(dhp = dhp / 10, 
         nb_tige = tige_ha / 25) %>% 
  dplyr::select(id_pe, no_mes, id_pe_mes, no_arbre, id_arbre, id_arb_mes,
                essence, dhp, nb_tige, etat)

trees_with_plot_info <- trees_final %>%
  left_join(plot_info_final, by = c("id_pe", "no_mes", "id_pe_mes"))

tree_plot_data <- trees_with_plot_info %>%
  mutate(date_sond = as.Date(date_sond)) %>%
  dplyr::rename(
    PlacetteID = id_pe,
    origTreeID = no_arbre,
    Latitude   = latitude,
    Longitude  = longitude,
    Altitude   = altitude,
    Sdom_Bio   = sdom_bio,
    Reg_Eco    = reg_eco,
    Type_Eco   = type_eco,
    Cl_Drai    = cl_drai,
    Espece     = essence,
    Etat       = etat,
    DHPcm      = dhp,
    Nombre     = nb_tige,
    Veg_Pot    = veg_pot,
    Pente      = pc_pent,
    Exposition = exposition
  )

# --- Process Additional PSP Data ---
additional_psp_data <- read.csv("./data/psp_tree_data.csv")
#additional_psp_data <- read.csv("./data/Tableau_placette_altitude_en_plus.csv")
additional_subset <- additional_psp_data %>%
  mutate(PlacetteID = str_pad(as.character(PlacetteID), width = 10, pad = "0")) %>%
  dplyr::select(PlacetteID, Type_Eco, Veg_Pot, Exposition, Pente, Sdom_Bio, Cl_Drai, sand_015cm, cec_015cm) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    Type_Eco   = as.character(Type_Eco),
    Veg_Pot    = as.character(Veg_Pot),
    Sdom_Bio   = as.character(Sdom_Bio),
    Cl_Drai    = as.character(Cl_Drai),
    sand_015cm = as.numeric(sand_015cm),
    cec_015cm  = as.numeric(cec_015cm),
    Pente      = as.numeric(Pente),
    Exposition = as.numeric(Exposition)
  )

# Ensure matching IDs format seamlessly
tree_plot_data <- tree_plot_data %>%
  dplyr::left_join(additional_subset, by = c("PlacetteID"), suffix = c("", ".add")) %>%
  dplyr::mutate(
    Type_Eco   = dplyr::coalesce(Type_Eco, Type_Eco.add),
    Veg_Pot    = dplyr::coalesce(Veg_Pot, Veg_Pot.add),
    Exposition = dplyr::coalesce(Exposition, Exposition.add),
    Pente      = dplyr::coalesce(Pente, Pente.add),
    Sdom_Bio   = dplyr::coalesce(Sdom_Bio, Sdom_Bio.add),
    Cl_Drai    = dplyr::coalesce(Cl_Drai, Cl_Drai.add)
  ) %>%
  dplyr::select(-ends_with(".add"))

# --- Clean Missing Values & Calculate Modes ---
cols_to_fill <- c("Type_Eco", "Veg_Pot", "Exposition", "Pente", "Sdom_Bio", "Cl_Drai")

tree_plot_data_filled <- tree_plot_data %>%
  group_by(PlacetteID) %>%
  fill(all_of(cols_to_fill), .direction = "downup") %>%
  ungroup()

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

# FIX: Assigned back to Pente instead of creating Pent
tree_plot_data_final <- tree_plot_data_filled %>%
  group_by(PlacetteID) %>%
  mutate(across(all_of(cols_to_fill), ~ ifelse(is.na(.x), get_mode(.x), .x))) %>%
  ungroup() %>%
  mutate(
    Pente      = ifelse(is.na(Pente), 0, Pente), 
    Exposition = ifelse(is.na(Exposition), 0, Exposition)
  )
#test NA plotwise
tree_plot_data_final_test <- tree_plot_data_final %>% 
  dplyr::select(PlacetteID, Type_Eco, Veg_Pot, Sdom_Bio, Cl_Drai, sand_015cm, cec_015cm) %>% 
  distinct()

# --- Filter Down Steps ---
# final_data_recent <- tree_plot_data_final %>%
#   filter(no_mes %in% c(1, 2, 3)) #%>% 
#   #filter(version %in% c("1er inv. 1970 à 1974", "1er inv. 1975 à 1981")) 

final_data_recent <- tree_plot_data_final %>%
  group_by(`PlacetteID`) %>%
  filter(all(c(1, 2, 3) %in% no_mes)) %>%
  ungroup()

final_data_recent_subset <- final_data_recent %>%
  filter(no_mes == 1) %>%
  rename(Year = year_sond)

# final_data_recent_subset <- final_data_recent %>%
#   filter(no_mes == 1) %>%
#   rename(Year = date_sond)

#test NA plotwise
final_data_recent_subset_test <- final_data_recent_subset %>% 
  dplyr::select(PlacetteID, Type_Eco, Veg_Pot, Sdom_Bio, Cl_Drai, sand_015cm, cec_015cm) %>% 
  distinct()

# --- Merge Climate Data ---
climate_data <- read.csv("./data/psp_with_climate.csv")
climate_data_subset <- climate_data %>% 
  dplyr::select(PlacetteID, Year, MeanTair, TotalPrcp, GrowSeason) %>% 
  dplyr::rename(TMoy = MeanTair, PTot = TotalPrcp, GrwDays = GrowSeason) %>%
  mutate(PlacetteID = str_pad(as.character(PlacetteID), width = 10, pad = "0"))

final_data_with_climate <- final_data_recent_subset %>%
  left_join(climate_data_subset, by = c("PlacetteID", "Year"))

#test NA plotwise
final_data_with_climate_test <- final_data_with_climate %>% 
  dplyr::select(PlacetteID, Type_Eco, Veg_Pot, Sdom_Bio, Cl_Drai, sand_015cm, cec_015cm,
                sand_015cm, cec_015cm, TMoy, PTot, GrwDays ) %>% 
  distinct()

#optional : to remove colums with NA
#final_data_with_climate <- na.omit(final_data_with_climate )

# ========================================================================== #
#                            FINAL FORMATTING & OUTPUT                       ####
# ========================================================================== #

# FIX: Calling final_data_with_climate instead of final_data_recent_subset
artemis_variables_final <- final_data_with_climate %>%
  dplyr::select(
    PlacetteID, origTreeID, 
    Longitude, Latitude, Altitude, 
    Veg_Pot, Reg_Eco, Type_Eco, Cl_Drai, Pente, Sdom_Bio, 
    Espece, Etat, DHPcm, Nombre,
    cec_015cm, sand_015cm,
    GrwDays, PTot, TMoy, Exposition,
    date_sond
  ) %>%
  arrange(PlacetteID, origTreeID)

# --- Output Saves ---
old_scipen <- options(scipen = 999)

#write.csv(artemis_variables_final, "./data/artemis_variables_final.2026_06_01.csv", row.names = FALSE)
save(artemis_variables_final, file = "./data/artemis_variables_final.2026_06_11.rda")
#saveRDS(artemis_variables_final, "./data/artemis_variables_final.2026_06_01.rds")

options(scipen = old_scipen)
print("Processing complete. Final ARTEMIS variables saved cleanly.")

















