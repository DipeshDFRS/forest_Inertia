#clean environment
rm(list=ls())

#Load libraries
library(remotes)
library(BioSIM)
library(dplyr)
#install Artemis2014 
options(download.file.method = "wininet")
remotes::install_github("Modelisation-DRF/Artemis2014")
library(Artemis2014)


#new intallation of ARTEMIS
ibrary(remotes)

#options(download.file.method = "wininet")
options(download.file.method = "auto")
remotes::install_github("Modelisation-DRF/Artemis2014")

require(remotes)
remotes::install_github("Modelisation-DRF/Artemis2014", force=TRUE)

require(remotes)
remotes::install_github("Modelisation-DRF/Artemis2014")

#From Collins data ####
#read data
psp_data <- read.csv("./data/psp_tree_data.csv")
psp_data <- psp_data %>% 
  dplyr::filter(no_mes == 1)

#test
# Number of PlacetteID that contain at least one missing value
placette_missing <- psp_data %>%
  dplyr::group_by(PlacetteID) %>%
  dplyr::summarise(has_na = any(is.na(dplyr::across(everything())))) %>%
  dplyr::filter(has_na)

nrow(placette_missing)

placette_missing_count <- psp_data %>%
  dplyr::group_by(PlacetteID) %>%
  dplyr::summarise(
    n_missing = sum(is.na(across(everything())))
  )

head(placette_missing_count)

#climate data
climate_data <- read.csv("./data/psp_with_climate.csv")

climate_data_subset <- climate_data %>% 
  dplyr::select(PlacetteID,Year, MeanTair,TotalPrcp, GrowSeason) %>% 
  dplyr::rename(TMoy = MeanTair,
                PTot = TotalPrcp,
                GrwDays = GrowSeason)

psp_data <- psp_data %>% 
  rename(Year = year_inventory)

psp_data_with_climate <- psp_data %>% 
  left_join(climate_data_subset, by = c( "PlacetteID", "Year" ))

#final data
Donnees_Preparees <- psp_data_with_climate %>%
  # 1. Fix data types, scales, and create missing columns
  mutate(
    # Convert DHP from mm to cm
    DHPcm = as.numeric(DHPcm) / 10,
    
    # Fix local tree ID sequence matching no_arbre
    origTreeID = as.numeric(no_arbre),
    
    # Force classifications/ecological metrics to match template formats
    Etat       = as.numeric(Etat),
    Cl_Drai    = as.numeric(Cl_Drai),
    Exposition = as.numeric(Exposition),
    Pente      = as.numeric(Pente),
    Sdom_Bio   = as.character(Sdom_Bio),
    Reg_Eco    = as.character(Reg_Eco),
    Type_Eco   = as.character(Type_Eco),
    Veg_Pot    = as.character(Veg_Pot),
    
    # Add explicit standard columns
    Nombre     = 1,
    #Strate     = "A_DETERMINER",  # Placeholder: populate based on your stratification rules if needed
    #Age_moy    = 50               # Placeholder: populate with true stand age if available
  ) %>%
  # 2. Select and order columns exactly like Donnees_Exemple
  select(
     PlacetteID, Latitude, Longitude, Altitude, #Strate,
    Sdom_Bio, Reg_Eco, Type_Eco, Cl_Drai, Espece, 
    Etat, DHPcm, Nombre, Veg_Pot, origTreeID, 
    GrwDays, PTot, TMoy, cec_015cm, sand_015cm, 
    Pente, Exposition #, Age_moy
  )

# Convert to standard data frame to clear tibble metadata structures
Donnees_Preparees <- as.data.frame(Donnees_Preparees)

#run ARTEMIS
result <- simulateurArtemis(Data_ori = Donnees_Preparees, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )

# run artemis on small portion of data first
random_sample <- Donnees_Preparees %>%
  slice_sample(n = 200)

result <- simulateurArtemis(Data_ori = random_sample, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )


# From Dipesh data ####
# from partilly clean data ####
artemis_data <- read.csv("./data/artemis_variables_final.2026_06_01.csv")

# Align data structure precisely with Donnees_Exemple
artemis_data <- artemis_data %>%
  mutate(
    PlacetteID = as.numeric(PlacetteID),
    origTreeID = as.numeric(origTreeID),
    Cl_Drai    = as.numeric(Cl_Drai),
    Etat       = as.numeric(Etat),
    
    # Add missing columns expected by the model
    #Strate     = "Sapiniere30ans",  # Set a dummy default or align with your stratum metadata
    #Age_moy    = 50                 # Set a dummy default or realistic plot mean age
  ) %>%
  # 3. Match the exact column order of Donnees_Exemple
  dplyr::select(
    PlacetteID, Latitude, Longitude, Altitude, Sdom_Bio, Reg_Eco, #Strate,
    Type_Eco, Cl_Drai, Espece, Etat, DHPcm, Nombre, Veg_Pot, origTreeID, 
    GrwDays, PTot, TMoy, cec_015cm, sand_015cm, Pente, Exposition, #Age_moy
  )

result <- simulateurArtemis(Data_ori = artemis_data, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
random_sample <- artemis_data %>%
  slice_sample(n = 500)

# Check the dimensions of your new data frame
dim(random_sample)

result <- simulateurArtemis(Data_ori = random_sample, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )

print(result)

#From .rda data####
load("./data/artemis_variables_final.2026_06_01.rda")

# Align data structure precisely with Donnees_Exemple
artemis_variables_final <- artemis_variables_final %>%
  mutate(
    PlacetteID = as.numeric(PlacetteID),
    origTreeID = as.numeric(origTreeID),
    Cl_Drai    = as.numeric(Cl_Drai),
    Etat       = as.numeric(Etat),
    
    # Add missing columns expected by the model
    #Strate     = "Sapiniere30ans",  # Set a dummy default or align with your stratum metadata
    #Age_moy    = 50                 # Set a dummy default or realistic plot mean age
  ) %>%
  # 3. Match the exact column order of Donnees_Exemple
  dplyr::select(
     PlacetteID, Latitude, Longitude, Altitude, Sdom_Bio, Reg_Eco, #Strate,
    Type_Eco, Cl_Drai, Espece, Etat, DHPcm, Nombre, Veg_Pot, origTreeID, 
    GrwDays, PTot, TMoy, cec_015cm, sand_015cm, Pente, Exposition, #date_sond #Age_moy
  )


result <- simulateurArtemis(Data_ori = artemis_variables_final, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
# run artemis on small portion of data first
df_test <- artemis_variables_final %>%
  slice_sample(n = 200)





#Raphel's addition start####
liste_placettes <- split(df, df$PlacetteID)

res <- lapply(liste_placettes, function(df){
  annee_dep <- unique(df$date_sond)
  if (length(annee_dep) != 1) {
    stop(
      "PlacetteID ", unique(df$PlacetteID),
      " has more than one date_sond: ",
      paste(annee_dep, collapse = ", ")
    )
  }
  Artemis2014::simulateurArtemis(
    Data_ori = df,
    AnneeDep= 2000,
    Horizon = 5, Tendance = 0, Residuel = 0,
    FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45')
  
})

resultats <- dplyr::bind_rows(res)
#Raphel's addition end####







#test start####
# Load libraries
library(dplyr)
library(Artemis2014)

artemis_vars <- load(file = 'data/artemis_variables_final.2026_06_01.rda')

artemis_variables_final <- artemis_variables_final %>%
  mutate(
    PlacetteID = as.numeric(PlacetteID),
    origTreeID = as.numeric(origTreeID),
    Cl_Drai    = as.numeric(Cl_Drai),
    Etat       = as.numeric(Etat),
    no_mes     = if ("no_mes" %in% names(.)) as.numeric(no_mes) else 1,
    Dom_Bio    = if ("Dom_Bio" %in% names(.)) as.character(Dom_Bio) else "NA",
    Depot      = if ("Depot" %in% names(.)) Depot else NA_character_
  ) %>%
  dplyr::select(
    PlacetteID, no_mes, origTreeID, Espece, Etat, DHPcm, Dom_Bio, 
    Nombre, Sdom_Bio, Veg_Pot, Latitude, Longitude, Altitude,
    Pente, Exposition, Depot, PTot, TMoy, GrwDays, Reg_Eco, Type_Eco, 
    Cl_Drai, cec_015cm, sand_015cm
  )

artemis_variables_final <- artemis_variables_final %>%
  arrange(PlacetteID, origTreeID, no_mes) %>%
  distinct(PlacetteID, origTreeID, .keep_all = TRUE)

# 5. Sample by Plot
sample_plots_rda <- unique(artemis_variables_final$PlacetteID)[1:5]
df_test_rda <- artemis_variables_final %>% 
  filter(PlacetteID %in% sample_plots_rda)

write.csv(
  df_test_rda,
  "./data/sample_data.csv",
  row.names = FALSE
)

# 6. Run the Artemis Simulator
result_rda <- simulateurArtemis(
  Data_ori = df_test_rda, 
  Horizon = 3, 
  AnneeDep = 2000,
  Tendance = 0, 
  Residuel = 0,
  FacHa = 25, 
  EvolClim = 0, 
  AccModif = 'ORI', 
  MortModif = 'ORI', 
  RCP = 'RCP45' 
)

head(result_rda)
#test end####

result <- simulateurArtemis(Data_ori = random_sample, Horizon = 3, Tendance = 0, Residuel = 0,
                            FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )


#run artemis with example data####
# Load a file 
Donnees_Example <- load("./data/Donnees_Exemple.rda")
names(Donnees_Exemple)
Donnees_Exemple <- Donnees_Exemple[, -23]

#prepare data
#Donnees_Example <-  PrepareData(Data, Climtous)


#examples
result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 3, Tendance = 0, Residuel = 0,
FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
print(result)

#export
write.csv(result, "./Output/example_output.csv")


# Avec effet TBE
result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 4,
TBE = c(1, 1, 1, 1), Tendance = 0, Residuel = 0,
FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
print(result)

# Avec traitement de coupe
result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 6,
Coupe_ON = c(3, NA, NA, NA, NA, 8), Tendance = 0, Residuel = 0,
FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
print(result)

# Avec modificateurs de coupe
result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 4,
Coupe_ON = c(3, NA, NA, 8),
Coupe_modif = list(80, NA, NA, 0), Tendance = 0, Residuel = 0,
FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
print(result)



#Intrant_data

load("./data/Intrant_Test.rda")









