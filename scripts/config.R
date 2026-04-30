# ============================================================================
# Configuration & Constants for END2DALY
# ============================================================================
# Single source of truth for paths, constants, thresholds
# Update here if you move data or change regional filters
# ============================================================================

# Data paths
PATH_METADATA_EXPOSURE <- "data/Expositionsdaten/raw/MetadatenExpositionsdaten.csv"
PATH_ETCHE_FUNCTIONS   <- "data/ETCHE/functionsETCHE.xlsx"
PATH_GV_REFERENCE      <- "data/Gebietstabellen/raw/GV100AD_30112025.txt"
PATH_VG250_GIS         <- "data/Gebietstabellen/raw/DE_VG250.gpkg"
PATH_KREISE_DATA       <- "data/Gebietstabellen/raw/kreise_daten.csv"
PATH_GEMEINDEN_DATA    <- "data/Gebietstabellen/raw/gemeinden_daten.csv"
PATH_BUNDESLAND_DATA   <- "data/Gebietstabellen/raw/bundesland_daten.csv"

# Output paths
PATH_EXPO_OUTPUT       <- "data/Expositionsdaten/processed/"
PATH_AREA_OUTPUT       <- "data/Gebietstabellen/processed/"

# Reused data path
PATH_GEMEINDE_KREIS_DATA<-paste0(PATH_AREA_OUTPUT,"gv_kreis_gemeinden.csv")
PATH_ALLEXPO_DATA     <-paste0(PATH_EXPO_OUTPUT,"allExpo.csv")
# Regional filter
HESSEN_ONLY            <- TRUE
HESSEN_BUNDESLAND_CODE <- "06"

# Create output directory if needed
dir.create(PATH_EXPO_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_AREA_OUTPUT, recursive = TRUE, showWarnings = FALSE)