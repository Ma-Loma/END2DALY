# ============================================================================
# Configuration & Constants for END2DALY
# ============================================================================
# Single source of truth for paths, constants, thresholds
# Update here if you move data or change regional filters
# ============================================================================

# Data paths
PATH_METADATA_EXPOSURE <- "data/Expositionsdaten/MetadatenExpositionsdaten.csv"
PATH_ETCHE_FUNCTIONS   <- "data/ETCHE/functionsETCHE.xlsx"
PATH_GV_REFERENCE      <- "data/Gebietstabellen/GV100AD_30112025.txt"
PATH_VG250_GIS         <- "data/Gebietstabellen/DE_VG250.gpkg"
PATH_KREISE_DATA       <- "data/Gebietstabellen/kreise_daten.csv"
PATH_GEMEINDEN_DATA    <- "data/Gebietstabellen/gemeinden_daten.csv"

# Output paths
PATH_EXPO_OUTPUT       <- "data/Expositionsdaten/sauber/"

# Regional filter
HESSEN_ONLY            <- TRUE
HESSEN_BUNDESLAND_CODE <- "06"

# Create output directory if needed
dir.create(PATH_EXPO_OUTPUT, recursive = TRUE, showWarnings = FALSE)