# ============================================================================
# Utility Functions for END2DALY Pipeline
# ============================================================================
# Shared functions used across Expositionsdaten_vorbereiten.qmd 
# and healthiar_hessen.qmd
# ============================================================================

library(tidyverse)

#' Read Column Names from External Text File
#'
#' Some Excel datasets store column names in separate .txt files.
#' This function reads them line-by-line into a vector.
#'
#' @param path Character: path to text file (one column name per line)
#'
#' @return Character vector of column names
#'
#' @examples
#' cols <- read_colnames("data/Expositionsdaten/ColNamesStr_*.txt")
#' # Returns: c("gemeinde_kennziffer", "anzahl_belasteter_lden_ab_50", ...)
#'
read_colnames <- function(path) {
  if (!file.exists(path)) {
    stop("Column names file not found: ", path, call. = FALSE)
  }
  
  readr::read_csv(path, col_names = FALSE, show_col_types = FALSE) %>%
    pull(1) %>%
    as.character()
}

#' Read One Exposure Dataset from Excel
#'
#' Wrapper around readxl::read_excel with standardized error handling.
#' Applies column names and metadata from metadata registry row.
#'
#' @param meta_row Single-row tibble with columns:
#'   - pfad: path to Excel file
#'   - tabellenblatt: sheet name
#'   - zeilen_weglassen: skip rows
#'   - zeilen_gesamt: max rows to read
#'   - spaltennamen: path to column names text file
#'   - geoschluessel_stellen: geocode format
#'   - datenquelle: data source name (HLNUG, BW, EEA, etc.)
#'
#' @return Tibble with raw exposure data plus metadata columns
#'
#' @details
#' - Applies NA patterns for common missing value codes
#' - Adds metadata columns: geoschluessel_stellen, datenquelle
#' - Stops with informative error if file not found or read fails
#'
read_one_dataset <- function(meta_row) {
  
  # Validate input
  if (!is.data.frame(meta_row) || nrow(meta_row) != 1) {
    stop("meta_row must be a single-row tibble/data.frame", call. = FALSE)
  }
  
  # Read column names
  col_names <- read_colnames(meta_row$spaltennamen)
  
  # Informative progress message
  cat("  ✓ Reading ", basename(meta_row$pfad), 
      " | Sheet: ", meta_row$tabellenblatt, "\n")
  
  # Read Excel with error handling
  tryCatch(
    readxl::read_excel(
      path = meta_row$pfad,
      sheet = meta_row$tabellenblatt,
      skip = meta_row$zeilen_weglassen,
      n_max = meta_row$zeilen_gesamt,
      col_names = col_names,
      na = c("", "NA", "Information not provided", "No data", "Not applicable")
    ) %>%
      mutate(
        geoschluessel_stellen = meta_row$geoschluessel_stellen,
        datenquelle = meta_row$datenquelle,
        .before = 1
      ),
    error = function(e) {
      stop("Failed to read ", meta_row$pfad, ": ", e$message, 
           call. = FALSE)
    }
  )
}

#' Transform Exposure Data to Long Format
#'
#' Standardizes noise exposure data from different sources into a uniform long format.
#' 
#' **Input format (wide):**
#' - One row per municipality
#' - Columns like: gemeinde_kennziffer, anzahl_belasteter_lden_ab_50, anzahl_belasteter_lden_ab_55, ...
#'
#' **Output format (long):**
#' - One row per (municipality × metric × exposure band)
#' - Columns: gemeinde_kennziffer, metrik, l_untergrenze, l_zentral, exponierte
#'
#' @param data Tibble: raw exposure data (wide format)
#'
#' @return Tibble in long format
#'
#' @details
#' Steps:
#' 1. Select only gemeinde & exposure columns
#' 2. Remove "_bis_X" suffixes from duplicate column names
#' 3. Pivot all "anzahl_belasteter_*" columns to long
#' 4. Extract metric name (lden, lnight) and threshold (e.g., 50, 55)
#' 5. Calculate central exposure: l_zentral = l_untergrenze + 2
#' 6. Replace NA with 0 (no exposure = 0 people exposed)
#'
#' @examples
#' data_long <- lang_machen(raw_data)
#'
lang_machen <- function(data) {
  
  # Ensure we have required columns
  if (!any(str_detect(names(data), "gemeinde|belasteter"))) {
    stop("Input data must contain 'gemeinde' and 'belasteter' columns", 
         call. = FALSE)
  }
  
  data %>%
    # Keep only relevant columns
    select(contains("gemeinde") | 
           contains("belasteter") | 
           contains("geoschluessel")) %>%
    # Remove "_bis_X" suffixes (artifact of Excel wide format)
    setNames(str_replace(names(.), "_bis_[0-9]*", "")) %>%
    # Pivot: exposure bands to rows
    pivot_longer(
      starts_with("anzahl"),
      names_sep = "_ab_",
      names_to = c("metrik_raw", "l_untergrenze"),
      values_to = "exponierte"
    ) %>%
    # Clean metric names & calculate central level
    mutate(
      l_untergrenze = as.numeric(l_untergrenze),
      l_zentral = l_untergrenze + 2,
      metrik = str_remove(metrik_raw, "anzahl_belasteter_") %>%
               str_replace_all("l_night", "lnight"),
      .keep = "unused"
    ) %>%
    # Replace missing with 0
    replace_na(list(exponierte = 0))
}

#' Standardize Gemeinde Kennziffer (Municipality Geocode)
#'
#' Ensures all municipal geocodes are in the standard 8-digit format:
#' [2-digit Bundesland Code] + [6-digit municipality code]
#'
#' Raw data sometimes has codes like:
#' - "433" (missing leading zeros & state code)
#' - "06433" (missing trailing zeros)
#' This function standardizes to "06433000" (for Hessen example).
#'
#' @param data Tibble: data with gemeinde_kennziffer and bundesland_code columns
#'
#' @return Tibble with standardized 8-digit gemeinde_kennziffer
#'
#' @examples
#' data %>% gkz_vereinheitlichen()
#'
gkz_vereinheitlichen <- function(data) {
  
  if (!all(c("gemeinde_kennziffer", "bundesland_code") %in% names(data))) {
    stop("Input must have 'gemeinde_kennziffer' and 'bundesland_code' columns",
         call. = FALSE)
  }
  
  data %>%
    mutate(
      # Extract last 6 digits, prepend state code
      gemeinde_kennziffer = str_sub(gemeinde_kennziffer,
                                     start = str_length(gemeinde_kennziffer) - 5) %>%
                           paste0(bundesland_code, .),
      .keep = "all"
    )
}

#' Validate Exposure Data
#'
#' Quality checks on standardized exposure data.
#' Prints warnings if issues detected.
#'
#' @param data Tibble: exposure data (after lang_machen & gkz_vereinheitlichen)
#'
#' @return Tibble (invisibly), for pipe-ability
#'
validate_exposure_data <- function(data) {
  
  cat("\n--- Quality Checks ---\n")
  
  # Check for missing geocodes
  missing_gkz <- data %>% filter(is.na(gemeinde_kennziffer)) %>% nrow()
  if (missing_gkz > 0) {
    warning(missing_gkz, " rows with missing geocodes", immediate. = TRUE)
  }
  
  # Check geocode format (should be 8 digits)
  bad_format <- data %>%
    filter(str_length(gemeinde_kennziffer) != 8) %>%
    nrow()
  if (bad_format > 0) {
    warning(bad_format, " geocodes not in 8-digit format", immediate. = TRUE)
  }
  
  # Check metric names
  expected_metrics <- c("lden", "lnight")
  unexpected <- setdiff(unique(data$metrik), expected_metrics)
  if (length(unexpected) > 0) {
    warning("Unexpected metrics: ", paste(unexpected, collapse = ", "), 
            immediate. = TRUE)
  }
  
  # Summary statistics
  cat("\nExposure summary:\n")
  cat("  Total rows:", nrow(data), "\n")
  cat("  Unique municipalities:", n_distinct(data$gemeinde_kennziffer), "\n")
  cat("  Metrics:", paste(unique(data$metrik), collapse = ", "), "\n")
  cat("  Sources:", paste(unique(data$datenquelle), collapse = ", "), "\n")
  cat("  Total exposed:", sum(data$exponierte, na.rm = TRUE), "persons\n\n")
  
  invisible(data)
}

extract_kreise_gemeinden <- function(lines) {
  kreise_data <- data.frame(
    Kreis_Code = character(),
    Kreis_Name = character(),
    stringsAsFactors = FALSE
  )
  gemeinden_data <- data.frame(
    Gemeindekennziffer = character(),
    Gemeinde_Name = character(),
    stringsAsFactors = FALSE
  )
  
  for (line in lines) {
    if (startsWith(line, "40")) {
      #Zeilen, in denen Kreisdaten stehen ("Satzart 40")
      bl_code <- substr(line, 11, 12)
      kreis_code <- substr(line, 13, 15)
      kreis_name <- substr(line, 23, 72)
      kreis_name <- gsub("\\s+$", "", gsub("^\\s+", "", kreis_name))
      kreise_data <- rbind(
        kreise_data,
        data.frame(
          Bundesland_Code = bl_code,
          Kreis_Code = kreis_code,
          Kreis_Name = kreis_name,
          stringsAsFactors = FALSE
        )
      )
    } else if (startsWith(line, "60")) {
      #Zeilen, in denen Gemeindedaten stehen ("Satzart 60")
      gkz_lang <- substr(line, 11, 19)
      gkz_kurz <- substr(line, 13, 19)
      bl_code <- substr(line, 11, 12)
      kreis_code <- substr(line, 13, 15)
      gemeinde_name <- substr(line, 23, 72)
      gemeinde_name <- gsub("\\s+$", "", gsub("^\\s+", "", gemeinde_name))
      gemeinden_data <- rbind(
        gemeinden_data,
        data.frame(
          Gemeindekennziffer_lang = gkz_lang,
          Gemeindekennziffer_kurz = gkz_kurz,
          Bundesland_Code = bl_code,
          Kreis_Code = kreis_code,
          Gemeinde_Name = gemeinde_name,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(list(kreise = kreise_data, gemeinden = gemeinden_data))
}