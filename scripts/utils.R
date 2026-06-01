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

#' Extract data concerning kreise from a large fixed format file
#'
#' @param lines imported via read_lines
#'
#' @returns kreise_data a data frame
#' @export
#'
#' @examples
extract_kreise <- function(lines) {
  kreise_data <- data.frame(
    bundesland_code = character(),
    kreis_code = character(),
    kreis_name = character(),
    stringsAsFactors = FALSE
  )
  
  for (line in lines) {
    if (startsWith(line, "40")) {
      #Zeilen, in denen Kreisdaten stehen ("Satzart 40")
      kreise_data <- rbind(
        kreise_data,
        data.frame(
          bundesland_code = substr(line, 11, 12),
          kreis_code = substr(line, 13, 15),
          kreis_name = substr(line, 23, 72),# %>%gsub("\\s+$", "", gsub("^\\s+", "", .)),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(kreise_data)
}


#' Extract data concerning gemeinden from a large fixed format file
#'
#' @param lines 
#'
#' @returns gemeinden_data a dataframe
#' @export
#'
#' @examples
extract_gemeinden <- function(lines) {
  gemeinden_data <- data.frame(
    gemeinde_kennziffer   = character(),
    gemeinde_bezeichnung   = character(),
    gemeinde_kennzeichnung = character(),
    bevoelkerung      = numeric(),
    flaeche = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (line in lines) {
    if (startsWith(line, "60")) {
      #Zeilen, in denen Gemeindedaten stehen ("Satzart 60")
      gemeinden_data <- rbind(
        gemeinden_data,
        data.frame(
          gemeinde_kennziffer   = substr(line, 11, 18),
          gemeinde_bezeichnung   = substr(line, 23, 72),
          gemeinde_kennzeichnung = substr(line, 123, 124),
          bevoelkerung      = substr(line, 140, 150) %>% as.numeric(),
          flaeche = substr(line, 129, 139) %>% as.numeric(),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  gemeinden_data <- gemeinden_data %>%
    mutate(
      gemeinde_kennzeichnung = replace_values(
        gemeinde_kennzeichnung,
        "60" ~ "Markt",
        "61" ~ "Kreisfreie Stadt",
        "62" ~ "Stadtkreis",
        "63" ~ "Stadt",
        "64" ~ "Kreisangehörige Gemeinde",
        "65" ~ "gemeindefreies Gebiet, bewohnt",
        "66" ~ "gemeindefreies Gebiet, unbewohnt",
        "67" ~ "große Kreisstadt"
      )
    ) %>%
    mutate(gemeinde_bezeichnung = str_squish(gemeinde_bezeichnung)) 
  return(gemeinden_data)
}


#' Use given age fractions to reduce both exposed and population in each row
#'
#' @param dat data frame with population, exposed and fractions
#'
#' @returns same data frame with population, exposed and fractions multiplied
#' @export
#'
#' @examples multiply_with_age_fraction(dat_exp_ERF)
multiply_with_age_fraction <- function(dat){
  dat |>
    mutate(
      exponierte =
        replace_when(
          exponierte,
          population_type %in% c("adults") ~ exponierte * fraction_adults_from_18_years,
          population_type %in% c("children") ~ exponierte * fraction_children_7_17_years
        ),
      bevoelkerung =
        replace_when(
          bevoelkerung,
          population_type %in% c("adults") ~ bevoelkerung * fraction_adults_from_18_years,
          population_type %in% c("children") ~ bevoelkerung * fraction_children_7_17_years
        )
    )
}


#' Calculate impact of absolute risk endpoints
#'
#' @param dat a dataframe with risk_type, threshold, ERF, exponierte,l_zentral,threshold,gemeinde_kennziffer,Bundesland_Code,DW
#'
#' @returns a dataframe with detailed infos of input and outcome
#' @export
#'
#' @details check data (single exposure scenario and single ERF)
#' then pass it to healthiar::attribute_health.
#' The information of source,metric,outcome,datenquelle,kartierungsumfang is piped through using the info field.
#' 
#' @examples
calc_macro_ar_impact <- function(dat) {
  rt_thr_ERF_df<-dat %>%
    select(risk_type, threshold, ERF) %>%
    unique()
  
  ifelse(nrow(rt_thr_ERF_df) > 1,
         stop(
           "Function calc_macro_ar_impact expects a data frame with a single ERF function!",
           rt_thr_ERF_df
         ),
         NA)
  lzentr_gembez_df<-dat %>%
    group_by(gemeinde_kennziffer,l_zentral,source) %>%
    summarise(n=n())
  ifelse( lzentr_gembez_df$n %>% 
            max(.) > 1,
          stop(
            "Function calc_macro_ar_impact expects a data frame with a single exposure scenario!",
            lzentr_gembez_df %>% filter(n>1)
          ),
          NA)
  dat %>%
    {
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        pop_exp = .$exponierte,
        exp_central = .$l_zentral,
        cutoff_central = first(.$threshold),
        erf_eq_central = first(.$ERF),
        geo_id_micro = .$gemeinde_kennziffer,
        geo_id_macro = .$Bundesland_Code,
        dw_central = .$DW,
        duration_central = 1,
        info = select(.,source,metric,outcome,datenquelle,kartierungsumfang)
      )
    } %>%
    .$health_detailed %>%
    .$results_raw %>%
    mutate(
      source = info_column_1,
      metric = info_column_2,
      outcome = info_column_3,
      datenquelle = info_column_4,
      kartierungsumfang=info_column_5,
      .keep="unused"
    )
}
