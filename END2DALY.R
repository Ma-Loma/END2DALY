rm(list = ls())
# library(remotes)
# remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)

library(tidyverse)
library(healthiar)
library(readxl)
library(janitor)
library(purrr)

## ETCHE Input lesen

functionsETCHE <- read_excel("data/functionsETCHE.xlsx") %>%
  mutate(ERF = str_replace_all(ERF, "threshold", as.character(threshold))) %>%
  mutate(DW = as.numeric(DW))

## HLNUG Expositionsdaten lesen

dat_str <- read_excel(
  "data/ULK2022StatistikGesamt_20240704.xlsx",
  sheet = "Straßenlärm",
  skip = 2,
  n_max = 425,
  col_names = read_lines("data/ColNamesStr_ULK2022StatistikGesamt_20240704.txt")
)

# dat_str %>% names() %>% 
#   write_lines("data/ColNamesStr_ULK2022StatistikGesamt_20240704.txt")


dat_str_BW <- read_excel(
  "data/andere BL/BW_Belastungsstatistik_2022.xlsx",
  sheet = "Hauptverkehrsstraßen",
  skip = 4,
  n_max = 779,
  col_names = read_lines("data/andere BL/ColNamesStr_BW_Belastungsstatistik_2022.txt")
  )

dat_flug <- read_excel(
  "data/ULK2022StatistikGesamt_20240704.xlsx",
  sheet = "Fluglärm",
  skip = 1,
  n_max = 35,
  col_names = read_lines("data/ColNamesFlug_ULK2022StatistikGesamt_20240704.txt")
)

dat_flug_BW <- read_excel(
  "data/andere BL/BW_Belastungsstatistik_2022.xlsx",
  sheet = "Flughafen Stuttgart",
  skip = 4,
  n_max = 10,
  col_names = read_lines("data/andere BL/ColNamesFlug_BW_Belastungsstatistik_2022.txt")
)


dat_ind <- read_excel(
  "data/ULK2022StatistikGesamt_20240704.xlsx",
  sheet = "Industrielärm",
  skip = 1,
  n_max = 6
) %>%
  janitor::clean_names()

## Gemeinde und Kreisnamen laden
kreise_daten <- read_delim(
  "./data/bundesweit/kreise_daten.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
gemeinden_daten <- read_delim(
  "data/bundesweit/gemeinden_daten.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

## Expositionsdaten schön machen

lang_machen <- function(data, welche_exp) {
  data %>%
    select(contains("gemeinde") | contains("belasteter")) %>%
    setNames(str_replace(names(.), "_bis_[0-9]*", "")) %>%
    pivot_longer(
      starts_with("anzahl"),
      names_sep = "_ab_",
      names_to = c("metric", "L_Untergrenze"),
      values_to = "population"
    ) %>%
    mutate(
      L_Untergrenze = as.numeric(L_Untergrenze),
      L_central = L_Untergrenze + 2,
      .keep = "unused"
    ) %>%
    mutate(kreis = str_sub(gemeinde_kennziffer, start = 1L, end = 3L)) %>%
    mutate(metric = str_remove(metric, "anzahl_belasteter_")) %>%
    mutate(metric = str_replace_all(metric, "l_night", "lnight")) %>%
    #    pivot_wider(names_from = metric, values_from = population) %>%
    replace_na(list(population = 0)) %>%
    mutate(source = welche_exp)
}

#use the source names as indicated in the ERF-Database
dat_l <- lang_machen(dat_str, "road") %>%
  bind_rows(lang_machen(dat_flug, "air")) %>%
  bind_rows(lang_machen(dat_ind, "industry")) %>%
  left_join(
    kreise_daten %>%
      filter(Bundesland_Code == "06") %>% #hier nur Hessen
      select(Kreis_Code, Kreis_Name) %>%
      rename(kreis = Kreis_Code, name_kreis = Kreis_Name),
    by = "kreis"
  )

# Funktion zur Berechnung der Gesundheitsauswirkungen für eine Expositions-ERF-Kombination
calc_kreis_ar_impact <- function(dat) {
  ifelse(dat %>%
           select(risk_type, threshold, ERF) %>%
           unique() %>%
           nrow() > 1,
         stop(
           "Function calc_kreis_ar_impact expects a data frame with a single ERF function!"
         ),
         NA)
  dat %>%
    {
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        pop_exp = .$population,
        exp_central = .$L_central,
        cutoff_central = first(.$threshold),
        erf_eq_central = first(.$ERF),
        geo_id_micro = .$name_stadt_gemeinde,
        geo_id_macro = .$name_kreis,
        dw_central = .$DW,
        duration_central = 1
      )
    } %>%
    .$health_detailed %>%
    .$results_raw %>%
    mutate(
      source = dat$source[1],
      metric = dat$metric[1],
      outcome = dat$outcome[1]
    )
}

################### Visualisierungen der Expositionsdaten

dir.create("plots", showWarnings = F)

#Übersicht Exposition nach Kreis und Quelle
dat_l %>%
  ggplot(aes(x = L_Untergrenze, y = population, fill = name_kreis)) +
  geom_col() +
  facet_grid(metric ~ source) +
  labs(title = "Lärmbelastung in Hessen nach Kreisen, Quelle und Lärmmetrik", x =
         "Lärmpegel in dB", y = "Anzahl exponierter Personen") +
  guides(fill = guide_legend(title = "Kreis"))
ggsave("plots/exposition_hessen.png")

#Übersicht Exposition im Kreis GG
dat_l %>%
  filter(name_kreis == "Groß-Gerau") %>%
  ggplot(aes(x = L_Untergrenze, y = population, fill = name_stadt_gemeinde)) +
  geom_col(just = 0) +
  facet_grid(metric ~ source) +
  labs(title = "Lärmbelastung im Kreis Groß Gerau nach Kommunen, Quelle und Lärmmetrik", x =
         "Lärmpegel in dB", y = "Anzahl exponierter Personen") +
  guides(fill = guide_legend(title = "Kommune"))
ggsave("plots/exposition_Kreis GG.png")

################### *händisches* Berechnen
################# einzelner (!)
################# Expositions-Wirkungen Paare


#Zusammenführen Expositionsdaten und ERF-Funktionen
## (Wahrscheinlich keine gute Idee)
dat_exp_ERF <- dat_l %>%
  left_join(functionsETCHE,
            by = c("source", "metric"),
            relationship = "many-to-many") %>%
  mutate(threshold = replace_na(threshold, 0))

functionsETCHE$outcome %>%
  unique()

#Berechnung der Gesundheitsauswirkungen für Straßenlärm und hohe Lärmbelästigung
HA_road_res <- dat_exp_ERF %>%
  filter(source == "road") %>%
  filter(metric == "lden") %>%
  filter(outcome == "High noise annoyance") %>%
  # nth(1) %>%
  # .$ERF %>%
  # as.character()%>%
  {
    calc_kreis_ar_impact(.)
  }
#Berechnung der Gesundheitsauswirkungen für Straßenlärm und hohe Schlafstörung
HSD_road_res <- dat_exp_ERF %>%
  filter(source == "road") %>%
  filter(metric == "lnight") %>%
  filter(outcome == "High sleep disturbance") %>%
  # nth(1) %>%
  # .$ERF %>%
  # as.character()%>%
  {
    calc_kreis_ar_impact(.)
  }


#Berechnung der Gesundheitsauswirkungen für Fluglärm und hohe Lärmbelästigung
HA_air_res <- dat_exp_ERF %>%
  filter(source == "air") %>%
  filter(metric == "lden") %>%
  filter(outcome == "High noise annoyance") %>%
  # nth(1) %>%
  # .$ERF %>%
  # as.character()%>%
  {
    calc_kreis_ar_impact(.)
  }

#HSD Fluglärm
HSD_air_res <- dat_exp_ERF %>%
  filter(source == "air") %>%
  filter(metric == "lnight") %>%
  filter(outcome == "High sleep disturbance") %>%
  # nth(1) %>%
  # .$ERF %>%
  # as.character()%>%
  {
    calc_kreis_ar_impact(.)
  }

# Reading comprehension - Fluglärm
Reading_air_res <- dat_exp_ERF %>%
  filter(source == "air") %>%
  filter(metric == "lden") %>%
  filter(outcome == "Reading Comprehension") %>%
  # nth(1) %>%
  # .$ERF %>%
  # as.character()%>%
  {
    calc_kreis_ar_impact(.)
  }


#Hier händisches Zusammenführen von Teilergebnisses Straßen- und Fluglärm


#Kreis GG: 433
#Aufteilung HA Straßenlärm im Kreis GG
HA_road_res %>%
  filter(geo_id_macro == "Groß-Gerau") %>%
  ggplot(aes(x = "", fill = geo_id_micro, y = impact)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Verteilung DALY durch Straßenlärm-bedingte hohe Lärmbelästigung im Kreis Groß Gerau")
ggsave("plots/HA_road_KreisGG.png")


outcome_all <-
  HA_road_res %>%
  bind_rows(HSD_road_res) %>%
  bind_rows(HA_air_res) %>%
  bind_rows(HSD_air_res) %>%
  bind_rows(Reading_air_res)

str_detect(outcome_all$geo_id_macro[1], "Groß-Gerau" | "Frankfurt")

kagzrm_kreise <- c("Darmstadt-Dieburg",
                   "Groß-Gerau",
                   "Main-Taunus-Kreis",
                   "Offenbach")
kagzrm_gemeinden <- scan("data/kagzrm_gemeinden.txt", what = "character", sep = "\r")

outcome_all$geo_id_macro %>% unique()
outcome_all %>%
  #filter( startsWith(geo_id_macro,"43")) %>%
  filter(geo_id_macro %in% kagzrm_kreise) %>%
  ggplot(aes(x = source, fill = geo_id_macro, y = impact)) +
  guides(fill = guide_legend(title = "Kreis")) +
  geom_col() +
  facet_grid(. ~ outcome) +
  labs(title = "Verteilung DALY durch einige Straßen/Fluglärm-bedingte Gesundheitsauswirkungen für die Kreise der KAGZRM", x = "Lärmquelle", y = "Attributable Krankheitslast [DALY]")

ggsave("plots/DALY_FlugStraße_KreiseKAGZRM.png")


############################ Hier wirds wild; noch nicht abgeschlossenes Automatisierungstesten...

functionsETCHE |>
  pmap(function(outcome,
                risk_type,
                outcome_type,
                population_type,
                source,
                ERF,
                metric,
                threshold,
                reference) {
    data.frame(erg = ERF)
  })

bla <- functionsETCHE |>
  split(1:nrow(functionsETCHE)) |>
  map_vec(function(exposure_outcome) {
    dat_l %>%
      filter(source == exposure_outcome$source,
             metric == exposure_outcome$metric) %>%
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        pop_exp = .$population,
        exp_central = .$L_central,
        cutoff_central = exposure_outcome$threshold,
        erf_eq_central = exposure_outcome$ERF,
        geo_id_micro = .$name_stadt_gemeinde,
        geo_id_macro = .$kreis
      ) %>%
      .$health_detailed
  })

#
#     data.frame(
#         healthiar::attribute_health(
#           approach_risk = "absolute_risk",
#           pop_exp = 100,
#           exp_central = 50,
#           erf_eq_central = exposure_outcome$ERF
#         )$health_detailed$results_raw
#     )
#   })
#

functionsETCHE |>
  pmap(function(outcome,
                risk_type,
                outcome_type,
                population_type,
                source,
                ERF,
                metric,
                threshold,
                reference) {
    data.frame(erg = ERF)
  })


bla <- dat_exp_ERF %>%
  group_by(source, metric, outcome) %>%
  group_split()
bla %>%
  group_keys()

bla %>% reframe(
  population_exposed = sum(population),
  Anz_Expositionsbänder = n(),
  fkt = first(ERF),
  erg = calc_kreis_ar_impact(.)
)
length(bla)
bla[[2]] %>%
  select(source, metric, outcome) %>%
  unique()
bla %>%
  group_by(erg$approach_risk, erg$cutoff) %>%
  summarize(n = n())
