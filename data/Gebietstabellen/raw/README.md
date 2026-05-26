# Administrative & Geographic Reference Data (Gebietstabellen)

## Contents

- **`GV100AD_31122022.txt`/`GV100AD_30112025.txt`**: Destatis municipality reference (fixed-width format)
  - Source: [Statistisches Bundesamt](https://www.destatis.de/)
  - Parsed in `Expositionsdaten_vorbereiten.qmd` to extract names, population, area
  
- **`DE_VG250EW.gpkg`**: BKG vector geometries (GIS boundaries)
  - Source: [Bundesamt für Kartographie und Geodäsie](https://gdz.bkg.bund.de/index.php/default/open-data/verwaltungsgebiete-1-250-000-stand-01-01-vg250-01-01.html)
  - Contains: municipality, district, state boundaries together with population, Names and AGS
  - Used for spatial visualization in `.qmd` files

- **`kreise_daten.csv`, `gemeinden_daten.csv`**: Reference tables
  - Manual or external source (clarify if needed)
  - Used to join names/codes

## Processing

These files are read as-is in `Expositionsdaten_vorbereiten.qmd` and `healthiar_hessen.qmd`.
No separate processing script yet.

## Future

If this data grows, consider extracting a `scripts/02_prepare_areas.R` to:
- Parse GV100AD
- Standardize all geocodes
- Export clean area reference as `data/Gebietstabellen/processed/areas_clean.parquet` or `.csv`