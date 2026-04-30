library(tidyverse)
# Funktion zum Extrahieren der Daten
extract_data <- function(lines) {
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

# Lese die Datei
lines <- readLines("./data/bundesweit/GV100AD_30112025.txt", encoding = "UTF-8")

# Extrahiere die Daten
extracted_data <- extract_data(lines)

# Speichere die Daten in CSV-Dateien
write.table(
  extracted_data$kreise,
  "./data/bundesweit/kreise_daten.csv",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8",
  sep = ";"
)
write.table(
  extracted_data$gemeinden,
  "./data/bundesweit/gemeinden_daten.csv",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8",
  sep = ";"
)

rm(extract_data, lines, extracted_data)
