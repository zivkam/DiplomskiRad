# Uvoz potrebnih paketa
library(rvest)
library(httr)
library(readr)

library(dplyr)
library(readr)

# Funkcija za učitavanje CSV fajla i dodavanje kolone sa nazivom teme
read_and_label <- function(file, tema) {
  data <- read_csv(file)
  data <- data %>% mutate(tema = tema)
  return(data)
}

# Definisanje grupa
groups <- list(
  sport = c("evropsko-prvenstvo-u-fudbalu-nemacka-2024.csv", "formula-1.csv", "nba.csv", "tenis.csv", "teret-n.csv"),
  umetnost = c("dakle-filmovi.csv", "klasicna-muzika.csv", "poezija.csv", "foto-za-sve-ljubitelje-fotografije.csv"),
  tehnologija = c("koju-igru-sledecu-da-skinem.csv", "ucenje-programiranja-za-ljude-koji-to-nisu-studirali.csv"),
  priroda = c("kinologija.csv", "psi-vs-macke.csv"),
  drustvo = c("futurizam.csv", "ljudski-vek-i-besmrtnost.csv", "sta-danas-rucavate.csv", "porodicne-vrijednosti.csv", "najjace-zastave-i-grbovi.csv")
)

# Kombinovanje i snimanje po grupama
for (group_name in names(groups)) {
  group_files <- groups[[group_name]]
  combined_data <- bind_rows(lapply(group_files, function(file) read_and_label(file, group_name)))
  write_excel_csv(combined_data, paste0(group_name, "_podaci.csv"))
}











