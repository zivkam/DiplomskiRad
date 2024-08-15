# Uvoz potrebnih paketa
library(readr)
library(dplyr)
library(writexl)


# Funkcija za učitavanje CSV fajla i dodavanje kolone sa nazivom teme
read_and_label <- function(file, tema) {
  # Proverite da li fajl postoji pre nego što ga učitate
  if (!file.exists(file)) {
    stop(paste("Fajl", file, "ne postoji."))
  }
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

# Kreiranje lista sa podacima za svaki tab u Excel fajlu
excel_data <- list()

for (group_name in names(groups)) {
  group_files <- groups[[group_name]]
  combined_data <- bind_rows(lapply(group_files, function(file) read_and_label(file, group_name)))
  excel_data[[group_name]] <- combined_data
}

# Snimanje u Excel fajl
write_xlsx(excel_data, "svi_podaci.xlsx")