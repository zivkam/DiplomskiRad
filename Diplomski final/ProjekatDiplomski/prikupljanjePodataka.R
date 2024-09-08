library(rvest)
library(httr)
library(readr)

# Funkcija za skrejpovanje jedne strane
scrape_page <- function(url) {
  page <- GET(url)
  
  if (status_code(page) == 200) {
    content <- content(page, "text")
    parsed_page <- read_html(content)
    
    # Pronalaženje svih poruka (postova) na stranici
    posts <- parsed_page %>%
      html_nodes(".comment-body") %>%
      html_text(trim = TRUE)
    
    return(posts)
  } else {
    warning(paste("Greška pri učitavanju stranice:", status_code(page)))
    return(NULL)
  }
}

# Funkcija za generisanje naziva CSV fajla na osnovu URL-a
generate_csv_name <- function(url) {
  # Izvlačenje naziva teme iz URL-a
  tema <- sub(".*teme/\\d+-([a-zA-Z0-9-]+)\\?.*", "\\1", url)
  # Vraćanje naziva CSV fajla
  return(paste0(tema, ".csv"))
}

# Lista URL-ova za skrejpovanje
urls <- c(
  'https://vukajlija.com/forum/teme/47737-sta-danas-rucavate?strana=1',
  'https://vukajlija.com/forum/teme/28757-koju-igru-sledecu-da-skinem?strana=1',
  'https://vukajlija.com/forum/teme/74830-ucenje-programiranja-za-ljude-koji-to-nisu-studirali?strana=1',
  'https://vukajlija.com/forum/teme/76581-evropsko-prvenstvo-u-fudbalu-nemacka-2024?strana=1',
  'https://vukajlija.com/forum/teme/1913-formula-1?strana=1',
  'https://vukajlija.com/forum/teme/43219-nba?strana=1',
  'https://vukajlija.com/forum/teme/57598-tenis?strana=1',
  'https://vukajlija.com/forum/teme/3511-teret-n?strana=1',
  'https://vukajlija.com/forum/teme/25623-poezija?strana=1',
  'https://vukajlija.com/forum/teme/922-dakle-filmovi?strana=1',
  'https://vukajlija.com/forum/teme/10844-klasicna-muzika?strana=1',
  'https://vukajlija.com/forum/teme/459-foto-za-sve-ljubitelje-fotografije?strana=1',
  'https://vukajlija.com/forum/teme/75804-ljudski-vek-i-besmrtnost?strana=1',
  'https://vukajlija.com/forum/teme/76470-najjace-zastave-i-grbovi?strana=1',
  'https://vukajlija.com/forum/teme/74523-psi-vs-macke?strana=1',
  'https://vukajlija.com/forum/teme/30796-kinologija?strana=1',
  'https://vukajlija.com/forum/teme/75191-futurizam?strana=1',
  'https://vukajlija.com/forum/teme/76591-porodicne-vrijednosti?strana=1'
)

# Iteracija kroz sve URL-ove
for (url in urls) {
  # Inicijalizacija liste za skladištenje svih postova
  all_posts <- list()
  
  # Iteracija kroz sve strane (pretpostavimo da svaka tema ima maksimalno 67 strana)
  for (i in 1:55) {
    page_url <- sub("\\?strana=\\d+", paste0("?strana=", i), url)
    posts <- scrape_page(page_url)
    
    if (!is.null(posts)) {
      all_posts <- c(all_posts, posts)
    } else {
      break # Prekida petlju ako je neka stranica prazna ili ima grešku
    }
  }
  
  # Pretvaranje liste u data frame
  all_posts_df <- data.frame(post = unlist(all_posts), stringsAsFactors = FALSE)
  
  # Generisanje naziva CSV fajla
  csv_file_name <- generate_csv_name(url)
  
  # Skladištenje rezultata u CSV fajl sa UTF-8 enkodiranjem pomoću readr paketa
  write_excel_csv(all_posts_df, csv_file_name)
  
  print(paste("Skrejpovanje za URL", url, "je završeno i podaci su sačuvani u", csv_file_name, "sa UTF-8 enkodiranjem"))
}
