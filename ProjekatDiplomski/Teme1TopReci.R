library(dplyr)
library(tm)
library(slam)
library(tidytext)
library(ggplot2)
library(readr)
library(topicmodels)

# Lista stop reči za srpski jezik
stopwords_sr <- c(
  " -","- ", "i", "da", "je", "u", "se", "na", "su", "za", "s", "nije", "sam", "si", "mi", "koji", "koja", "koje", "koju",
  "kako", "šta", "što", "ovo", "to", "ali", "od", "do", "sa", "o", "kao", "kada", "ga", "te", "me", "bi", "bila",
  "bili", "bio", "biti", "ili", "tu", "ta", "taj", "ta", "to", "ih", "njih", "njemu", "nje", "njoj", "njima", "njihov",
  "njihova", "njihovo", "moj", "moja", "moje", "tvoj", "tvoja", "tvoje", "njegov", "njegova", "njegovo", "naš", "naša",
  "naše", "vaš", "vaša", "vaše", "mene", "meni", "tebe", "tebi", "nas", "nama", "vas", "vama", "ko", "šta", "koje",
  "koju", "kojima", "čije", "čija", "čije", "ovaj", "ova", "ovo", "onaj", "ona", "ono", "oni", "one", "ona", "bio",
  "bila", "bili", "bilo", "će", "hoće", "biće", "mogu", "može", "ne", "da", "li", "već", "jer", "pa", "takođe",
  "gde", "tamo", "ovde", "iz", "preko", "sve", "svaki", "sva", "svako", "svi", "svakog", "svega", "sebe", "sebi",
  "sebe", "ovaj", "onaj", "ta", "ovo", "ono", "možda", "čak", "ipak", "nekad", "uvek", "nikad", "sad", "sada",
  "posle", "pre", "kad", "dok", "što", "svi", "neki", "neka", "neko", "nijedan", "nijedna", "nijedno", "znam", "samo","treba",
  "sto", "tako","sta", "samo", "onda", "malo", "ima", "ako", "nisam", "nema", "još","nešto","ima", "baš","dto","dta","danas",
  "zbog", "zato", "više", "toga", "nego", "mnogo","bude","bih", "ima", "bi", "kurac", "ove", "dobro", "bude", "bas","jel",
  "jos", "moze","vec", "vise", "the", "jedan", "httpwwwimdbcomtitlett", "smo", "neke","nesto", "mora","koliko", "govna", "evo",
  "you", "neku", "jebe", "govna", "for", "and", "zna", "jebem", "haha","god", "bre", "that", "not", "ću", "nisu", "nista", "mozda",
  "mislim", "bez", "nisu", "imas", "ništa", "hvala", "imas", "with", "kod", "mozes", "bad", "možeš", "opet", "imao", "nedto","imam",
  "vide", "jbg", "mada", "njega", "imaju", "imaš", "znači","toliko", "bolje", "hahaha", "lolo","joj","svoje","par","isto","nam","neće","oko","itd"
)

# Funkcija za normalizaciju ćirilice i latinice
normalize_text <- function(text) {
  text <- tolower(text)
  # Zamena ćirilice latinicom
  cir_to_lat <- function(text) {
    chartr("абвгдђежзијклмнопрстћуфхцчџш",
           "abvgdđežzijklmnoprstćufhccčdžš", text)
  }
  text <- sapply(text, cir_to_lat)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords_sr)
  text <- stripWhitespace(text)
  return(text)
}

# Funkcija za obradu jedne teme
process_topic <- function(data, topic_name, num_topics = 2) {  # Postavljeno na najmanje 2
  # Normalizacija teksta
  data$post <- sapply(data$post, normalize_text)
  
  # Kreiranje korpusa
  corpus <- Corpus(VectorSource(data$post))
  
  # Čišćenje teksta
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords_sr)
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Kreiranje Document-Term Matrix sa sparse matricom
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))
  
  # Identifikovanje praznih dokumenata
  non_empty_docs <- row_sums(dtm) > 0
  
  # Uklanjanje praznih dokumenata iz DTM i data
  dtm <- dtm[non_empty_docs, ]
  data <- data[non_empty_docs, ]
  
  # Primena LDA modela
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Prikazivanje tema
  topics <- tidy(lda_model, matrix = "beta")
  
  # Prikaz top 10 reči za svaku temu
  top_terms <- topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Vizualizacija top reči za svaku temu
  p <- ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", labeller = label_both) +
    coord_flip() +
    labs(title = paste("Top 10 reči za temu", topic_name), x = "Reč", y = "Beta")
  
  # Snimanje grafika kao PNG fajla
  ggsave(filename = paste0(topic_name, "_top_terms.png"), plot = p, width = 10, height = 8)
  
  # Prikazivanje dokumenata i pripadajućih tema
  doc_topics <- tidy(lda_model, matrix = "gamma")
  doc_topics <- doc_topics %>%
    group_by(document) %>%
    slice_max(gamma, n = 1) %>%
    ungroup()
  
  # Uparivanje dokumenata sa originalnim podacima
  data$topic <- doc_topics$topic
  
  # Snimanje rezultata u CSV fajl
  write_csv(data, paste0(topic_name, "_sa_temama.csv"))
}

# Učitavanje i obrada podataka za svaku temu
sport <- read_csv("sport_podaci.csv")
umetnost <- read_csv("umetnost_podaci.csv")
tehnologija <- read_csv("tehnologija_podaci.csv")
priroda <- read_csv("priroda_podaci.csv")
drustvo <- read_csv("drustvo_podaci.csv")

process_topic(sport, "sport")
process_topic(umetnost, "umetnost")
process_topic(tehnologija, "tehnologija")
process_topic(priroda, "priroda")
process_topic(drustvo, "drustvo")
