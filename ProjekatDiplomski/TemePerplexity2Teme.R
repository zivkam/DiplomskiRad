# Učitaj potrebne biblioteke
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

# Funkcija za obradu jedne teme i izračunavanje perplexity
process_topic <- function(data, topic_name, num_topics = 2) {  
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
  dtm <<- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))  # Globalna promenljiva
  non_empty_docs <- row_sums(dtm) > 0
  
  # Uklanjanje praznih dokumenata iz DTM i data
  dtm <<- dtm[non_empty_docs, ]  # Globalna promenljiva
  data <- data[non_empty_docs, ]
  
  # Primena LDA modela
  lda_model <<- LDA(dtm, k = num_topics, control = list(seed = 1234))  # Globalna promenljiva
  
  # Izračunavanje perplexity
  perplexity_value <- perplexity(lda_model, newdata = dtm)
  return(perplexity_value)
}

# Učitavanje i obrada podataka za svaku temu
topic_data <- list(
  sport = read_csv("sport_podaci.csv"),
  umetnost = read_csv("umetnost_podaci.csv"),
  tehnologija = read_csv("tehnologija_podaci.csv"),
  priroda = read_csv("priroda_podaci.csv"),
  drustvo = read_csv("drustvo_podaci.csv")
)

# Inicijalizacija vektora za čuvanje perplexity vrednosti
perplexity_results <- data.frame(Tema = character(), Perplexity = numeric(), stringsAsFactors = FALSE)

# Obrada za sve teme i prikupljanje perplexity rezultata
for (topic_name in names(topic_data)) {
  perplexity_value <- process_topic(topic_data[[topic_name]], topic_name)
  perplexity_results <- rbind(perplexity_results, data.frame(Tema = topic_name, Perplexity = perplexity_value))
}

# Kreiranje grafikona za perplexity vrednosti
ggplot(perplexity_results, aes(x = Tema, y = Perplexity, fill = Tema)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Perplexity vrednosti za svaku temu", x = "Tema", y = "Perplexity") +
  theme_minimal()

# Snimanje grafikona kao PNG fajla
ggsave(filename = "perplexity_results.png", width = 10, height = 6)
