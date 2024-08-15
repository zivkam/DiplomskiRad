# Proveri i instaliraj potrebne pakete
if (!require("wordcloud")) {
  install.packages("wordcloud")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("tm")) {
  install.packages("tm")
}
if (!require("readr")) {
  install.packages("readr")
}

# Učitaj pakete
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tm)
library(readr)

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

# Funkcija za generisanje i čuvanje oblaka reči
generate_wordcloud <- function(data, topic_name) {
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
  
  # Kreiranje Document-Term Matrix
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))
  
  # Generisanje frekvencija reči
  term_table <- as.matrix(dtm)
  word_freq <- sort(colSums(term_table), decreasing = TRUE)
  
  # Postavljanje boja
  pal <- brewer.pal(8, "Dark2")
  
  # Kreiranje oblaka reči
  png(filename = paste0(topic_name, "_wordcloud.png"), width = 800, height = 600)
  wordcloud(names(word_freq), freq = word_freq, min.freq = 2, max.words = 200, random.order = FALSE, colors = pal)
  dev.off()
}

# Učitavanje i obrada podataka za svaku temu
sport <- read_csv("sport_podaci.csv")
umetnost <- read_csv("umetnost_podaci.csv")
tehnologija <- read_csv("tehnologija_podaci.csv")
priroda <- read_csv("priroda_podaci.csv")
drustvo <- read_csv("drustvo_podaci.csv")

generate_wordcloud(sport, "sport")
generate_wordcloud(umetnost, "umetnost")
generate_wordcloud(tehnologija, "tehnologija")
generate_wordcloud(priroda, "priroda")
generate_wordcloud(drustvo, "drustvo")
