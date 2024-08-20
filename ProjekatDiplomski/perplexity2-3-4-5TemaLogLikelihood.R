# Učitaanje paketa
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

# Funkcija za obradu jedne teme i izračunavanje perplexity za različit broj tema
process_topic <- function(data, topic_name, num_topics) {  
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
  non_empty_docs <- row_sums(dtm) > 0
  
  # Uklanjanje praznih dokumenata iz DTM i data
  dtm <- dtm[non_empty_docs, ]
  data <- data[non_empty_docs, ]
  
  # Primena LDA modela
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Izračunavanje perplexity
  perplexity_value <- perplexity(lda_model, newdata = dtm)
  return(perplexity_value)
}

process_topic_logLik <- function(data, topic_name, num_topics) {
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
  non_empty_docs <- row_sums(dtm) > 0
  
  # Uklanjanje praznih dokumenata iz DTM i data
  dtm <- dtm[non_empty_docs, ]
  data <- data[non_empty_docs, ]
  
  # Primena LDA modela
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Izračunavanje log-likelihood
  logLik_value <- logLik(lda_model)
  return(as.numeric(logLik_value))
}

# Inicijalizacija vektora za čuvanje log-likelihood vrednosti
logLik_results <- data.frame(Tema = character(), NumTopics = numeric(), LogLikelihood = numeric(), stringsAsFactors = FALSE)


# Učitavanje i obrada podataka za svaku temu
topic_data <- list(
  sport = read_csv("sport_podaci.csv"),
  umetnost = read_csv("umetnost_podaci.csv"),
  tehnologija = read_csv("tehnologija_podaci.csv"),
  priroda = read_csv("priroda_podaci.csv"),
  drustvo = read_csv("drustvo_podaci.csv")
)

# Brojevi tema koje ćemo testirati
num_topics_list <- c(2, 3, 4, 5)

# Inicijalizacija vektora za čuvanje perplexity vrednosti
perplexity_results <- data.frame(Tema = character(), NumTopics = numeric(), Perplexity = numeric(), stringsAsFactors = FALSE)


# Obrada za sve teme i sve brojeve tema, i prikupljanje perplexity rezultata
for (topic_name in names(topic_data)) {
  for (num_topics in num_topics_list) {
    perplexity_value <- process_topic(topic_data[[topic_name]], topic_name, num_topics)
    perplexity_results <- rbind(perplexity_results, data.frame(Tema = topic_name, NumTopics = num_topics, Perplexity = perplexity_value))
    logLik_value <- process_topic_logLik(topic_data[[topic_name]], topic_name, num_topics)
    logLik_results <- rbind(logLik_results, data.frame(Tema = topic_name, NumTopics = num_topics, LogLikelihood = logLik_value))
    
  }
}

# Kreiranje grafikona za perplexity vrednosti
ggplot(perplexity_results, aes(x = interaction(Tema, NumTopics), y = Perplexity, fill = Tema)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Perplexity vrednosti za svaku temu i broj tema", x = "Tema i Broj Tema", y = "Perplexity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Snimanje grafikona kao PNG fajla
ggsave(filename = "perplexity_results_multiple_topics.png", width = 12, height = 6)



# Kreiranje grafikona za log-likelihood vrednosti
ggplot(logLik_results, aes(x = interaction(Tema, NumTopics), y = LogLikelihood, fill = Tema)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Log-Likelihood vrednosti za svaku temu i broj tema", x = "Tema i Broj Tema", y = "Log-Likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Snimanje grafikona kao PNG fajla
ggsave(filename = "logLik_results_multiple_topics.png", width = 12, height = 6)