# Instaliranje i učitavanje potrebne pakete
#install.packages(c("readr", "tm", "quanteda", "tidyverse"))
#install.packages("topicmodels")
library(readr)
library(tm)
library(topicmodels)
library(quanteda)
library(tidyverse)

# Učitavanje podatke
sport <- read_csv("sport_podaci.csv")
umetnost <- read_csv("umetnost_podaci.csv")
tehnologija <- read_csv("tehnologija_podaci.csv")
priroda <- read_csv("priroda_podaci.csv")
drustvo <- read_csv("drustvo_podaci.csv")

# Funkcija za pripremu podataka
preprocess_data <- function(data) {
  corpus <- Corpus(VectorSource(data$text)) # Pretpostavka da se tekst nalazi u koloni 'text'
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <- DocumentTermMatrix(corpus)
  return(dtm)
}

# Priprema podataka
sport_dtm <- preprocess_data(sport)
umetnost_dtm <- preprocess_data(umetnost)
tehnologija_dtm <- preprocess_data(tehnologija)
priroda_dtm <- preprocess_data(priroda)
drustvo_dtm <- preprocess_data(drustvo)

# Funkcija za kreiranje LDA modela
create_lda_model <- function(dtm, k) {
  lda <- LDA(dtm, k = k)
  return(lda)
}

# Kreiranje modela LDA za različite brojeve tema
models_sport <- lapply(c(2, 3, 4, 5), function(k) create_lda_model(sport_dtm, k))
models_umetnost <- lapply(c(2, 3, 4, 5), function(k) create_lda_model(umetnost_dtm, k))
models_tehnologija <- lapply(c(2, 3, 4, 5), function(k) create_lda_model(tehnologija_dtm, k))
models_priroda <- lapply(c(2, 3, 4, 5), function(k) create_lda_model(priroda_dtm, k))
models_drustvo <- lapply(c(2, 3, 4, 5), function(k) create_lda_model(drustvo_dtm, k))

# Funkcija za izračunavanje perplexity
calculate_perplexity <- function(lda_model, dtm) {
  perplexity(lda_model, newdata = dtm)
}


# Izračunavanje perplexity za svaki model
perplexity_sport <- sapply(models_sport, function(model) calculate_perplexity(model, sport_dtm))
perplexity_umetnost <- sapply(models_umetnost, function(model) calculate_perplexity(model, umetnost_dtm))
perplexity_tehnologija <- sapply(models_tehnologija, function(model) calculate_perplexity(model, tehnologija_dtm))
perplexity_priroda <- sapply(models_priroda, function(model) calculate_perplexity(model, priroda_dtm))
perplexity_drustvo <- sapply(models_drustvo, function(model) calculate_perplexity(model, drustvo_dtm))

# Kreiranje dataframe za vizualizaciju
perplexity_df <- tibble(
  Domain = rep(c("Sport", "Umetnost", "Tehnologija", "Priroda", "Drustvo"), each = 4),
  Num_Topics = rep(c(2, 3, 4, 5), 5),
  Perplexity = c(perplexity_sport, perplexity_umetnost, perplexity_tehnologija, perplexity_priroda, perplexity_drustvo)
)

# Filtriranje eventualne NA vrednosti
perplexity_df <- perplexity_df %>%
  filter(!is.na(Perplexity))


# Histogram za perplexity
plot <- ggplot(perplexity_df, aes(x = Num_Topics, y = Perplexity, fill = Domain)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Perplexity za različite brojeve tema po domenima",
       x = "Broj Tema",
       y = "Perplexity") +
  theme_minimal()

# Čuvanje grafikona kao .png datoteku
ggsave(filename = "perplexity_histogram.png", plot = plot, width = 10, height = 6, dpi = 300)
