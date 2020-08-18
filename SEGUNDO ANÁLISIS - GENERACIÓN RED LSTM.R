devtools::install_github("rstudio/tensorflow", force= TRUE)
devtools::install_github("rstudio/keras", force= TRUE)
devtools::install_github("rstudio/reticulate", force=TRUE)
install.packages("remotes")

install.packages("reticulate")
install_tensorflow(version = "2.0.0b1", method = "conda", envname = "r-reticulate")

library(reticulate)
library(tensorflow)
library(keras)
library(tidyverse)

sessionInfo()


 
########## MODELO LSTM ##########

# VARIABLES & CLEANSING of DATA

Videogames <-read_csv("C:/Users/marti/OneDrive/Documentos/D.Analytics/vgsales.csv")
Videogames <- Videogames %>% slice_head(n = 5000)

#cantidad de NA por variable
sapply(Videogames, function(x) sum(is.na(x)))
#cantidad de NULL por variable
sapply(Videogames, function(x) sum(is.null(x)))

#voy a convertir years de character a dobble
Videogames$Year <- as.integer(Videogames$Year)
str(Videogames)

#se perdieron 271 datos. 

#Eliminar las filas con NA
Videogames <-Videogames[!is.na(Videogames$Year),]

#crea la nueva variable nuemero de letras
Videogames <- mutate(Videogames, N_letras = nchar(Videogames$Name))


#DELIMITATION of VARIABLES & RESTRICTIONS

texto <- paste(Videogames$Name, collapse = " ")                                                                                                         
texto

# Nueva variable para trabajar

texto_min <- tolower(texto)
cat("Corpus length:", nchar(texto), "\n")

#Restrictions

mmaxlen <- 30
sstep <- 2
texto_indexes <- seq(1, nchar(texto_min) - mmaxlen, by = sstep)

sentences <- str_sub(texto_min, texto_indexes, texto_indexes + mmaxlen - 1)
next_chars <- str_sub(texto_min, texto_indexes + mmaxlen, texto_indexes + mmaxlen)
cat("Number of sequences: ", length(sentences), "\n")

# NUMERO DE LETRA ÚNICA

chars <- unique(sort(strsplit(texto_min, "")[[1]]))
cat("Unique characters:", length(chars), "\n")
chars

char_indices <- 1:length(chars) 
names(char_indices) <- chars
char_indices

#ONE HOT VECTOR

cat("Vectorization...\n") 
x <- array(0L, dim = c(length(sentences), mmaxlen, length(chars)))
y <- array(0L, dim = c(length(sentences), length(chars)))

for (i in 1:length(sentences)) {
  sentence <- strsplit(sentences[[i]], "")[[1]]
  for (t in 1:length(sentence)) {
    char <- sentence[[t]]
    x[i, t, char_indices[[char]]] <- 1
  }
  next_char <- next_chars[[i]]
  y[i, char_indices[[next_char]]] <- 1
}

# MODELO

model <- keras_model_sequential() %>%
  layer_lstm(units = 128, input_shape = c(mmaxlen, length(chars))) %>% 
  layer_dense(units = length(chars), activation = "softmax")


optimizer <- optimizer_rmsprop(lr = 0.01)
model %>% compile(loss = "categorical_crossentropy", optimizer = optimizer)   


#TRAINING MODEL & SAMPLING

sample_next_char <- function(preds, temperature = 1.0) {
  preds <- as.numeric(preds)
  preds <- log(preds) / temperature
  exp_preds <- exp(preds)
  preds <- exp_preds / sum(exp_preds)
  which.max(t(rmultinom(1, 1, preds)))
}


for (epoch in 1:1) { cat("epoch", epoch, "\n")
  model %>% fit(x, y, batch_size = 1000, epochs= 100) 
  start_index <- sample(1:(nchar(texto_min) - mmaxlen - 1), 1)  
  seed_text <- str_sub(texto_min, start_index, start_index + mmaxlen - 1)
  cat("--- Generating with seed:", seed_text, "\n\n")
  
  for (temperature in c(1.0, 1.2, 1.5, 2.0)) {
    cat("------ temperature:", temperature, "\n")
    cat(seed_text, "\n")
    generated_text <- seed_text
    
    for (i in 1:20) {
      sampled <- array(0, dim = c(1, mmaxlen, length(chars)))
      generated_chars <- strsplit(generated_text, "")[[1]]
    
      for (t in 1:length(generated_chars)) {
          har <- generated_chars[[t]]
          sampled[1, t, char_indices[[char]]] <- 1
      }
    }
  }
}
  
  