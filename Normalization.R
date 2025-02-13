#normalizzazione delle colonne del dataset per far contenere solo valori tra 1 e -1

normalize <- function(x) {
  return(2 * ((x - min(x)) / (max(x) - min(x))) - 1)
}

# Normalizziamo più colonne del dataset
df[,4 : 9] <- lapply(df[, 4 : 9], normalize)
