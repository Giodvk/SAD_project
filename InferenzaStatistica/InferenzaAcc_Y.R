#Inferenza statistica su accelerazione dell'asse x

body_acc_y <- df$body_acc_y

#Bisogna suddividere il campione che si considera in intervalli per poi utilizzare
# il test del chi-quadrato

library(dplyr)

# Calcolo della larghezza del bin con Freedman-Diaconis
iqr_value <- IQR(body_acc_y, na.rm = TRUE)
n <- nrow(df)
bin_width <- 2 * iqr_value / (n^(1/3))

# Creazione dei bin
breaks <- seq(min(body_acc_y, na.rm = TRUE), 
              max(body_acc_y, na.rm = TRUE), 
              by = bin_width)

# Assegnazione delle osservazioni ai bin
cuts <- cut(body_acc_y, breaks = breaks, include.lowest = TRUE)
table(cuts)

#Cancelliamo o uniamo gli intervalli che hanno un numero di osservazioni <=5
breaks <- breaks[-c(1, length(breaks)-1, length(breaks))]

breaks[1] <- -1
breaks[length(breaks)] <- 1
print(breaks)
cuts <- cut(body_acc_x, breaks = breaks, include.lowest = TRUE)
table(cuts)


#Procediamo a eseguire il test-chi quadrato per una normale 

#Parametri da stimare k = 2
mu <- mean(body_acc_y, na.rm = TRUE)
sigma <- sd(body_acc_y, na.rm = TRUE)

# 2️⃣ Calcola probabilità teoriche per ogni bin
# Converti gli intervalli (fattori) in stringhe e poi in numeri
bin_edges <- as.numeric(gsub("\\(|\\]|\\[", "", unlist(strsplit(levels(cuts), ","))))
bin_edges <- unique(bin_edges)
print(bin_edges)
probs <- pnorm(bin_edges, mean = mu, sd = sigma)  # Calcola CDF nei bordi
probs <- diff(probs)  # Probabilità per ogni intervallo

# 3️⃣ Frequenze osservate
obs_freq <- table(cuts)
print(obs_freq)
# 4️⃣ Frequenze attese
expected_freq <- probs * sum(obs_freq)

# 5️⃣ Test di chi-quadrato
chi_test <- chisq.test(x = obs_freq, p = probs, rescale.p = TRUE)

# 6️⃣ Risultati
print(chi_test)

alpha <- 0.05
qchisq(alpha/2, df = length(cuts) - 2 - 1)
qchisq(1 - alpha/2, df = length(cuts) - 2 - 1)

