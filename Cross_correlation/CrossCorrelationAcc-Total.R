install.packages('corrplot')

library(dplyr)
library(corrplot)

cor_vars <- df[, 3:11]

cor_matrix <- cor(cor_vars, method ="pearson")
corrplot(cor_matrix, method = "circle", tl.col = "black")


# Crea tutte le possibili coppie di variabili (coppie significative che hai scelto)
var_combinations <- list(
  c("body_acc_x", "total_acc_x"),
  c("body_acc_y", "total_acc_y"),
  c("body_acc_z", "total_acc_z")
)

# Per ogni coppia di variabili, calcola la cross-correlazione
for (pair in var_combinations) {
  # Estrai i dati per la coppia
  var1 <- pair[1]
  var2 <- pair[2]
  
  # Calcola la cross-correlazione tra le due variabili
  result_crosscorr <- df %>%
    group_by(WindowID) %>%
    summarise(
      crosscorr_plot = list(ccf(get(var1), get(var2), plot = FALSE, na.action = na.fail))
    )
  
  # Visualizza il grafico per ogni finestra
  for (i in seq_along(result_crosscorr$crosscorr_plot)) {
    window <- result_crosscorr$WindowID[i]
    ccf_result <- result_crosscorr$crosscorr_plot[[i]]
    plot(ccf_result, main = paste("Cross-Correlazione:", var1, "vs", var2, "- Window ID:", window))
  }
}

