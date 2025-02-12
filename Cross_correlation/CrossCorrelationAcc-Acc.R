library(dplyr)

vars_pairs <- list(
  c('body_acc_x', 'body_acc_y'),
  c('body_acc_y', 'body_acc_z'),
  c('body_acc_x', 'body_acc_z')
)

for(pair in vars_pairs){
  var1 <- pair[1]
  var2 <- pair[2]
  
  cross_score <- df %>%
    group_by(WindowID) %>%
    summarise(
      cross_correlation = list(ccf(get(var1), get(var2), plot = FALSE, na.action = na.fail))
    )
  
  for(i in seq_along(cross_score$cross_correlation)){
    window <- cross_score$WindowID[i]
    ccf_score <- cross_score$cross_correlation[[i]]
    plot(ccf_score, main = paste("Cross-Correlazione:", var1, "vs", var2, "- Window ID:", window))
  }
}

fft_result <- fft(df$body_acc_x)  # Trasformata di Fourier sull'accelerazione x
freq <- (0:(length(fft_result)-1)) / length(fft_result)  # Frequenze associate

plot(freq, Mod(fft_result), type = "l", main = "Spettro di Fourier", xlab = "Frequenza (Hz)", ylab = "Ampiezza")

