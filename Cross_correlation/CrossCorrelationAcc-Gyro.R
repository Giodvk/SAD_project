library(dplyr)


vars_pairs <- list(
  c('body_acc_x', 'body_gyro_x'),
  c('body_acc_y', 'body_gyro_y'),
  c('body_acc_z', 'body_gyro_z')
)

for(pair in vars_pairs){
  var1 <- pair[1]
  var2 <- pair[2]
  
  cross_correlation_score <- df %>%
    group_by(WindowID) %>%
    summarise(crosscore_score = list(ccf(get(var1), get(var2), plot = FALSE, 
                                          na.action = na.fail)))
  
  for(i in seq_along(cross_correlation_score$crosscore_score)){
    window <- cross_correlation_score$WindowID[i]
    ccf_result <- cross_correlation_score$crosscore_score[[i]]
    plot(ccf_result, main = paste("Cross-Correlazione:", var1, "vs", var2, "- Window ID:", window))
  }
}
