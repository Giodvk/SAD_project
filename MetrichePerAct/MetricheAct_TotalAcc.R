library(dplyr)
library(moments)

grouped_data <- df %>%
  group_by(Activity) %>%
  select(total_acc_x, total_acc_y, total_acc_z, Activity)

body_tot <- grouped_data %>% filter(Activity == 1)


metrics <- summarise(body_tot,
                      across(
                        c(total_acc_x, total_acc_y, total_acc_z),
                        list(
                          mean = ~ mean(.x, na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE),
                          var = ~ var(.x, na.rm = TRUE),
                          sd = ~ sd(.x, na.rm = TRUE),
                          skewness = ~ skewness(.x, na.rm = TRUE),
                          kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                      ))

body_tot <- grouped_data %>% filter(Activity == 2)

metrics[nrow(metrics) + 1, ] <- summarise(body_tot,
                                            across(
                                              c(total_acc_x, total_acc_y, total_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_tot <- grouped_data %>% filter(Activity == 3)

metrics[nrow(metrics) + 1, ] <- summarise(body_tot,
                                            across(
                                              c(total_acc_x, total_acc_y, total_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_tot <- grouped_data %>% filter(Activity == 4)

metrics[nrow(metrics) + 1, ] <- summarise(body_tot,
                                            across(
                                              c(total_acc_x, total_acc_y, total_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_tot <- grouped_data %>% filter(Activity == 5)

metrics[nrow(metrics) + 1, ] <- summarise(body_tot,
                                            across(
                                              c(total_acc_x, total_acc_y, total_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_tot <- grouped_data %>% filter(Activity == 6)

metrics[nrow(metrics) + 1, ] <- summarise(body_tot,
                                            across(
                                              c(total_acc_x, total_acc_y, total_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))



