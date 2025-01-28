library(dplyr)
library(moments)

grouped_data <- df %>%
  group_by(Activity) %>%
  select(body_acc_x, body_acc_y, body_acc_z, Activity)

body_acc_1 <- grouped_data %>% filter(Activity == 1)


metrics1 <- summarise(body_acc_1,
                      across(
                        c(body_acc_x, body_acc_y, body_acc_z),
                        list(
                          mean = ~ mean(.x, na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE),
                          var = ~ var(.x, na.rm = TRUE),
                          sd = ~ sd(.x, na.rm = TRUE),
                          skewness = ~ skewness(.x, na.rm = TRUE),
                          kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                      ))

body_acc_1 <- grouped_data %>% filter(Activity == 2)

metrics1[nrow(metrics1) + 1, ] <- summarise(body_acc_1,
                     across(
                       c(body_acc_x, body_acc_y, body_acc_z),
                       list(
                         mean = ~ mean(.x, na.rm = TRUE),
                         median = ~ median(.x, na.rm = TRUE),
                         var = ~ var(.x, na.rm = TRUE),
                         sd = ~ sd(.x, na.rm = TRUE),
                         skewness = ~ skewness(.x, na.rm = TRUE),
                         kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                     ))

body_acc_1 <- grouped_data %>% filter(Activity == 3)

metrics1[nrow(metrics1) + 1, ] <- summarise(body_acc_1,
                                            across(
                                              c(body_acc_x, body_acc_y, body_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_acc_1 <- grouped_data %>% filter(Activity == 4)

metrics1[nrow(metrics1) + 1, ] <- summarise(body_acc_1,
                                            across(
                                              c(body_acc_x, body_acc_y, body_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_acc_1 <- grouped_data %>% filter(Activity == 5)

metrics1[nrow(metrics1) + 1, ] <- summarise(body_acc_1,
                                            across(
                                              c(body_acc_x, body_acc_y, body_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_acc_1 <- grouped_data %>% filter(Activity == 6)

metrics1[nrow(metrics1) + 1, ] <- summarise(body_acc_1,
                                            across(
                                              c(body_acc_x, body_acc_y, body_acc_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))



