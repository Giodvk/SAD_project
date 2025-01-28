library(dplyr)
library(moments)

grouped_data <- df %>%
  group_by(Activity) %>%
  select(body_gyro_x, body_gyro_y, body_gyro_z, Activity)

body_gyro <- grouped_data %>% filter(Activity == 1)


metrics <- summarise(body_gyro,
                      across(
                        c(body_gyro_x, body_gyro_y, body_gyro_z),
                        list(
                          mean = ~ mean(.x, na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE),
                          var = ~ var(.x, na.rm = TRUE),
                          sd = ~ sd(.x, na.rm = TRUE),
                          skewness = ~ skewness(.x, na.rm = TRUE),
                          kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                      ))

body_gyro <- grouped_data %>% filter(Activity == 2)

metrics[nrow(metrics) + 1, ] <- summarise(body_gyro,
                                            across(
                                              c(body_gyro_x, body_gyro_y, body_gyro_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_gyro <- grouped_data %>% filter(Activity == 3)

metrics[nrow(metrics) + 1, ] <- summarise(body_gyro,
                                            across(
                                              c(body_gyro_x, body_gyro_y, body_gyro_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_gyro <- grouped_data %>% filter(Activity == 4)

metrics[nrow(metrics) + 1, ] <- summarise(body_gyro,
                                            across(
                                              c(body_gyro_x, body_gyro_y, body_gyro_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_gyro <- grouped_data %>% filter(Activity == 5)

metrics[nrow(metrics) + 1, ] <- summarise(body_gyro,
                                            across(
                                              c(body_gyro_x, body_gyro_y, body_gyro_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))

body_gyro <- grouped_data %>% filter(Activity == 6)

metrics[nrow(metrics) + 1, ] <- summarise(body_gyro,
                                            across(
                                              c(body_gyro_x, body_gyro_y, body_gyro_z),
                                              list(
                                                mean = ~ mean(.x, na.rm = TRUE),
                                                median = ~ median(.x, na.rm = TRUE),
                                                var = ~ var(.x, na.rm = TRUE),
                                                sd = ~ sd(.x, na.rm = TRUE),
                                                skewness = ~ skewness(.x, na.rm = TRUE),
                                                kurtosis = ~ kurtosis(.x, na.rm = TRUE))
                                            ))



