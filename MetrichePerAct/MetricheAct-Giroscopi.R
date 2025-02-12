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







library(ggplot2)
library(gridExtra)
library(tidyr) # Make sure tidyr is loaded for pivot_longer

# Sample Data (replace with your actual data)

# Dati in formato lungo
df_long <- df %>%
  pivot_longer(
    cols = c(body_gyro_x, body_gyro_y, body_gyro_z),
    names_to = "Giroscopio",
    values_to = "Valore"
  )

# Function to create boxplot for a single asse
create_boxplot <- function(axis) {
  
  axis_label <- toupper(gsub("body_gyro_", "", axis)) # Extract x, y, or z
  
  df_long %>%
    filter(Giroscopio == axis) %>%
    ggplot(aes(x = factor(Activity), y = Valore, fill = factor(Activity))) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Asse", axis_label),  # Use the extracted label
      x = "Attività",
      y = "Valore"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Combine the 3 graphs
grid.arrange(
  create_boxplot("body_gyro_x"),
  create_boxplot("body_gyro_y"),
  create_boxplot("body_gyro_z"),
  ncol = 3,
  top = "Confronto Attività per Asse (Boxplot) - Giroscopio"
)



