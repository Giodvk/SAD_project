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









library(ggplot2)
library(gridExtra)
library(tidyr) # Make sure tidyr is loaded for pivot_longer

# Sample Data (replace with your actual data)

# Dati in formato lungo
df_long <- df %>%
  pivot_longer(
    cols = c(body_acc_x, body_acc_y, body_acc_z),
    names_to = "Asse",
    values_to = "Valore"
  )

# Function to create boxplot for a single asse
create_boxplot <- function(axis) {
  
  axis_label <- toupper(gsub("body_acc_", "", axis)) # Extract x, y, or z
  
  df_long %>%
    filter(Asse == axis) %>%
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
  create_boxplot("body_acc_x"),
  create_boxplot("body_acc_y"),
  create_boxplot("body_acc_z"),
  ncol = 3,
  top = "Confronto Attività per Asse (Boxplot)"
)



