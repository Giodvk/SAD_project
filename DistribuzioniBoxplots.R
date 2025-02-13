# Imposta la griglia 2x3 per i plot
par(mfrow = c(2, 3))

# Crea i boxplot con colori e titoli personalizzati
boxplot(df$body_acc_x, main = "body_acc_x", col = "lightcoral", ylab = "Values")
boxplot(df$body_acc_y, main = "body_acc_y", col = "lightblue", ylab = "Values")
boxplot(df$body_acc_z, main = "body_acc_z", col = "lightgreen", ylab = "Values")
boxplot(df$body_gyro_x, main = "body_gyro_x", col = "gold", ylab = "Values")
boxplot(df$body_gyro_y, main = "body_gyro_y Y", col = "violet", ylab = "Values")
boxplot(df$body_gyro_z, main = "body_gyro_z", col = "orange", ylab = "Values")

# Resetta il layout alla fine (opzionale)
par(mfrow = c(1, 1))
library(dplyr)
library(tidyr)
# Funzione per calcolare le statistiche dettagliate dei boxplot
calculate_box_stats <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  
  # Calcolo dei limiti teorici
  lower_fence <- q[1] - 1.5 * iqr
  upper_fence <- q[2] + 1.5 * iqr
  
  # Calcolo dei baffi effettivi (rispetto ai dati reali)
  lower_whisker <- min(x[x >= lower_fence], na.rm = TRUE)
  upper_whisker <- max(x[x <= upper_fence], na.rm = TRUE)
  
  # Conteggio outlier
  n_outliers <- sum(x < lower_fence | x > upper_fence, na.rm = TRUE)
  
  return(c(
    Q1 = q[1],
    Q3 = q[2],
    IQR = iqr,
    Lower_Fence = lower_fence,
    Upper_Fence = upper_fence,
    Lower_Whisker = lower_whisker,
    Upper_Whisker = upper_whisker,
    N_Outliers = n_outliers
  ))
}

# Applica la funzione a tutte le colonne rilevanti
cols <- c("body_acc_x", "body_acc_y", "body_acc_z",
          "body_gyro_x", "body_gyro_y", "body_gyro_z")

results <- df %>% 
  summarise(across(all_of(cols), calculate_box_stats)) %>% 
  t() %>% 
  as.data.frame() %>% 
  `colnames<-`(c("Q1", "Q3", "IQR", "Lower_Fence", "Upper_Fence", 
                 "Lower_Whisker", "Upper_Whisker", "N_Outliers"))

# Visualizza i risultati
print(results)

library(ggplot2)
par(mfrow = c(2, 3))

ggplot(df_standardized, aes(x = body_acc_x)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_acc_x")
ggplot(df, aes(x = body_acc_y)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_acc_y")
ggplot(df, aes(x = body_acc_z)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_acc_z")
ggplot(df, aes(x = body_gyro_x)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_gyro_x")
ggplot(df, aes(x = body_gyro_y)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_gyro_y")
ggplot(df, aes(x = body_gyro_z)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Distribuzione di body_gyro_z")


# Define a function to set outliers to NA based on 3 standard deviations
remove_outliers <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)  # Calculate mean
  sd_x <- sd(x, na.rm = TRUE)      # Calculate standard deviation
  lower_bound <- mean_x - 3 * sd_x
  upper_bound <- mean_x + 3 * sd_x
  ifelse(x >= lower_bound & x <= upper_bound, x, NA)  # Set outliers to NA
}

# Apply the function to each column without removing rows
df_without_outliers <- df
df_without_outliers$body_acc_x <- remove_outliers(df$body_acc_x)
df_without_outliers$body_acc_y <- remove_outliers(df$body_acc_y)
df_without_outliers$body_acc_z <- remove_outliers(df$body_acc_z)
df_without_outliers$body_gyro_x <- remove_outliers(df$body_gyro_x)
df_without_outliers$body_gyro_y <- remove_outliers(df$body_gyro_y)
df_without_outliers$body_gyro_z <- remove_outliers(df$body_gyro_z)

par(mfrow = c(2, 3))

# Boxplots for the cleaned data (ignoring NAs)
boxplot(df_without_outliers$body_acc_x, main = "body_acc_x (No Outliers)", col = "lightcoral", ylab = "Values", na.rm = TRUE)
boxplot(df_without_outliers$body_acc_y, main = "body_acc_y (No Outliers)", col = "lightblue", ylab = "Values", na.rm = TRUE)
boxplot(df_without_outliers$body_acc_z, main = "body_acc_z (No Outliers)", col = "lightgreen", ylab = "Values", na.rm = TRUE)
boxplot(df_without_outliers$body_gyro_x, main = "body_gyro_x (No Outliers)", col = "gold", ylab = "Values", na.rm = TRUE)
boxplot(df_without_outliers$body_gyro_y, main = "body_gyro_y (No Outliers)", col = "violet", ylab = "Values", na.rm = TRUE)
boxplot(df_without_outliers$body_gyro_z, main = "body_gyro_z (No Outliers)", col = "orange", ylab = "Values", na.rm = TRUE)
par(mfrow = c(1, 1))

# Density plots for the cleaned data using ggplot2
library(ggplot2)
# Install the gridExtra package if not already installed
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

library(ggplot2)
library(gridExtra)

# Create individual plots
p1 <- ggplot(df_without_outliers, aes(x = body_acc_x)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_acc_x (No Outliers)")

p2 <- ggplot(df_without_outliers, aes(x = body_acc_y)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_acc_y (No Outliers)")

p3 <- ggplot(df_without_outliers, aes(x = body_acc_z)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_acc_z (No Outliers)")

p4 <- ggplot(df_without_outliers, aes(x = body_gyro_x)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_gyro_x (No Outliers)")

p5 <- ggplot(df_without_outliers, aes(x = body_gyro_y)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_gyro_y (No Outliers)")

p6 <- ggplot(df_without_outliers, aes(x = body_gyro_z)) + 
  geom_density(fill = "blue", alpha = 0.5, na.rm = TRUE) + 
  labs(title = "Density of body_gyro_z (No Outliers)")

# Arrange the plots in a 2x3 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
library(ggplot2)
library(tidyr)

# Converti i dati in formato lungo
df_long <- df %>%
  pivot_longer(cols = c(body_acc_x, body_acc_y, body_acc_z, 
                        body_gyro_x, body_gyro_y, body_gyro_z),
               names_to = "Sensor", 
               values_to = "Value")

# Crea i boxplot
ggplot(df_long, aes(x = Sensor, y = Value, fill = Sensor)) +
  geom_boxplot() +
  facet_wrap(~Sensor, nrow = 2, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("lightcoral", "lightblue", "lightgreen", 
                               "gold", "violet", "orange"))


library(ggplot2)
library(tidyr)

# Converti i dati in formato lungo
df_long <- df %>%
  pivot_longer(cols = c(body_acc_x, body_acc_y, body_acc_z, 
                        body_gyro_x, body_gyro_y, body_gyro_z),
               names_to = "Sensor", 
               values_to = "Value")

# Crea violin plot
ggplot(df_long, aes(x = Sensor, y = Value, fill = Sensor)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.5) + # Aggiungi mini-boxplot
  facet_wrap(~Sensor, nrow = 2, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("lightcoral", "lightblue", "lightgreen", 
                               "gold", "violet", "orange")) +
  labs(title = "Violin Plot per Sensori")

library(dplyr)

df_clean <- df %>%
  mutate(across(
    c(body_acc_x, body_acc_y, body_acc_z,
      body_gyro_x, body_gyro_y, body_gyro_z),
    ~ {
      qnt <- quantile(., probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- 1.5 * IQR(., na.rm = TRUE)
      lower <- qnt[1] - iqr
      upper <- qnt[2] + iqr
      ifelse(. < lower | . > upper, NA, .)  # Sostituisci con NA
    }
  )) %>%
  na.omit()  # Opzionale: elimina righe con NA


par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

sensors <- c("body_acc_x", "body_acc_y", "body_acc_z",
             "body_gyro_x", "body_gyro_y", "body_gyro_z")

colors <- c("lightcoral", "lightblue", "lightgreen",
            "gold", "violet", "orange")

for (i in seq_along(sensors)) {
  boxplot(df_clean[[sensors[i]]],
          main = sensors[i],
          col = colors[i],
          ylab = "Values",
          outline = FALSE  # Non mostra gli outlier rimanenti (se presenti)
  )
}

library(ggplot2)
df_clean_long <- df_clean %>%
  pivot_longer(cols = all_of(sensors), names_to = "Sensor", values_to = "Value")

ggplot(df_clean_long, aes(x = Sensor, y = Value, fill = Sensor)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = colors) +
  theme_minimal()


library(dplyr)

winsorize <- function(x, lower_limit = NULL, upper_limit = NULL) {
  if (is.null(lower_limit) || is.null(upper_limit)) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- 1.5 * IQR(x, na.rm = TRUE)
    lower_limit <- qnt[1] - iqr
    upper_limit <- qnt[2] + iqr
  }
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  return(x)
}
df_winsorized <- df %>%
  mutate(across(all_of(cols), winsorize))
# Applica a tutte le colonne
cols <- c("body_acc_x", "body_acc_y", "body_acc_z",
          "body_gyro_x", "body_gyro_y", "body_gyro_z")

robust_scaler <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

df_robust <- df %>%
  mutate(across(all_of(cols), robust_scaler))


signed_log <- function(x) {
  sign(x) * log1p(log1p(abs(x)))  # log1p evita problemi con valori vicini a zero
}

df_log <- df %>%
  mutate(across(all_of(cols), signed_log))
boxplot(df_log$body_acc_x, main = "body_acc_x", col = "lightcoral", ylab = "Values")
boxplot(df_log$body_acc_y, main = "body_acc_y", col = "lightblue", ylab = "Values")
boxplot(df_log$body_acc_z, main = "body_acc_z", col = "lightgreen", ylab = "Values")
boxplot(df_log$body_gyro_x, main = "body_gyro_x", col = "gold", ylab = "Values")
boxplot(df_log$body_gyro_y, main = "body_gyro_y Y", col = "violet", ylab = "Values")
boxplot(df_log$body_gyro_z, main = "body_gyro_z", col = "orange", ylab = "Values")

