#Calcolo delle metriche raggruppandole per attività
install.packages("moments")
install.packages("tidyr")  # Solo se non è già installato

library(tidyr)  # Carica il pacchetto
library(dplyr)
library(moments)

printMetrics <- function(x,y,z, metric){
  print(paste(metric, x))
  print(paste(metric, y))
  print(paste(metric, z))
}


# Filtra e raggruppa i dati
grouped_data <- df %>%
  filter(Activity == 2) %>% 
  group_by(Subject) %>% 
  select(Subject, Activity, body_acc_x, body_acc_y, body_acc_z, WindowID) %>%
  mutate(
    # Assegna timestamp che partono da 0 per ogni soggetto (50Hz = 0.02 secondi)
    time_stamp = (row_number() - 1) * 0.02
  )

# Calcola tutte le metriche per ogni asse in un unico passaggio
metrics <- grouped_data %>% 
  summarise(
    across(
      c(body_acc_x, body_acc_y, body_acc_z),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        var = ~ var(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        skewness = ~ skewness(.x, na.rm = TRUE),
        kurtosis = ~ kurtosis(.x, na.rm = TRUE)
      )
    )
  )

# Visualizza le metriche
print(metrics)



# Calcola inizio, fine e centro di ogni finestra
window_time <- grouped_data %>%
  group_by(WindowID) %>%
  summarise(
    start_time = min(time_stamp),     # Tempo di inizio finestra
    end_time = max(time_stamp),       # Tempo di fine finestra
    center_time = start_time + (end_time - start_time)/2  # Centro della finestra
  )

# Aggiungi le metriche (es. media, skewness)
window_metrics <- grouped_data %>%
  group_by(WindowID) %>%
  summarise(
    mean_x = mean(body_acc_x),
    mean_y = mean(body_acc_y),
    mean_z = mean(body_acc_z)
  ) %>%
  left_join(window_time, by = "WindowID")

# Riformatta i dati in formato lungo
window_metrics_long <- window_metrics %>%
  pivot_longer(
    cols = c(mean_x, mean_y, mean_z),
    names_to = "Metrica",
    values_to = "Valore"
  )

# Grafico con facet
ggplot(window_metrics_long, aes(x = center_time, y = Valore, color = Metrica)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Metrica, ncol = 1, scales = "free_y") + # Assi y separati
  labs(
    title = "Medie accelerazioni nel tempo",
    x = "Tempo (s)",
    y = "Valore"
  ) +
  theme_minimal()

# Converti il dataframe in formato "lungo" per ggplot2
metrics_long <- metrics %>%
  pivot_longer(
    cols = -Subject,
    names_to = c("Asse", "Metrica"),
    names_pattern = "(.*)_(.*)",
    values_to = "Valore"
  )

media_data <- metrics_long %>%
  filter(Metrica == "mean")

ggplot(media_data, aes(x = Asse, y = Valore, fill = Asse)) +
  geom_boxplot() +
  labs(
    title = "Confronto della media tra gli assi (Activity 1)",
    x = "Asse",
    y = "Media"
  ) +
  theme_minimal()


sd_data <- metrics_long %>%
  filter(Metrica == "sd")

ggplot(sd_data, aes(x = Asse, y = Valore, fill = Asse)) +
  geom_boxplot() +
  labs(
    title = "Confronto della deviazione standard tra gli assi (Activity 1)",
    x = "Asse",
    y = "Deviazione standard"
  ) +
  theme_minimal()

skew_data <- metrics_long %>%
  filter(Metrica == "skewness")

ggplot(skew_data, aes(x = Asse, y = Valore, fill = Asse)) +
  geom_boxplot() +
  labs(
    title = "Confronto della skewness tra gli assi (Activity 1)",
    x = "Asse",
    y = "Skewness"
  ) +
  theme_minimal()






