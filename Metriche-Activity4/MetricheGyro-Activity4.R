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
  filter(Activity == 4) %>% 
  group_by(Subject) %>% 
  select(Subject, Activity, body_gyro_x, body_gyro_y, body_gyro_z, WindowID) %>%
  mutate(
    # Assegna timestamp che partono da 0 per ogni soggetto (50Hz = 0.02 secondi)
    time_stamp = (row_number() - 1) * 0.02
  )

# Calcola tutte le metriche per ogni asse in un unico passaggio
metrics <- grouped_data %>% 
  summarise(
    across(
      c(body_gyro_x, body_gyro_y, body_gyro_z),
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


# Le medie dei giroscopi sono molto differenti tra loro, innanzitutto l'unica variabile
# che presenta media con valori solo negativi è body_gyro_z la cui forma di distribuzione
# è sempre asimmetrica a sinistra ad eccezione di alcuni soggetti. La x e la y del giroscopio 
# hanno media che variano e la distribuzione cambia da soggetto a soggetto.

# La varianza e la deviazione standard sono molto alte per quanto riguarda la y che è
# la più varibile tra le tre 

#Per quanto riguarda la skewness è molto variabile in tutte e tre i casi considerati
# facendo capire che la forma della distribuzione cambia tra un soggeto a l'altro

#La curtosi in tutte e tre le varibili è diversa cambiando in base al soggetto.


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
    mean_x = mean(body_gyro_x),
    mean_y = mean(body_gyro_y),
    mean_z = mean(body_gyro_z)
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
    title = "Medie giroscopi nel tempo",
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






