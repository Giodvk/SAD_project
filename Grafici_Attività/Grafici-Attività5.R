df = read.csv(file.choose(), head = TRUE, sep=",", dec=".")

library(dplyr)
library(ggplot2)

grouped_data <- df %>% 
  group_by(Activity, Subject) %>%
  filter(Activity == 5) %>%
  select(Activity, Subject, body_acc_x, body_acc_y, body_acc_z, body_gyro_x, body_gyro_y, body_gyro_z,
         total_acc_x)

#Kernel Density Plot body_acc_x
ggplot(grouped_data, aes(x = body_acc_x, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_acc_x), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_acc_x per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_acc_x)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_acc_x
ggplot(grouped_data, aes(x = body_acc_x))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_acc_x attività 5
ggplot(grouped_data, aes(y = body_acc_x, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_acc_x - Attività 5",
    y = "Accelerazione (body_acc_x)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_acc_x, main = "Autocorrelation Function - body_acc_x", plot = TRUE)


#Grafici attività 5 per body_acc_y


#Kernel Density Plot body_acc_y
ggplot(grouped_data, aes(x = body_acc_y, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_acc_y), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_acc_y per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_acc_y)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_acc_y
ggplot(grouped_data, aes(x = body_acc_y))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_acc_y attività 5
ggplot(grouped_data, aes(y = body_acc_y, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_acc_y - Attività 5",
    y = "Accelerazione (body_acc_x)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_acc_y, main = "Autocorrelation Function - body_acc_y", plot = TRUE)

#Grafici per body_acc_z

#Kernel Density Plot body_acc_z
ggplot(grouped_data, aes(x = body_acc_z, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_acc_z), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_acc_z per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_acc_z)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_acc_z
ggplot(grouped_data, aes(x = body_acc_z))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_acc_z attività 5
ggplot(grouped_data, aes(y = body_acc_z, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_acc_z - Attività 5",
    y = "Accelerazione (body_acc_z)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_acc_z, main = "Autocorrelation Function - body_acc_z", plot = TRUE)

#Grafici body_gyro_x


#Kernel Density Plot body_gyro_x
ggplot(grouped_data, aes(x = body_gyro_x, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_gyro_x), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_gyro_x per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_gyro_x)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_gyro_x
ggplot(grouped_data, aes(x = body_gyro_x))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_gyro_x attività 5
ggplot(grouped_data, aes(y = body_gyro_x, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_gyro_x - Attività 5",
    y = "Accelerazione (body_gyro_x)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_gyro_x, main = "Autocorrelation Function - body_gyro_x", plot = TRUE)

#Grafici body_gyro_y


#Kernel Density Plot body_gyro_y
ggplot(grouped_data, aes(x = body_gyro_y, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_gyro_y), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_gyro_y per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_gyro_y)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_gyro_y
ggplot(grouped_data, aes(x = body_gyro_y))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_gyro_y attività 5
ggplot(grouped_data, aes(y = body_gyro_y, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_gyro_y - Attività 5",
    y = "Accelerazione (body_gyro_y)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_gyro_y, main = "Autocorrelation Function - body_gyro_y", plot = TRUE)

#Grafici body_gyro_z

#Kernel Density Plot body_gyro_z
ggplot(grouped_data, aes(x = body_gyro_z, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_density(alpha = 0.3) +  
  # Aggiungi la densità complessiva (senza suddivisione per soggetto)
  geom_density(
    aes(x = body_gyro_z), 
    inherit.aes = FALSE,  # Ignora le estetiche globali
    color = "gray20",      # Colore uniforme
    linewidth = 0.8       # Spessore per evidenziare
  ) +
  labs(
    title = "Distribuzione body_gyro_z per Attività 5",
    subtitle = "Linea nera: distribuzione complessiva",
    fill = "Subject",
    color = "Subject",
    x = "Accelerazione (body_gyro_z)",
    y = "Densità"
  ) +
  theme_minimal()


#Istogramma body_gyro_z
ggplot(grouped_data, aes(x = body_gyro_z))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal()


#BoxPlot body_gyro_z attività 5
ggplot(grouped_data, aes(y = body_gyro_z, color = as.factor(Subject), fill = as.factor(Subject))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  labs(
    bold = FALSE,
    title = "Distribuzione di body_gyro_z - Attività 5",
    y = "Accelerazione (body_gyro_z)",  # Etichetta chiara per l'asse Y
    x = NULL,  # Rimuove l'etichetta dall'asse X
    fill = "Soggetto",  # Migliora la legenda
    color = "Soggetto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Oscura i valori dell'asse X
    axis.ticks.x = element_blank(),  # Rimuove i tick dall'asse X
    panel.grid.major.x = element_blank(),  # Rimuove le griglie verticali
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),  # Griglie orizzontali più eleganti
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Migliora il titolo
    axis.title.y = element_text(size = 12),  # Migliora l'etichetta Y
    legend.position = "right"  # Posizione della legenda per leggibilità
  )


acf(grouped_data$body_gyro_z, main = "Autocorrelation Function - body_gyro_z", plot = TRUE)



