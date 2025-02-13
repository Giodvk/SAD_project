library(ggplot2)
library(dplyr)
library(forecast) # Per stl() e autoplot()

# Presumendo che il tuo dataframe si chiami 'df'

for (activity_id in 1:6) {
  # Crea il nome del file
  filename <- paste0("stl_decomposition_activity_", activity_id, ".png")
  
  # Filtra i dati per l'attività corrente
  activity_data <- df %>%
    filter(Subject == 1, Activity == activity_id) %>%
    pull(body_acc_x) # Oppure body_acc_y, body_acc_z, ecc. - scegli in base alle necessità
  
  # Verifica se ci sono dati per questa attività per evitare errori
  if (length(activity_data) > 0) {
    # Crea un oggetto time series
    activity_ts <- ts(activity_data, frequency = 50)
    
    # Esegui la decomposizione STL
    stl_decomp <- stl(activity_ts, s.window = "periodic")
    
    # Crea il grafico
    plot <- autoplot(stl_decomp) +
      labs(title = paste("STL Decomposition (Body Acc X) - Activity", activity_id)) # Titolo dinamico
    
    # Salva il grafico come PNG
    ggsave(filename, plot = plot, width = 10, height = 6, units = "in") # Regola larghezza/altezza come necessario
    
    print(paste("STL decomposition for Activity", activity_id, "saved to", filename)) # Messaggio di conferma
  } else {
    print(paste("No data found for Activity", activity_id)) # Messaggio se non ci sono dati
  }
  
}


# Esempio che utilizza body_acc_y e WindowID.Puoi adattare questo
# loop in modo simile a quanto sopra se ti servono grafici per finestra.

for (activity_id in 1:6) {
  for (window_id in unique(df$WindowID)) { # Loop attraverso ogni WindowID unico
    filename <- paste0("stl_decomposition_activity_gyro_z_", activity_id, "_window_", window_id, ".png")
    
    window_data <- df %>%
      filter(Subject == 1, Activity == activity_id, WindowID == window_id) %>%
      pull(body_gyro_z)
    
    if (length(window_data) > 0) {
      window_ts <- ts(window_data, frequency = 50)
      stl_decomp <- stl(window_ts, s.window = "periodic")
      plot <- autoplot(stl_decomp) +
        labs(title = paste("STL Decomposition (Body gyro z) - Activity", activity_id, "- Window", window_id)) # Titolo dinamico che include Activity ID e Window ID
      ggsave(filename, plot = plot, width = 10, height = 6, units = "in") # Salva il grafico
      print(paste("STL decomposition for Activity", activity_id, "Window", window_id, "saved to", filename)) # Messaggio di conferma salvataggio con Activity ID e Window ID
    } else {
      print(paste("No data found for Activity", activity_id, "Window", window_id)) # Messaggio se non ci sono dati per Activity ID e Window ID
    }
  }
}