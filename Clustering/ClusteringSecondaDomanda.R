# Carichiamo le librerie necessarie
library(dplyr)
library(ggplot2)
# Assicuriamoci che KMeans_rcpp sia disponibile (proviene da pacchetti come ClusterR o è installato nel vostro workflow)
# library(KMeansRCpp)  # Decommentare se necessario

# --- Step 1: Presupponiamo di avere già i nostri dati PCA per la visualizzazione ---
# Qui presupponiamo che i nostri dati PCA 2D siano stati creati suddividendo l'output PCA alle prime due componenti.
# Per esempio:
# viz_data <- as.data.frame(prepped_data_juiced[, 1:2])
# E poi abbiamo aggiunto Soggetto e Attività:
# viz_data$Subject <- activity_features$Subject
# viz_data$Activity <- activity_features$Activity

# --- Step 2: Per ogni Attività, eseguiamo il clustering k-means su PC1 e PC2.
# Un'idea è quella di scegliere k uguale al numero di Soggetti unici per quell'attività.
library(dplyr)
library(ggplot2)
# (Assicurarsi che KMeans_rcpp sia caricato dal vostro pacchetto di clustering)

# --- Dopo i nostri passaggi di pre-elaborazione ---
# prepped_data_juiced è prodotto dalla nostra recipe che ha rimosso Soggetto/Attività.
# Presupponiamo che le colonne PCA siano denominate PC1 e PC2.
# Ora, ricolleghiamo le colonne Soggetto e Attività dai nostri dati originali.
# (Questo funziona se le righe rimangono nello stesso ordine.)
viz_data <- as.data.frame(prepped_data_juiced[, 1:2])
viz_data$Subject  <- activity_features$Subject
viz_data$Activity <- activity_features$Activity
print(nSubjects)
# --- Cluster per ogni Attività ---
# Per ogni attività, eseguiamo il clustering sui dati PCA 2D.
# Qui, utilizziamo k-means con k impostato al numero di soggetti unici per quell'attività.
viz_data_clustered <- viz_data %>%
  group_by(Activity) %>%
  mutate(nSubjects = n_distinct(Subject)) %>%
  mutate(cluster = as.factor(
    KMeans_rcpp(as.matrix(select(., PC01, PC02)),
                clusters = nSubjects,
                num_init = 10,
                seed = 42)$clusters
  )) %>%
  ungroup()

# --- Visualizzazione ---
# In questo grafico:
# - Gli assi x e y rappresentano PC1 e PC2.
# - I colori indicano il Soggetto effettivo (ovvero l'utente che ha eseguito l'attività).
# - Le forme dei punti rappresentano il cluster k-means (ovvero la stima di quale utente potrebbe essere).
# - Il facet per Attività mostra un pannello separato per ciascuna attività.
plot_by_activity <- ggplot(viz_data_clustered, aes(x = PC1, y = PC2, color = Subject, shape = cluster)) +
  geom_point(alpha = 0.7, size = 2.5) +
  facet_wrap(~ Activity) +
  labs(title = "Within-Activity Clustering: Inferring User Identity",
       x = "PC1", y = "PC2",
       color = "Subject",
       shape = "K-means Cluster") +
  theme_minimal()

# Visualizziamo il plot
print(plot_by_activity)














library(ggplot2)
library(dplyr)
library(cluster)
library(ClusterR)
library(mclust) # Per ARI


# Presumendo che 'df' sia il dataframe dai passaggi precedenti

activity_features <- df %>%
  select(Subject, Activity, contains("body_acc"), contains("gyro")) %>%
  group_by(Subject, Activity) %>%
  mutate(
    window_id = (row_number() - 1) %/% 64
  ) %>%
  group_by(Subject, Activity, window_id) %>%
  summarise(
    across(c(contains("acc"), contains("gyro")),
           list(mean = mean,
                sd = sd,
                variance = var,
                min = min,
                max = max,
                energy = ~sum(.^2)/length(.),
                zcr = ~sum(abs(diff(. > 0)))),
           .names = "{.col}_{.fn}")
  ) %>%
  ungroup()

# Creiamo una label binaria 'Movement' basata sulla mappatura delle attività 1-6 (ancora necessaria per il contesto originale)
activity_features <- activity_features %>%
  mutate(Movement = case_when(
    Activity %in% 1:3 ~ "Moving", # Le attività 1, 2, 3 sono di movimento
    Activity %in% 4:6 ~ "Still",   # Le attività 4, 5, 6 sono statiche
    TRUE ~ "Other"         # Gestiamo qualsiasi altra attività se presente
  )) %>%
  filter(Movement != "Other") # Rimuoviamo le attività 'Other' se vogliamo concentrarci solo su Moving vs Still


unique_activities <- unique(activity_features$Activity)

for (current_activity in unique_activities) {
  # 1. Filtriamo i dati per l'attività corrente
  activity_data_current <- activity_features %>% filter(Activity == current_activity)
  
  # 2. Determiniamo k (numero di soggetti unici nell'attività corrente)
  optimal_k_activity <- n_distinct(activity_data_current$Subject)
  cat(sprintf("\n--- Activity: %s ---\n", current_activity))
  cat(sprintf("Number of unique subjects for Activity %s: %d\n", current_activity, optimal_k_activity))
  
  # 3. Recipe di Pre-elaborazione per l'attività corrente
  preproc_recipe_activity <- recipe(~ ., data = activity_data_current) %>%
    step_rm(Movement, Subject, Activity, window_id) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_pca(all_numeric_predictors(), num_comp = 10, threshold = 0.95) %>%
    step_zv(all_predictors())
  
  prepped_data_juiced_activity <- prep(preproc_recipe_activity) %>% juice()
  prepped_data_activity <- as.matrix(prepped_data_juiced_activity)
  
  # 4. Clustering K-means per l'attività corrente
  if (optimal_k_activity > 1) { # Eseguiamo il clustering solo se c'è più di un soggetto
    kmeans_model_activity <- KMeans_rcpp(prepped_data_activity, optimal_k_activity,
                                         num_init = 10, seed = 42)
    clusters_activity <- kmeans_model_activity$clusters
    
    # 5. Valutazione rispetto al Soggetto per l'attività corrente
    ari_activity <- adjustedRandIndex(clusters_activity, activity_data_current$Subject)
    v_measure_activity_result <- calculate_vmeasure(clusters_activity, activity_data_current$Subject)
    v_score_activity <- v_measure_activity_result$v_measure
    cat(sprintf("Optimal k (Subjects): %d, ARI (vs Subject): %.3f, V-measure (vs Subject): %.3f\n", optimal_k_activity, ari_activity, v_score_activity))
    
    # 6. Visualizzazione (PCA per l'attività corrente, colore per Soggetto, forma per cluster)
    viz_data_activity <- as.data.frame(prepped_data_juiced_activity[, 1:2]) # Utilizziamo le prime 2 PC per la visualizzazione
    if(ncol(viz_data_activity) >= 2) { # Assicuriamoci che ci siano almeno 2 PC da visualizzare
      colnames(viz_data_activity) <- paste0("PC", 1:ncol(viz_data_activity)) # Rinominiamo le colonne per una più facile referenza
      
      viz_data_activity$cluster <- factor(clusters_activity)
      viz_data_activity$Subject <- factor(activity_data_current$Subject)
      
      plot_activity <- ggplot(viz_data_activity, aes(x = PC1, y = PC2, color = Subject, shape = cluster)) +
        geom_point(alpha = 0.7, size = 2.5) +
        scale_shape_manual(values = 1:optimal_k_activity) + # Utilizziamo forme fino a k ottimale
        labs(title = paste("Clusters for Activity:", current_activity, "(PCA Visualization)"),
             color = "Subject",
             shape = "Cluster") +
        theme_minimal()
      print(plot_activity)
    } else {
      cat("PCA ha prodotto meno di 2 componenti, saltiamo la visualizzazione per questa attività.\n")
    }
    
  } else {
    cat("Solo un soggetto per questa attività, saltiamo clustering e valutazione.\n")
  }
}






























library(ggplot2)
library(dplyr)
library(cluster)
library(ClusterR)
library(mclust)   # Per adjustedRandIndex
library(recipes)  # Per le funzioni recipe

# Presumendo che 'df' sia il dataframe dai passaggi precedenti

activity_features <- df %>%
  select(Subject, Activity, contains("body_acc"), contains("gyro")) %>%
  group_by(Subject, Activity) %>%
  mutate(
    window_id = (row_number() - 1) %/% 64
  ) %>%
  group_by(Subject, Activity, window_id) %>%
  summarise(
    across(c(contains("acc"), contains("gyro")),
           list(mean = mean,
                sd = sd,
                variance = var,
                min = min,
                max = max,
                energy = ~sum(.^2)/length(.),
                zcr = ~sum(abs(diff(. > 0)))),
           .names = "{.col}_{.fn}")
  ) %>%
  ungroup()

# Creiamo una label binaria 'Movement' basata sulla mappatura delle attività
activity_features <- activity_features %>%
  mutate(Movement = case_when(
    Activity %in% 1:3 ~ "Moving", # Le attività 1, 2, 3 sono di movimento
    Activity %in% 4:6 ~ "Still",  # Le attività 4, 5, 6 sono statiche
    TRUE ~ "Other"
  )) %>%
  filter(Movement != "Other")  # Rimuoviamo le attività 'Other' se vogliamo focalizzarci su Moving vs Still

unique_activities <- unique(activity_features$Activity)

for (current_activity in unique_activities) {
  # 1. Filtriamo i dati per l'attività corrente
  activity_data_current <- activity_features %>% filter(Activity == current_activity)
  
  # 2. Determiniamo k (numero di soggetti unici nell'attività corrente)
  optimal_k_activity <- n_distinct(activity_data_current$Subject)
  cat(sprintf("\n--- Activity: %s ---\n", current_activity))
  cat(sprintf("Number of unique subjects for Activity %s: %d\n", current_activity, optimal_k_activity))
  
  # 3. Recipe di Pre-elaborazione per l'attività corrente
  preproc_recipe_activity <- recipe(~ ., data = activity_data_current) %>%
    step_rm(Movement, Subject, Activity, window_id) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_pca(all_numeric_predictors(), num_comp = 10, threshold = 0.95) %>%
    step_zv(all_predictors())
  
  prepped_data_juiced_activity <- prep(preproc_recipe_activity) %>% juice()
  prepped_data_activity <- as.matrix(prepped_data_juiced_activity)
  
  # 4. Clustering K-means per l'attività corrente (se più di un soggetto)
  if (optimal_k_activity > 1) {
    kmeans_model_activity <- KMeans_rcpp(prepped_data_activity, optimal_k_activity,
                                         num_init = 10, seed = 42)
    clusters_activity <- kmeans_model_activity$clusters
    
    # 5. Valutazione rispetto al Soggetto per l'attività corrente
    ari_activity <- adjustedRandIndex(clusters_activity, activity_data_current$Subject)
    v_measure_activity_result <- calculate_vmeasure(clusters_activity, activity_data_current$Subject)
    v_score_activity <- v_measure_activity_result$v_measure
    cat(sprintf("Optimal k (Subjects): %d, ARI (vs Subject): %.3f, V-measure (vs Subject): %.3f\n",
                optimal_k_activity, ari_activity, v_score_activity))
    
    # 6. Test di Permutazione: shuffling delle label per generare distribuzioni nulle per ARI e V-measure
    n_permutations <- 1000  # Regoliamo se necessario
    null_ari <- numeric(n_permutations)
    null_v <- numeric(n_permutations)
    true_labels <- activity_data_current$Subject
    
    for (perm in 1:n_permutations) {
      # Shuffliamo le label del soggetto (questo interrompe qualsiasi vera associazione)
      shuffled_labels <- sample(true_labels)
      null_ari[perm] <- adjustedRandIndex(clusters_activity, shuffled_labels)
      null_v[perm] <- calculate_vmeasure(clusters_activity, shuffled_labels)$v_measure
    }
    
    # Calcoliamo le metriche nulle medie e i p-value
    avg_null_ari <- mean(null_ari)
    avg_null_v <- mean(null_v)
    p_value_ari <- (sum(null_ari >= ari_activity) + 1) / (n_permutations + 1)
    p_value_v <- (sum(null_v >= v_score_activity) + 1) / (n_permutations + 1)
    
    cat(sprintf("Test di permutazione (n=%d): Media ARI nulla: %.3f, p-value ARI: %.3f\n",
                n_permutations, avg_null_ari, p_value_ari))
    cat(sprintf("Test di permutazione (n=%d): Media V-measure nulla: %.3f, p-value V: %.3f\n",
                n_permutations, avg_null_v, p_value_v))
    
    # 7. Visualizzazione (PCA per l'attività corrente, colorato per Soggetto, formato per cluster)
    viz_data_activity <- as.data.frame(prepped_data_juiced_activity[, 1:2])
    if (ncol(viz_data_activity) >= 2) {
      colnames(viz_data_activity) <- paste0("PC", 1:ncol(viz_data_activity))
      
      viz_data_activity$cluster <- factor(clusters_activity)
      viz_data_activity$Subject <- factor(activity_data_current$Subject)
      
      plot_activity <- ggplot(viz_data_activity, aes(x = PC1, y = PC2, color = Subject, shape = cluster)) +
        geom_point(alpha = 0.7, size = 2.5) +
        scale_shape_manual(values = 1:optimal_k_activity) +
        labs(title = paste("Clusters for Activity:", current_activity, "(PCA Visualization)"),
             color = "Subject",
             shape = "Cluster") +
        theme_minimal()
      print(plot_activity)
    } else {
      cat("PCA ha prodotto meno di 2 componenti, saltiamo la visualizzazione per questa attività.\n")
    }
    
  } else {
    cat("Solo un soggetto per questa attività, saltiamo clustering e valutazione.\n")
  }
}

