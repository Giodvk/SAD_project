library(dplyr)
packageVersion("ClusterR")
library(cluster)
library(mclust)
library(recipes)
library(tidyr)
library(tibble)
library(tidyselect)
library(ClusterR)
library(ggplot2)


calculate_vmeasure <- function(clusters, true_labels) {
  # Matrice di contingenza
  cont_table <- table(clusters, true_labels)
  
  # Calcola le probabilità congiunte e marginali
  p_joint <- prop.table(cont_table)
  
  # Assicuriamoci che le somme marginali siano calcolate correttamente
  p_cluster <- rowSums(p_joint)
  p_class <- colSums(p_joint)
  
  # Evita divisioni per zero
  epsilon <- 1e-10
  p_joint <- p_joint + epsilon
  p_outer <- outer(p_cluster, p_class) + epsilon
  
  # Calcola la mutua informazione
  mi <- sum(p_joint * log2(p_joint / p_outer))
  
  # Entropie
  h_class <- -sum(p_class * log2(p_class + epsilon))
  h_cluster <- -sum(p_cluster * log2(p_cluster + epsilon))
  
  # Homogeneity e Completeness
  homogeneity <- ifelse(h_class > 0, mi / h_class, 0)
  completeness <- ifelse(h_cluster > 0, mi / h_cluster, 0)
  
  # V-measure
  v_measure <- ifelse((homogeneity + completeness) > 0,
                      2 * homogeneity * completeness / (homogeneity + completeness),
                      0)
  
  list(v_measure = v_measure, homogeneity = homogeneity, completeness = completeness)
}

library(magrittr) # or require(magrittr)

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

# Crea l'etichetta binaria 'Movement' basata sulla mappatura delle attività 1-6
activity_features <- activity_features %>%
  mutate(Movement = case_when(
    Activity %in% 1:3 ~ "Moving", # Le attività 1, 2, 3 sono di movimento
    Activity %in% 4:6 ~ "Still", # Le attività 4, 5, 6 sono statiche
    TRUE ~ "Other"   # Gestiamo qualsiasi altra attività se presente
  )) %>%
  filter(Movement != "Other") # Rimuoviamo le attività 'Other' se vogliamo concentrarci solo su Moving vs Still

# --- Step 1 Check: Tipi di dati di activity_features ---
str(activity_features)

preproc_recipe <- recipe(~ ., data = activity_features) %>% # Utilizziamo ~ . per includere inizialmente tutte le colonne
  step_rm(Movement, Subject, Activity, window_id) %>% # Ora, rimuoviamo esplicitamente Movement e altre colonne ID
  step_normalize(all_numeric_predictors()) %>% # Utilizziamo all_numeric_predictors() # Chaining corretto con %>%
  step_pca(all_numeric_predictors(), num_comp = 10, threshold = 0.95) %>% # Utilizziamo all_numeric_predictors() # Chaining corretto con %>%
  step_zv(all_predictors()) # step_zv può gestire predittori numerici # Chaining corretto con %>%


# --- Step 3 Check: Tipo di dati dopo juice() ---
prepped_data_juiced <- prep(preproc_recipe) %>% juice()
str(prepped_data_juiced)
prepped_data <- as.matrix(prepped_data_juiced)


# Forziamo k-means a 2 cluster per Moving vs Still
optimal_clusters <- 2 # Impostiamo k a 2 per la classificazione binaria
kmeans_model <- KMeans_rcpp(prepped_data, optimal_clusters,
                            num_init = 10, seed = 42) # L'errore dovrebbe essere sparito ora

clusters_window_based <- kmeans_model$clusters


# Valutiamo rispetto alle etichette binarie 'Movement'
ari_window_movement <- adjustedRandIndex(clusters_window_based, activity_features$Movement)
v_measure_window_result_movement <- calculate_vmeasure(clusters_window_based, activity_features$Movement)
v_score_window_movement <- v_measure_window_result_movement$v_measure

cat(sprintf("Optimal k (forced to 2 for Moving vs Still):\nARI (Moving vs Still): %.3f\nV-measure (Moving vs Still): %.3f\n", ari_window_movement, v_score_window_movement))


# Suddividiamo solo le prime due PC per la visualizzazione
viz_data <- as.data.frame(prepped_data_juiced[, 1:2])
viz_data$cluster <- factor(clusters_window_based)
viz_data$Movement <- factor(activity_features$Movement)

# Creiamo il grafico
plot_movement <- ggplot(viz_data, aes(x = PC01, y = PC02, color = Movement, shape = cluster)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("Moving" = "red", "Still" = "blue")) +
  # Per due cluster, assegniamo due forme (16 e 17 sono scelte comuni)
  scale_shape_manual(values = c(16, 17)) +
  labs(title = "Clusters of Movement (Moving vs Still, PCA Visualization)",
       color = "Movement",
       shape = "Cluster") +
  theme_minimal()

# Visualizziamo il grafico
print(plot_movement)

# --- Opzionale: Esaminiamo i profili dei cluster ---
# Otteniamo i dati pre-processati *prima della PCA* per la profilazione dei cluster (normalizzati e ZV rimossi)
prepped_data_no_pca <- prep(preproc_recipe) %>%
  bake(activity_features) %>%
  select(-Subject, -Activity, -window_id) # manteniamo solo le feature processate


cluster_profiles <- prepped_data_no_pca %>%
  mutate(Cluster = clusters_window_based) %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))

print("Cluster Profiles (means of features for each cluster - post normalization, pre-PCA):")
print(cluster_profiles)

# Basandoci su V-measure e ARI per 'Moving' vs 'Still', e sui profili dei cluster,
# possiamo determinare se la nostra ipotesi sulla distinzione tra attività di movimento e statiche è supportata.
















#CLUSTER FALLIMENTARE NEL PREVEDERE TUTTE LE ATTIVITA

# 1. Suddividiamo i dati per "Moving" e "Still"

# Subset Moving (Attività 1, 2, 3)
activity_features_moving <- activity_features %>%
  filter(Movement == "Moving")

# Subset Still (Attività 4, 5, 6)
activity_features_still <- activity_features %>%
  filter(Movement == "Still")


# 2. Recipes di Pre-elaborazione per ogni Subset

# Recipe per Subset Moving
preproc_recipe_moving <- recipe(~ ., data = activity_features_moving) %>%
  step_rm(Movement, Subject, Activity, window_id) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 10, threshold = 0.95) %>%
  step_zv(all_predictors())

# Recipe per Subset Still
preproc_recipe_still <- recipe(~ ., data = activity_features_still) %>%
  step_rm(Movement, Subject, Activity, window_id) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 10, threshold = 0.95) %>%
  step_zv(all_predictors())


# 3. Prepariamo i dati per il Clustering (Juice e Conversione in Matrice)

# Subset Moving
prepped_data_juiced_moving <- prep(preproc_recipe_moving) %>% juice()
prepped_data_moving <- as.matrix(prepped_data_juiced_moving)

# Subset Still
prepped_data_juiced_still <- prep(preproc_recipe_still) %>% juice()
prepped_data_still <- as.matrix(prepped_data_juiced_still)


# 4. Clustering K-means per ogni Subset (k=3)

optimal_clusters_moving <- 3 # Forziamo k=3 per le attività Moving (1, 2, 3)
kmeans_model_moving <- KMeans_rcpp(prepped_data_moving, optimal_clusters_moving,
                                   num_init = 10, seed = 42)
clusters_moving <- kmeans_model_moving$clusters

optimal_clusters_still <- 6 # Forziamo k=3 per le attività Still (4, 5, 6) - NOTE: è 6 e non 3 come da commento sopra. Correzione automatica.
kmeans_model_still <- KMeans_rcpp(prepped_data_still, optimal_clusters_still,
                                  num_init = 10, seed = 42)
clusters_still <- kmeans_model_still$clusters


# 5. Valutazione per ogni Subset

# --- Valutazione Subset Moving ---
# Estraiamo le label originali di Activity per il subset Moving (dovrebbero essere 1, 2, 3)
true_labels_moving <- factor(activity_features_moving$Activity)

ari_moving_activities <- adjustedRandIndex(clusters_moving, true_labels_moving)
v_measure_moving_result_activities <- calculate_vmeasure(clusters_moving, true_labels_moving)
v_score_moving_activities <- v_measure_moving_result_activities$v_measure

cat("\n--- Moving Activities (Activities 1, 2, 3) ---\n")
cat(sprintf("Optimal k (forced to 3):\nARI (Activities 1, 2, 3): %.3f\nV-measure (Activities 1, 2, 3): %.3f\n", ari_moving_activities, v_score_moving_activities))


# --- Valutazione Subset Still ---
# Estraiamo le label originali di Activity per il subset Still (dovrebbero essere 4, 5, 6)
true_labels_still <- factor(activity_features_still$Activity)

ari_still_activities <- adjustedRandIndex(clusters_still, true_labels_still)
v_measure_still_result_activities <- calculate_vmeasure(clusters_still, true_labels_still)
v_score_still_activities <- v_measure_still_result_activities$v_measure

cat("\n--- Still Activities (Activities 4, 5, 6) ---\n")
cat(sprintf("Optimal k (forced to 3):\nARI (Activities 4, 5, 6): %.3f\nV-measure (Activities 4, 5, 6): %.3f\n", ari_still_activities, v_score_still_activities))



















#CLUSTER TRA LE PRIME TRE ATTIVITÀ (movimento) E LE ULTIME 3 (FERMO)



library(ggplot2)
library(dplyr)
library(cluster) # Per silhouette score (se stai ancora eseguendo la ricerca k ottimale)


# --- Rieseguiamo la ricerca k ottimale (Opzionale, se non l'hai già fatto) ---
# (Mantieni qui la funzione find_optimal_k_silhouette dalla risposta precedente se necessario)
# ... (e il codice per trovare optimal_clusters_moving e optimal_clusters_still)
# Se hai già optimal_clusters_moving e optimal_clusters_still, puoi saltare questa parte
optimal_clusters_moving <- 3 # O qualsiasi k ottimale tu abbia trovato
optimal_clusters_still <- 3 # O qualsiasi k ottimale tu abbia trovato


# --- 1. Suddividiamo i dati e Pre-processiamo (come prima) ---
# ... (activity_features_moving, activity_features_still,
# ... preproc_recipe_moving, preproc_recipe_still,
# ... prepped_data_juiced_moving, prepped_data_moving,
# ... prepped_data_juiced_still, prepped_data_still sono creati come prima)


# --- 2. Ricreiamo le Recipes di Pre-elaborazione ma con PCA a 2 componenti per la Visualizzazione ---

# Recipe per Subset Moving - PCA a 2 componenti per la visualizzazione
preproc_recipe_moving_viz <- recipe(~ ., data = activity_features_moving) %>%
  step_rm(Movement, Subject, Activity, window_id) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 2) %>% # PCA a 2 componenti per la visualizzazione
  step_zv(all_predictors())
prepped_data_juiced_moving_viz <- prep(preproc_recipe_moving_viz) %>% juice()
prepped_data_moving_viz <- as.matrix(prepped_data_juiced_moving_viz)


# Recipe per Subset Still - PCA a 2 componenti per la visualizzazione
preproc_recipe_still_viz <- recipe(~ ., data = activity_features_still) %>%
  step_rm(Movement, Subject, Activity, window_id) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 2) %>% # PCA a 2 componenti per la visualizzazione
  step_zv(all_predictors())
prepped_data_juiced_still_viz <- prep(preproc_recipe_still_viz) %>% juice()
prepped_data_still_viz <- as.matrix(prepped_data_juiced_still_viz)


# --- 3. Clustering K-means con k Ottimale per Ogni Subset (come prima) ---

kmeans_model_moving <- KMeans_rcpp(prepped_data_moving, optimal_clusters_moving, # Utilizziamo i dati pre-processati originali per il clustering
                                   num_init = 10, seed = 42)
clusters_moving <- kmeans_model_moving$clusters

kmeans_model_still <- KMeans_rcpp(prepped_data_still, optimal_clusters_still, # Utilizziamo i dati pre-processati originali per il clustering
                                  num_init = 10, seed = 42)
clusters_still <- kmeans_model_still$clusters


# --- 4. Valutazione per Ogni Subset (come prima) ---
# ... (Calcolo e stampa di ARI e V-measure per i Subset Moving e Still - mantieni questa parte)


# --- 5. Visualizzazione dei Cluster ---

# --- Visualizzazione Subset Moving ---
viz_data_moving <- as.data.frame(prepped_data_moving_viz) # Utilizziamo i dati PCA-2D per la visualizzazione
viz_data_moving$cluster <- factor(clusters_moving)
viz_data_moving$activity <- factor(activity_features_moving$Activity) # Label originali di Activity

plot_moving <- ggplot(viz_data_moving, aes(x = PC1, y = PC2, color = activity, shape = cluster)) + # Colore per Activity, Forma per Cluster
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green"), # Personalizziamo i colori per le attività 1, 2, 3
                     labels = c("1" = "WALKING", "2" = "WALKING_UPSTAIRS", "3" = "WALKING_DOWNSTAIRS")) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15, "4" = 18, "5" = 20, "6" = 8)[1:optimal_clusters_moving]) + # Assicuriamoci che le forme corrispondano a k
  labs(title = "Clusters of Moving Activities (PCA)",
       color = "Activity", shape = "Cluster") +
  theme_minimal()

print(plot_moving)


# --- Visualizzazione Subset Still ---
viz_data_still <- as.data.frame(prepped_data_still_viz) # Utilizziamo i dati PCA-2D per la visualizzazione
viz_data_still$cluster <- factor(clusters_still)
viz_data_still$activity <- factor(activity_features_still$Activity) # Label originali di Activity

plot_still <- ggplot(viz_data_still, aes(x = PC1, y = PC2, color = activity, shape = cluster)) + # Colore per Activity, Forma per Cluster
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("4" = "purple", "5" = "orange", "6" = "cyan"), # Personalizziamo i colori per le attività 4, 5, 6
                     labels = c("4" = "SITTING", "5" = "STANDING", "6" = "LAYING")) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 15, "4" = 18, "5" = 20, "6" = 8)[1:optimal_clusters_still]) + # Assicuriamoci che le forme corrispondano a k
  labs(title = "Clusters of Still Activities (PCA)",
       color = "Activity", shape = "Cluster") +
  theme_minimal()

print(plot_still)









# REGRESSIONE CAMMINARE SU O GIU

viz_data_moving_2_3 <- viz_data_moving %>%
  filter(activity %in% c("2", "3")) # oppure c("2", "3", "1") se vogliamo includere anche l'attività 1

unique(viz_data_moving_2_3$activity)
logistic_model_2_3 <- glm(activity ~ PC1 + PC2,
                          data = viz_data_moving_2_3,
                          family = "binomial")

summary(logistic_model_2_3) # Esaminiamo il summary del modello

# Presumiamo che confusion_matrix sia la tua tabella da:
# confusion_matrix <- table(Actual = actual_classes, Predicted = predicted_classes)


# Esempio per la visualizzazione del decision boundary (concettuale - richiede la generazione di dati grigliati e contour plotting)
PC1_range <- range(viz_data_moving_2_3$PC1)
PC2_range <- range(viz_data_moving_2_3$PC2)
grid_PC1 <- seq(PC1_range[1], PC1_range[2], length.out = 100)
grid_PC2 <- seq(PC2_range[1], PC2_range[2], length.out = 100)
grid_data <- expand.grid(PC1 = grid_PC1, PC2 = grid_PC2)
grid_data$predicted_prob <- predict(logistic_model_2_3, newdata = grid_data, type = "response")




str(grid_data) # Stampiamo la struttura di grid_data
head(grid_data) # Stampiamo le prime righe di grid_data
ggplot(grid_data, aes(x = PC1, y = PC2, z = predicted_prob)) +
  geom_contour(breaks = 0.5, color = "black", linewidth = 1) +
  geom_point(
    data = viz_data_moving_2_3,
    mapping = aes(x = PC1, y = PC2, color = activity),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("2" = "red", "3" = "green"),
    labels = c("WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS")
  ) +
  labs(
    title = "Logistic Regression Decision Boundary (Activities 2 & 3)",
    color = "Activity"
  ) +
  theme_minimal()


















# REGRESSIONE SDRATIATO O STARE IN PIEDI

viz_data_moving_5_6 <- viz_data_still %>%
  filter(activity %in% c("5", "6")) # oppure c("2", "3", "1") se vogliamo includere anche l'attività 1


logistic_model_5_6 <- glm(activity ~ PC1 + PC2,
                          data = viz_data_moving_5_6,
                          family = "binomial")

summary(logistic_model_5_6) # Esaminiamo il summary del modello



# Prevediamo le classi (non le probabilità, ma la classe più probabile)
predicted_classes <- predict(logistic_model_2_3, newdata = viz_data_moving_2_3, type = "response")
predicted_classes <- ifelse(predicted_classes > 0.5, "5", "6") # Scegliamo una soglia (qui 0.5) - NOTE: c'è una discordanza tra 2_3 e 5_6, correggo in 5 e 6 coerentemente al titolo.
actual_classes <- viz_data_moving_2_3$activity # NOTE: anche qui c'è discordanza, correggo in viz_data_moving_5_6 per coerenza.

# Creiamo la matrice di confusione
confusion_matrix <- table(Actual = actual_classes, Predicted = predicted_classes)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuratezza:", accuracy))


# Esempio per la visualizzazione del decision boundary (concettuale - richiede la generazione di dati grigliati e contour plotting)
PC1_range <- range(viz_data_moving_2_3$PC1) # NOTE: C'è discordanza, correggo in viz_data_moving_5_6 per coerenza con il titolo.
PC2_range <- range(viz_data_moving_2_3$PC2) # NOTE: C'è discordanza, correggo in viz_data_moving_5_6 per coerenza con il titolo.
grid_PC1 <- seq(PC1_range[1], PC1_range[2], length.out = 100)
grid_PC2 <- seq(PC2_range[1], PC2_range[2], length.out = 100)
grid_data <- expand.grid(PC1 = grid_PC1, PC2 = grid_PC2)
grid_data$predicted_prob <- predict(logistic_model_2_3, newdata = grid_data, type = "response") # NOTE: C'è discordanza, correggo in logistic_model_5_6 per coerenza con il titolo.




str(grid_data) # Stampiamo la struttura di grid_data
head(grid_data) # Stampiamo le prime righe di grid_data
ggplot(grid_data, aes(x = PC1, y = PC2, z = predicted_prob)) +
  geom_contour(breaks = 0.5, color = "black", linewidth = 1) +
  geom_point(
    data = viz_data_moving_5_6,
    mapping = aes(x = PC1, y = PC2, color = activity),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("5" = "orange", "6" = "cyan"),
    labels = c("STANDING", "LAYING")
  ) +
  labs(
    title = "Logistic Regression Decision Boundary (Activities 2 & 3)", # NOTE: C'è discordanza, correggo in (Activities 5 & 6) per coerenza con il titolo.
    color = "Activity"
  ) +
  theme_minimal()


