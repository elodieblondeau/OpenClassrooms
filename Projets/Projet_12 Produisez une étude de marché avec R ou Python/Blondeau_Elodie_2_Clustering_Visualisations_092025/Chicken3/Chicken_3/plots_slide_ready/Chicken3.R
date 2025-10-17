# ------------------------------------------
# Packages
# ------------------------------------------
library(ggplot2)
library(ggrepel)
library(dplyr)

# ------------------------------------------
# Paramètres
# ------------------------------------------
out_dir <- "plots_slide_ready"
if(!dir.exists(out_dir)) dir.create(out_dir)
arrow_mult <- 5  # multiplicateur pour les flèches biplot

# ------------------------------------------
# Remplissage valeurs manquantes pour la Chine
# ------------------------------------------
df_acp_ready[df_acp_ready$Zone == "Chine", "Stabilite_politique"] <- -0.4
df_acp_ready[df_acp_ready$Zone == "Chine", "Inflation_alimentaire_pct"] <- 2.6
# ------------------------------------------
# Variables pour ACP
# ------------------------------------------
# Afficher les colonnes pour décider lesquelles garder
names(vars_export)

vars_export <- df_acp_ready[, c(
  "Disponibilité.alimentaire.en.quantité..kg.personne.an.",
  "Exportations...quantité",
  "Balance_commerciale",
  "Production_par_habitant",
  "Population.totale",
  "Part_population_urbaine",
  "Stabilite_politique",
  "PIB_par_habitant_PPA"
  
)]

cor(vars_export)
# Standardisation
vars_scaled <- scale(vars_export)

# ------------------------------------------
# Réalisation de l'ACP
# ------------------------------------------
res.pca <- prcomp(vars_scaled)
scores <- as.data.frame(res.pca$x)
scores$Pays <- df_acp_ready$Zone
loadings <- as.data.frame(res.pca$rotation)
loadings$Variable <- rownames(loadings)

library(ggplot2)
library(ggrepel)  # Pour des labels qui ne se chevauchent pas

# Calcul de la variance expliquée
var_explained <- res.pca$sdev^2 / sum(res.pca$sdev^2)
var_explained_pct <- var_explained * 100

# Scores des individus
scores <- as.data.frame(res.pca$x)
scores$Pays <- df_acp_ready$Zone

# Loadings des variables
loadings <- as.data.frame(res.pca$rotation)
loadings$Variable <- rownames(loadings)

# Pour adapter la longueur des flèches à l'échelle des scores
mult <- min(
  (max(scores$PC1) - min(scores$PC1)) / (max(loadings$PC1) - min(loadings$PC1)),
  (max(scores$PC2) - min(scores$PC2)) / (max(loadings$PC2) - min(loadings$PC2))
)

# Biplot avec ggplot
p_biplot <- ggplot() +
  # Points des pays
  geom_point(data = scores, aes(x = PC1, y = PC2), color = "steelblue", size = 2) +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = Pays), size = 3) +
  # Flèches des variables
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 0.8) +
  geom_text_repel(data = loadings, aes(x = PC1 * mult, y = PC2 * mult, label = Variable),
                  color = "red", size = 3) +
  labs(title = "Biplot ACP",
       x = paste0("PC1 (", round(var_explained_pct[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained_pct[2], 1), "%)")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

print(p_biplot)

# ------------------------------------------
# Scree plot
# ------------------------------------------

# Création du dataframe pour ggplot
df_var <- data.frame(
  PC = paste0("PC", 1:length(var_explained)),
  Variance = var_explained
)

# Conversion en pourcentage
df_var$Variance_pct <- df_var$Variance * 100

# Calcul de la variance cumulée en %
df_var$Variance_cum_pct <- cumsum(df_var$Variance_pct)

# Tracé du Scree plot avec variance cumulée
p_scree <- ggplot(df_var, aes(x = PC)) +
  geom_bar(aes(y = Variance_pct), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = Variance_cum_pct, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Variance_cum_pct), color = "red", size = 2) +
  geom_text(aes(y = Variance_pct, label = round(Variance_pct, 1)), vjust = -0.5) +
  geom_text(aes(y = Variance_cum_pct, label = paste0(round(Variance_cum_pct, 1), "%")), 
            vjust = -1.2, color = "red", size = 3.5) +
  labs(title = "Éboulis (Scree plot) avec variance cumulée",
       x = "Composantes principales",
       y = "Proportion de variance expliquée (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Affichage
print(p_scree)

# Sauvegarde
ggsave(file.path(out_dir, "01_scree_plot_cum.png"), p_scree, width = 8, height = 5, dpi = 300)

# ------------------------------------------
# Cercle des corrélations
# ------------------------------------------
theta <- seq(0, 2*pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))

# Cercle PC1-PC2
loadings_corr <- loadings
loadings_corr$PC1 <- loadings_corr$PC1 * res.pca$sdev[1]
loadings_corr$PC2 <- loadings_corr$PC2 * res.pca$sdev[2]

p_circle_12 <- ggplot() +
  geom_path(data = circle, aes(x = x, y = y), color = "black", size = 0.7) +
  geom_segment(data = loadings_corr,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), color = "blue", size = 1) +
  geom_text_repel(data = loadings_corr,
                  aes(x = PC1, y = PC2, label = Variable),
                  color = "blue", size = 4,
                  box.padding = 0.3, point.padding = 0.2) +
  labs(title = "Cercle des corrélations (PC1 vs PC2)",
       x = "PC1", y = "PC2") +
  coord_equal() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

print(p_circle_12)
ggsave(file.path(out_dir, "02_cercle_corr_PC1_PC2.png"), p_circle_12, width = 7.5, height = 7.5, dpi = 300)

# Cercle PC1-PC3
loadings_corr$PC1 <- loadings_corr$PC1 * res.pca$sdev[1]
loadings_corr$PC3 <- loadings_corr$PC3 * res.pca$sdev[3]

p_circle_34 <- ggplot() +
  geom_path(data = circle, aes(x = x, y = y), color = "black", size = 0.7) +
  geom_segment(data = loadings_corr,
               aes(x = 0, y = 0, xend = PC3, yend = PC4),
               arrow = arrow(length = unit(0.3, "cm")), color = "blue", size = 1) +
  geom_text_repel(data = loadings_corr,
                  aes(x = PC1, y = PC3, label = Variable),
                  color = "blue", size = 4,
                  box.padding = 0.3, point.padding = 0.2) +
  labs(title = "Cercle des corrélations (PC1 vs PC3)",
       x = "PC1", y = "PC3") +
  coord_equal() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

print(p_circle_34)
ggsave(file.path(out_dir, "03_cercle_corr_PC1_PC3.png"), p_circle_34, width = 7.5, height = 7.5, dpi = 300)


# ------------------------------------------
# CAH sur scores ACP
# ------------------------------------------
dist_matrix <- dist(scores_cluster, method = "euclidean")
cah <- hclust(dist_matrix, method = "ward.D2")
fviz_dend(cah,
          k = NULL,          # ne pas découper en clusters
          rect = FALSE,      # pas de rectangles
          k_colors = NULL,   # pas de palette
          cex = 0.6,         # taille du texte des labels
          lwd = 0.8,         # épaisseur des branches
          main = "Dendrogramme CAH (scores ACP)",
          xlab = "Pays",
          ylab = "Distance")

# ------------------------------------------
# Méthode du coude (Elbow method)
# ------------------------------------------
set.seed(123)

# Données pour le clustering (scores ACP sur 4 dimensions)
scores_cluster <- scores[, c("PC1", "PC2", "PC3", "PC4")]

# Calcul du WSS pour différents k
wss <- sapply(1:10, function(k){
  kmeans(scores_cluster, centers = k, nstart = 25)$tot.withinss
})

# Mise en dataframe
df_wss <- data.frame(
  k = 1:10,
  wss = wss
)

# Tracé du coude
p_elbow <- ggplot(df_wss, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen") +
  annotate("text", x = 4.2, y = wss[4], label = "k = 4", color = "darkgreen", hjust = 0) +
  labs(title = "Méthode du coude",
       x = "Nombre de clusters k",
       y = "Inertie intra-classe (WSS)") +
  theme_minimal(base_size = 14)

print(p_elbow)
ggsave(file.path(out_dir, "06_elbow_method.png"), p_elbow, width = 7, height = 5, dpi = 300)

# ------------------------------------------
# Méthode de la silhouette moyenne
# ------------------------------------------
library(cluster)
library(factoextra)

# Calcul de la silhouette moyenne pour k = 2 à 10
sil_avg <- sapply(2:10, function(k){
  km_res <- kmeans(scores_cluster, centers = k, nstart = 25)
  ss <- silhouette(km_res$cluster, dist(scores_cluster))
  mean(ss[, 3])  # moyenne de la silhouette
})

# Mise en dataframe
df_sil <- data.frame(
  k = 2:10,
  silhouette = sil_avg
)

# Tracé
p_sil <- ggplot(df_sil, aes(x = k, y = silhouette)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgreen") +
  annotate("text", x = 4.2, y = sil_avg[3], label = "k = 4", color = "darkgreen", hjust = 0) +
  labs(title = "Silhouette moyenne selon k",
       x = "Nombre de clusters k",
       y = "Silhouette moyenne") +
  theme_minimal(base_size = 14)

print(p_sil)
ggsave(file.path(out_dir, "07_silhouette_method.png"), p_sil, width = 7, height = 5, dpi = 300)


# ------------------------------------------
# CAH avec fviz_dend
# ------------------------------------------
library(factoextra)

# Calcul de la distance et clustering hiérarchique
dist_matrix <- dist(scores_cluster, method = "euclidean")
cah <- hclust(dist_matrix, method = "ward.D2")

# Dendrogramme avec couleurs pour k = 4 clusters
fviz_dend(cah,
          k = 4,                     # nombre de clusters
          rect = TRUE,               # dessine des rectangles autour des clusters
          rect_border = "steelblue", # couleur des rectangles
          rect_fill = TRUE,          # remplir les rectangles
          k_colors = "jco",          # palette de couleurs (jco, lncbi, etc.)
          cex = 0.6,                 # taille du texte des labels
          lwd = 0.8,                 # épaisseur des branches
          main = "Dendrogramme CAH (scores ACP)",
          xlab = "Pays",
          ylab = "Distance")



# ------------------------------------------
# K-means sur scores ACP (PC1-PC4)
# ------------------------------------------
set.seed(123)
k <- 4
scores_cluster <- scores[, c("PC1", "PC2", "PC3", "PC4")]
km <- kmeans(scores_cluster, centers = k, nstart = 25)
scores$Cluster_Kmeans <- as.factor(km$cluster)

# Projection K-means PC1-PC2
p_kmeans_12 <- ggplot(scores, aes(x = PC1, y = PC2, color = Cluster_Kmeans, label = Pays)) +
  geom_point(size = 3) +
  geom_text_repel(size = 2.5) +
  labs(title = "K-means sur ACP (PC1 vs PC2)", color = "Cluster") +
  theme_minimal()
print(p_kmeans_12)
ggsave(file.path(out_dir, "04_projection_Kmeans_PC1_PC2.png"), p_kmeans_12, width = 9, height = 6, dpi = 300)


library(tidyr)  # pour pivot_longer
library(ggplot2)

# Centroides K-means
centroids <- as.data.frame(km$centers)
centroids$Cluster <- factor(1:k)

# Transformer en long format avec pivot_longer
centroids_long <- centroids %>%
  pivot_longer(cols = starts_with("PC"),  # toutes les colonnes PC
               names_to = "PC",
               values_to = "Value")

# Heatmap
p_centroids <- ggplot(centroids_long, aes(x = PC, y = Cluster, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap des centroides K-means (PC1-PC4)",
       x = "Composantes principales",
       y = "Cluster") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_centroids)

# Sauvegarde
ggsave(file.path(out_dir, "08_heatmap_centroids.png"), p_centroids, width = 7, height = 5, dpi = 300)

## BOX PLOT par cluster selon les données


# Ajouter les clusters aux données originales
df_clusters <- df_acp_ready %>%
  mutate(Cluster = scores$Cluster_Kmeans)

# Disponibilité alimentaire en kg par personne par cluster
ggplot(df_clusters, aes(x = Cluster, y = Disponibilité.alimentaire.en.quantité..kg.personne.an., fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Disponibilité alimentaire en kg par personne par cluster")


# Importation de poulets
ggplot(df_clusters, aes(x = Cluster, y = Importations...quantité, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Importations")

# PIB par habitant
ggplot(df_clusters, aes(x = Cluster, y = PIB_par_habitant_PPA, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "PIB par habitant")



