# 1. Préparer la variable cible pour la Régression Logistique (GLM)
# 'is_genuine' doit être transformée en facteur pour la classification binaire.
# R utilisera implicitement 'False' (ou 0) comme niveau de référence.
df_clean$target <- as.factor(df_clean$is_genuine)

# 2. Entraînement du modèle de Régression Logistique (GLM)
# L'utilisation explicite des variables (au lieu de '.') est plus sûre.
modele_logistique_R <- glm(target ~ diagonal + height_left + height_right + margin_low + margin_up + length, 
                           data = df_clean, 
                           family = binomial(link = "logit"))

# 3. Affichage du résumé statistique détaillé (pour votre rapport !)
print("--- Résultat du Modèle de Régression Logistique (GLM) ---")
summary(modele_logistique_R)

# 4. Affichage des Odd Ratios et des intervalles de confiance (Très pertinent pour le rapport)
print("--- Odds Ratios ---")
exp(cbind(OR = coef(modele_logistique_R), confint(modele_logistique_R)))
