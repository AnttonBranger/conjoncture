# -------------------------------------------------------------------------
# 1. Chargement des Packages
# -------------------------------------------------------------------------

# install.packages(c("eurostat", "tidyverse", "lubridate", "janitor"))
library(eurostat)
library(tidyverse)
library(lubridate)
library(janitor)

# -------------------------------------------------------------------------
# 2. Paramètres de l'Analyse
# -------------------------------------------------------------------------

PAYS <- "PT"        # Portugal
CODE_PRINCIPAL <- "teibs020" # Base de données pour les enquêtes détaillées

# Codes de variables fournis par l'utilisateur
INDICATEURS_CIBLES <- c(
  "BS-CCI-BAL",   # Confiance des Consommateurs
  "BS-ICI-BAL",   # Confiance Industrielle
  "BS-SCI-BAL",   # Confiance dans les Services
  "BS-RCI-BAL",   # Confiance dans le Commerce de Détail
  "BS-CSMCI-BAL"  # Confiance dans la Construction
)

# Noms lisibles pour les graphiques et tableaux
NOM_INDICATEURS <- c(
  "BS-CCI-BAL" = "Consommateurs",
  "BS-ICI-BAL" = "Industrie",
  "BS-SCI-BAL" = "Services",
  "BS-RCI-BAL" = "Commerce Détail",
  "BS-CSMCI-BAL" = "Construction"
)

# Période d'étude du projet (Janvier 2024 - Décembre 2025)
DATE_DEBUT <- ymd("2023-01-01")
DATE_FIN <- ymd("2025-12-01") 

# -------------------------------------------------------------------------
# 3. Fonction de Récupération des Données (SANS SIMULATION)
# -------------------------------------------------------------------------

#' Récupère et filtre les données de confiance Eurostat
charger_donnees_confiance <- function(code_bdd) {
  
  message(paste("-> Récupération réelle de la BDD Eurostat :", code_bdd, "pour le Portugal (PT)..."))
  
  # Récupération réelle des données Eurostat
  df_brut <- get_eurostat(
    id = code_bdd,
    filters = list(
      geo = PAYS,
      freq = "M",
      s_adj = "SA" # Séries Désaisonnalisées
    )
  ) 
  
  # --- Filtrage et Préparation ---
  df_filtre <- df_brut %>%
    # S'assurer que seuls les indicateurs ciblés sont inclus
    filter(indic %in% INDICATEURS_CIBLES) %>%
    # Renommer la variable de temps et la mettre au format date
    rename(Date = time) %>%
    mutate(Date = floor_date(Date, "month")) %>%
    # Filtrer sur la période d'étude
    filter(Date >= DATE_DEBUT & Date <= DATE_FIN) %>%
    # Sélectionner les colonnes pertinentes
    select(Date, indic, values) %>%
    # Remplacer les codes bruts par des noms lisibles
    mutate(indic_bt = recode(indic, !!!NOM_INDICATEURS)) %>% 
    select(-indic)
  
  if (nrow(df_filtre) == 0) {
    stop("Aucune donnée trouvée pour la période et les filtres spécifiés. Vérifiez les dates, les codes ou la connexion.")
  }
  
  return(df_filtre)
}

# -------------------------------------------------------------------------
# 4. Collecte, Préparation et Analyse des Données
# -------------------------------------------------------------------------

df_confiance_long <- charger_donnees_confiance(CODE_PRINCIPAL)

# Passage au format large pour les calculs de variation
df_confiance_wide <- df_confiance_long %>%
  pivot_wider(names_from = indic_bt, values_from = values) %>%
  arrange(Date)

# Noms des colonnes pour l'analyse (après pivot)
cols_a_analyser <- unname(NOM_INDICATEURS)

# --- Calcul des Variations ---

# Calcul de la variation M/M-1 et M/M-12 (en points de solde)
df_analyse <- df_confiance_wide %>%
  mutate(
    across(
      .cols = all_of(cols_a_analyser),
      .fns = list(
        var_mm1 = ~ .x - lag(.x, n = 1L),
        var_mm12 = ~ .x - lag(.x, n = 12L)
      ),
      .names = "{.fn}_{.col}"
    )
  )

# --- Résumé des Dernières Données ---
dernier_mois <- df_analyse %>% 
  tail(1) %>% 
  select(Date, all_of(cols_a_analyser), starts_with("var"))

# -------------------------------------------------------------------------
# 5. Affichage des Résultats Clés
# -------------------------------------------------------------------------

cat("\n========================================================================\n")
cat("ANALYSE SECTORIELLE DES INDICATEURS DE CONFIANCE (PORTUGAL - SÉRIES DÉS.)\n")
cat("========================================================================\n")

cat("\nDERNIÈRES DONNÉES DISPONIBLES (Niveau et variations en points de solde) :\n")
dernier_mois %>%
  pivot_longer(cols = -Date, names_to = "Indicateur", values_to = "Valeur") %>%
  mutate(
    Type = case_when(
      grepl("var_mm12", Indicateur) ~ "Var M/M-12 (pts)",
      grepl("var_mm1", Indicateur) ~ "Var M/M-1 (pts)",
      .default = "Niveau (points)"
    ),
    Indicateur = gsub("var_mm12_|var_mm1_", "", Indicateur)
  ) %>%
  pivot_wider(names_from = Type, values_from = Valeur) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  print(n=Inf)

cat("\n========================================================================\n")
cat("TABLEAU COMPLET DES VARIATIONS M/M-1 (12 derniers mois) :\n")
cat("========================================================================\n")
df_analyse %>% 
  tail(12) %>%
  select(Date, starts_with("var_mm1")) %>%
  rename_with(~gsub("var_mm1_", "", .x), starts_with("var_mm1")) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  print(n=Inf)

# -------------------------------------------------------------------------
# 6. Visualisation Graphique
# -------------------------------------------------------------------------

# Plot en utilisant le format 'long'
g <- df_confiance_long %>%
  ggplot(aes(x = Date, y = values, color = indic_bt)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  # Ligne de référence à 0 (point d'équilibre pour tous ces indicateurs de solde)
  geom_hline(
    yintercept = 0, 
    linetype = "dotted", 
    color = "red", 
    alpha = 0.7
  ) +
  labs(
    title = paste("Indicateurs de Confiance Sectoriels au Portugal (PT) : ", format(DATE_DEBUT, "%b %Y"), " - ", format(DATE_FIN, "%b %Y"), sep = ""),
    subtitle = "Indices de Solde (Séries Désaisonnalisées - SA). L'équilibre est à 0.",
    y = "Solde (Points)",
    x = "Date",
    color = "Secteur"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Affichage du graphique
print(g)
