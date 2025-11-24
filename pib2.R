# Packages
library(eurostat)
library(dplyr)
library(lubridate)
library(tidyr)

# ----------------------------
# 1) PIB trimestriel (volume)
# ----------------------------

pib <- get_eurostat("namq_10_gdp", time_format = "date") %>%
  filter(geo == "PT",
         na_item == "B1GQ",   # PIB en volume chaîné
         s_adj == "SCA", 
         unit == "CLV15_MEUR") %>% 
  mutate(potentiel = hpfilter(values, freq = 1600)$trend,
         output_gap = 100 * (values - potentiel) / potentiel,
         pct_gdp_growth = (values / dplyr::lag(values, 4) - 1) * 100)

# ----------------------------
# 2) Décomposition du PIB
# ----------------------------

pib_comp <- get_eurostat("namq_10_gdp", time_format = "date") %>%
  filter(geo == "PT",
         s_adj == "SCA",
         unit == "CLV15_MEUR",
         na_item %in% c("P3",       # consommation des ménages
                        "P51G",     # FBCF
                        "P6",       # exportations
                        "P7",       # importations
                        "P52P53"    # variation des stocks
         )) %>%
  filter(TIME_PERIOD >= as.Date("2023-01-01"))

# ----------------------------
# 3) Valeur ajoutée par branche A10
# ----------------------------

va <- get_eurostat("namq_10_a10", time_format = "date") %>%
  filter(geo == "PT",
         unit == "CLV10_MNAC",
         s_adj == "SCA",
         unit == "CLV15_MEUR") %>%
  filter(TIME_PERIOD >= as.Date("2024-01-01"))


# ----------------------------
# 4) Indicateurs de confiance — ESI
# ----------------------------

confiance <- get_eurostat("ei_bsin_m_r2", time_format = "date") %>%
  filter(geo == "PT",
         TIME_PERIOD >= as.Date("2024-01-01"))

# ----------------------------
# Aperçu des datasets
# ----------------------------

list(
  pib = head(pib),
  pib_comp = head(pib_comp),
  va = head(va),
  confiance = head(confiance)
)

# ----------------------------
# Graphiques
# ----------------------------


ggplot(pib, aes(TIME_PERIOD, values)) +
  geom_line(size=1) +
  geom_line(aes(y = potentiel), col = "red", size = 0.5) +
  labs(
    title = "Portugal – PIB en volume et PIB potentiel",
    subtitle = "Données trimestrielles, base 2015 = 100",
    x = "Date",
    y = "Millions d’euros en volume",
    caption = "Source : Eurostat (namq_10_gdp)"
  ) +
  theme_minimal()

table_pib_qoq <- pib %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    croissance_qoq = round((values/lag(values) - 1)*100, 2)
  ) %>%
  select(TIME_PERIOD, croissance_qoq) %>% 
  filter(TIME_PERIOD >= as.Date("2024-01-01"))

table_pib_qoq


ggplot(pib_comp, aes(TIME_PERIOD, values, color = na_item)) +
  geom_line(size=1) +
  labs(
    title = "Portugal – Consommation vs PIB",
    subtitle = "Données en volume chaîné",
    x = "Date",
    y = "Millions d’euros",
    color = "Agrégat",
    caption = "Source : Eurostat"
  ) +
  theme_minimal(base_size=13)

table_decomp <- pib_comp %>%
  filter(na_item %in% c("P3","P51G","P6","P7","P52P53")) %>%
  mutate(var_qoq = round((values/lag(values) - 1)*100, 2)) %>% 
  select(TIME_PERIOD, var_qoq, na_item) %>% 
  arrange(TIME_PERIOD, na_item) %>%
  pivot_wider(
    names_from = na_item,
    values_from = var_qoq
  ) %>%
  arrange(TIME_PERIOD) %>% 
  filter(TIME_PERIOD >= as.Date("2024-01-01"))

table_decomp
