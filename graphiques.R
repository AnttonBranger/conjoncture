#######################
####    PIB    ########
#######################

ggplot(pib_clean2, aes(x = YEAR)) +
  geom_line(aes(y = values), size = 1) +
  geom_line(aes(y = VALUE), col = "red", size = 0.5) +
  labs(
    title = "Évolution du PIB en volume et PIB potentiel",
    x = "Année",
    y = "PIB (millions d’euros)",
    caption = "Source : Eurostat (namq_10_gdp)"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(pib_clean2, aes(x = YEAR, y = output_gap)) +
  geom_hline(yintercept = 0, col = "red") +
  geom_line() +
  labs(
    title = "Output Gap — Protugal",
    subtitle = "Écart de production en % du PIB potentiel",
    x = "Année",
    y = "Output gap (%)",
    caption = "Source : Eurostat (namq_10_gdp)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

ggplot(pib_clean2 %>% filter(YEAR >= 2015), aes(x = YEAR)) +
  
  # Barres Output Gap
  geom_col(aes(y = output_gap, fill = output_gap > 0)) +
  scale_fill_manual(values = c("TRUE" = "#72B22F", "FALSE" = "#E67E22"), guide = "none") +
  
  # Ligne croissance du PIB
  geom_line(aes(y = pct_gdp_growth * 1), linewidth = 1.1) +
  
  # Deux axes Y
  scale_y_continuous(
    name = "Output Gap (%)",
    sec.axis = sec_axis(~ ., name = "Taux de croissance du PIB réel (%)")
  ) +
  
  # Titres
  labs(
    title = "Taux de croissance du PIB réel et Output Gap",
    subtitle = "Données trimestrielles",
    x = "Année",
    caption = "Source : Eurostat (namq_10_gdp)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y.right = element_text(color = "black")
  )

#######################
###  Consommation  ####
#######################

ggplot(conso, aes(TIME_PERIOD, values)) +
  geom_line(size = 1.1, color = "darkblue") +
  labs(
    title = "Portugal – Consommation des ménages",
    subtitle = "En millions d’euros constants",
    y = "",
    x = "",
    caption = "Source : Eurostat (nama_10_gdp)"
  ) +
  theme_minimal(base_size = 13)

confiance_menages %>% 
  filter(TIME_PERIOD >= as.Date("2024-01-01")) %>% 
  ggplot(aes(TIME_PERIOD, values)) +
  geom_line(size = 1.1, color = "darkorange3") +
  labs(
    title = "Portugal – Indicateur de confiance des consommateurs",
    y = "Solde d’opinion",
    x = "",
    caption = "Source : Eurostat (ei_bsco_m)"
  ) +
  theme_minimal(base_size = 13)

#######################
### Marche travail ####
#######################

ggplot(chomage, aes(TIME_PERIOD, values)) +
  geom_line(size=1.1, color="darkred") +
  labs(
    title = "Portugal – Taux de chômage",
    subtitle = "Données mensuelles, désaisonnalisées",
    y = "%",
    x = "Date",
    caption = "Source : Eurostat (une_rt_m)"
  ) +
  theme_minimal(base_size = 13)

emploi %>% 
  filter(TIME_PERIOD >= as.Date("2024-01-01")) %>% 
  ggplot(aes(TIME_PERIOD, values)) +
  geom_line(color="steelblue", size=1.2) +
  labs(
    title = "Portugal – Emploi total (LFS)",
    subtitle = "15–74 ans, en milliers de personnes",
    x = "Date",
    y = "Milliers",
    caption = "Source : Eurostat (lfsq_egan)"
  ) +
  theme_minimal(base_size = 13)

vacancies %>% 
  filter(TIME_PERIOD >= as.Date("2024-01-01")) %>% 
  ggplot(aes(TIME_PERIOD, values)) +
  geom_line(size = 1.1, color = "darkorange3") +
  labs(
    title = "Portugal – Taux d'emplois vacants (NSA)",
    subtitle = "En %, ensemble des secteurs (A-S), données non désaisonnalisées",
    x = "Date",
    y = "%",
    caption = "Source : Eurostat (jvs_q_nace2)"
  ) +
  theme_minimal(base_size = 13)

#######################
###### Inflation ######
#######################

ggplot(df_infl, aes(TIME_PERIOD, values, color = type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Portugal – Inflation totale et sous-jacente",
    subtitle = "Taux annuels (%)",
    x = "Date",
    y = "%",
    color = "",
    caption = "Source : Eurostat (prc_hicp_manr)"
  ) +
  theme_minimal(base_size = 13)

#######################
#### Commerce ext #####
#######################

ggplot(export_import, aes(x = TIME_PERIOD, y = values, color = item)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ type, scales = "free_y") +
  labs(
    title = "Portugal — Exportations et importations de biens & services",
    x = "Période",
    y = "Millions d'euros",
    color = "",
    caption = "Source : Eurostat (ext_st_27_2020msbec et bop_c6_m)"
  ) +
  theme_minimal()

ggplot(solde_total, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = solde_total), linewidth = 1.3, color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Portugal — Solde commercial (biens + services)",
    y = "Millions d'euros",
    x = "",
    caption = "Source : Eurostat (ext_st_27_2020msbec et bop_c6_m)"
  ) +
  theme_minimal()

tce_pt %>% 
  filter(quarter >= as.Date("2024-01-01")) %>% 
  ggplot(aes(x = quarter, y = values)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Portugal — Taux de change effectif nominal (NEER)",
    x = "Trimestre",
    y = "Index (2015 = 100)",
    caption = "Source : Eurostat (ert_eff_ic_q)"
  ) +
  theme_minimal()

#######################
#  Finances publiques #
#######################

ggplot(deficit_pt, aes(x = TIME_PERIOD, y = values)) +
  geom_line(size = 1) +
  geom_hline(yintercept = -3, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Portugal – Déficit public (% du PIB)",
    y = "% PIB", x = "",
    caption = "Source : Eurostat (gov_10dd_edpt1)"
  )

ggplot(dette, aes(x = TIME_PERIOD, y = values)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Portugal – Dette publique (% du PIB)",
    y = "% PIB", x = "",
    caption = "Source : Eurostat (gov_10dd_edpt1)"
  )

ggplot(taux_10y_pt, aes(x = TIME_PERIOD, y = values)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Portugal – Taux à 10 ans (obligations d'État)",
    x = "", y = "%",
    caption = "Source : Eurostat (irt_lt_mcby_m)"
  )


###########################################
# Utilisation des capacités de production #
###########################################

ggplot(cap_24_25, aes(x = TIME_PERIOD, y = values)) +
  geom_line(col = "steelblue", size = 1) +
  labs(
    title = "Taux d'utilisation des capacités — Portugal",
    subtitle = "2024–2025",
    x = "Date (trimestre)",
    y = "Capacité utilisée (%)",
    caption = "Source : Eurostat (ei_bsin_q_r2)"
  ) +
  theme_minimal()
