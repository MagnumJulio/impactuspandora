library(purrr)
library(broom)

#---- CONFIGURAÇÃO ----#
regiao       <- "EA"             # Código do Eurostat
label_regiao <- "Zona do Euro"       # Nome exibido nos gráficos


#---- HICP ----#
hicp_df <- get_macro_data_eurostat(
  id = "prc_hicp_midx",
  filter_expr = paste0("geo == '", regiao, "' & coicop %in% c('CP00', 'FOOD', 'NRG', 'TOT_X_NRG_FOOD') & unit == 'I15'"),
  time_format = "raw",
  label = "label"
)

hicp_df <- hicp_df |>
  dplyr::mutate(coicop = dplyr::recode(coicop,
                                       "CP00" = "Geral",
                                       "FOOD" = "Alimentos, álcool e tabaco",
                                       "NRG" = "Energia",
                                       "TOT_X_NRG_FOOD" = "Núcleo"
  ))

hicp_filtered <- hicp_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2017-01-01"))

hicp_palette <- c(
  "Geral"     = "gray",
  "Alimentos, álcool e tabaco" = "#082631",
  "Energia"   = "#37A6D9",
  "Núcleo"    = "#166083"
)

#---- Econometria ----#
hicp_yoy <- hicp_filtered %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(coicop) %>%
  arrange(date) %>%
  mutate(var_yoy = (values / lag(values, 12) - 1) * 100) %>%
  filter(!is.na(var_yoy)) %>%
  ungroup()

hicp_wide <- hicp_yoy %>%
  select(date, year, coicop, var_yoy) %>%
  pivot_wider(names_from = coicop, values_from = var_yoy) %>%
  filter(!is.na(`Geral`), !is.na(`Alimentos, álcool e tabaco`), !is.na(Energia), !is.na(Núcleo))

reg_por_ano <- function(df) {
  lm(Geral ~ `Alimentos, álcool e tabaco` + Energia + Núcleo, data = df)
}

pesos_estimados <- hicp_wide %>%
  group_by(year) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$year))) %>%
  map_dfr(~ tidy(reg_por_ano(.x)), .id = "year") %>%
  filter(term != "(Intercept)") %>%
  select(year, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

dados_24_25 <- hicp_wide %>%
  filter(year %in% c("2024", "2025"))

modelo_24_25 <- reg_por_ano(dados_24_25)

pesos_2025 <- tidy(modelo_24_25) %>%
  filter(term != "(Intercept)") %>%
  mutate(year = "2025") %>%
  select(year, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

pesos_final <- pesos_estimados %>%
  filter(year != "2025") %>%
  bind_rows(pesos_2025)

pesos_long <- pesos_final %>%
  pivot_longer(-year, names_to = "coicop", values_to = "peso") %>%
  mutate(coicop = str_remove_all(coicop, "`"))

hicp_contrib <- hicp_yoy %>%
  filter(coicop != "Geral") %>%
  left_join(pesos_long, by = c("year", "coicop")) %>%
  mutate(contrib = peso * var_yoy)

hicp_geral <- hicp_yoy %>%
  filter(coicop == "Geral") %>%
  select(date, geral_yoy = var_yoy)


#---- GRÁFICO: DECOMPOSIÇÃO YoY ----#
ggplot(hicp_contrib, aes(x = date, y = contrib, fill = coicop)) +
  geom_col(position = "stack", width = 25) +
  geom_line(data = hicp_geral,
            aes(x = date, y = geral_yoy),
            size = 1, color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = hicp_palette) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  labs(title    = paste("Inflação YoY -", label_regiao, "(HICP)"),
       subtitle = paste("Contribuições por componente. Última observação:", format(max(hicp_contrib$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       caption  = "Fonte: Eurostat/Impactus UFRJ",
       fill     = NULL) +
  theme_pandora()

#---- GRÁFICO: MM3M SAAR ----#
library(zoo)

hicp_geral_sa <- hicp_filtered %>%
  filter(coicop == "Geral") %>%
  arrange(date) %>%
  mutate(value_log = log(values))

ts_geral <- ts(hicp_geral_sa$value_log,
               start = c(year(min(hicp_geral_sa$date)), month(min(hicp_geral_sa$date))),
               frequency = 12)

stl_decomp <- stl(ts_geral, s.window = "periodic")

log_sa <- stl_decomp$time.series[, "trend"] + stl_decomp$time.series[, "remainder"]

hicp_geral_sa <- hicp_geral_sa %>%
  mutate(
    log_sa        = as.numeric(log_sa),
    var_log_mom   = log_sa - lag(log_sa),
    mm3m_saar     = rollmean(var_log_mom, 3, align = "right", fill = NA) * 12,
    mm3m_saar_pct = 100 * (exp(mm3m_saar) - 1)
  )

ggplot(hicp_geral_sa, aes(x = date, y = mm3m_saar_pct)) +
  geom_line(size = 1.2, color = "#082631") +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = hicp_geral_sa %>% slice_tail(n = 1),
            aes(label = sprintf("%.1f%%", mm3m_saar_pct)),
            hjust = -0.2, show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title    = paste(label_regiao, ": HICP Geral - MM3M SAAR"),
       subtitle = paste("Taxa anualizada ajustada sazonalmente. Última observação:", format(max(hicp_geral_sa$date), "%b %Y")),
       y        = "Taxa anualizada (%)",
       x        = NULL,
       caption  = "Fonte: Eurostat/Impactus UFRJ") +
  theme_pandora()


#--- PPI

# Configuração
regiao       <- "DE"
label_regiao <- "Alemanha"

# B-E35
# B_c_x_MIG_MRG
# MIG_ING
# MIG_NRG
# MIG_CAG
# MIG_DCOG
# MIG_NDCOG


# Coleta do PPI total para o mercado interno (domestic market)
ppi_df <- get_macro_data_eurostat(
  id = "sts_inppd_m",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "nace_r2 == 'B-E36' & ",
    "s_adj == 'NSA' & ",
    "unit == 'PCH_SM'"
  ),
  time_format = "raw"
)

# Recodificação da categoria para legenda
ppi_df <- ppi_df %>%
  dplyr::mutate(nace_r2 = dplyr::recode(nace_r2,
                                        "B-E36" = "PPI Total - Mercado Doméstico"))

# Conversão da data e filtro de período
ppi_df <- ppi_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2018-01-01"))

# Paleta de cores personalizada
ppi_palette <- c(
  "PPI Total - Mercado Doméstico" = "#37A6D9"
)

# Gráfico
ggplot(ppi_df, aes(x = date, y = values, color = nace_r2)) +
  geom_line(size = 1.2) +
  geom_text(
    data = ppi_df %>% group_by(nace_r2) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", values)),
    hjust = -0.1, vjust = 0.5, show.legend = FALSE, size = 3.5
  ) +
  scale_color_manual(values = ppi_palette) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  labs(title    = paste(label_regiao, ": Índice de Preços ao Produtor (PPI), AS - Variação Anual"),
       subtitle = paste("Última observação:", format(max(ppi_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Eurostat / Impactus UFRJ") +
  theme_pandora() +
  theme(legend.position = "none")





