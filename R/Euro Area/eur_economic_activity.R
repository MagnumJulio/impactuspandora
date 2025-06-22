#---- Economic Activity



#---- PRODUÇÃO INDUSTRIAL

# Configuração
regiao       <- "DE"
label_regiao <- "Alemanha"

# I21 : Índice 21
# PCH_SM : a/a

# Coleta da produção industrial total e por categorias
industrial_df <- get_macro_data_eurostat(
  id = "sts_inpr_m",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "nace_r2 %in% c('B-D') & ",
    "s_adj == 'CA' & unit == 'PCH_SM'"
  ),
  time_format = "raw"
)

# Recodificação das categorias para legenda
industrial_df <- industrial_df %>%
  dplyr::mutate(nace_r2 = dplyr::recode(nace_r2,
                                        "B-D" = "Total indústria",
  ))

# Conversão da data e filtro de período
industrial_df <- industrial_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2018-01-01"))

# Paleta de cores personalizada
industrial_palette <- c(
  "Total indústria"             = "#166083"
)


ggplot(industrial_df, aes(x = date, y = values, color = nace_r2)) +
  geom_line(size = 1.2) +
  geom_text(
    data = industrial_df %>% group_by(nace_r2) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", values)),
    hjust = -0.1, vjust = 0.5, show.legend = FALSE, size = 3.5
  ) +
  scale_color_manual(values = industrial_palette) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  labs(title    = paste(label_regiao,": Produção Industrial, AS - Variação Anual"),
       subtitle = paste("Última observação:", format(max(industrial_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Eurostat / Impactus UFRJ") +
  theme_pandora() +
  theme(legend.position = "none")


#-----
# PIB

# Configuração
regiao       <- "EA20"
label_regiao <- "Zona do Euro"
# CLV_PCH_PRE : t/t
# CLV_PCH_SM : a/a
# CLV_PCH_ANN : t/t anualizado

# Coleta do PIB real (índice volume encadeado, base 2021 = 100)
gdp_df <- get_macro_data_eurostat(
  id = "namq_10_gdp",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "na_item == 'B1GQ' & ",
    "s_adj == 'SCA' & ",
    "unit == 'CLV_PCH_PRE'"
  ),
  time_format = "raw"
)

# Recodificação para legenda
gdp_df <- gdp_df %>%
  dplyr::mutate(na_item = dplyr::recode(na_item, "B1GQ" = "PIB real"))

# Conversão de "YYYY-QX" para data no primeiro mês de cada trimestre
gdp_df <- gdp_df %>%
  mutate(
    date = as.character(date),
    date = case_when(
      grepl("^\\d{4}-Q1$", date) ~ paste0(substr(date, 1, 4), "-01-01"),
      grepl("^\\d{4}-Q2$", date) ~ paste0(substr(date, 1, 4), "-04-01"),
      grepl("^\\d{4}-Q3$", date) ~ paste0(substr(date, 1, 4), "-07-01"),
      grepl("^\\d{4}-Q4$", date) ~ paste0(substr(date, 1, 4), "-10-01"),
      TRUE ~ NA_character_
    ),
    date = as.Date(date)
  ) %>%
  filter(!is.na(date) & date >= as.Date("2018-01-01"))




# Paleta de cores personalizada
gdp_palette <- c("PIB real" = "#166083")

# Gráfico
ggplot(gdp_df, aes(x = date, y = values, color = na_item)) +
  geom_line(size = 1.2) +
  geom_text(
    data = gdp_df %>% group_by(na_item) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", values)),
    hjust = -0.1, vjust = 0.5, show.legend = FALSE, size = 3.5
  ) +
  scale_color_manual(values = gdp_palette) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  labs(title    = paste(label_regiao,": PIB Real, AS - Variação Trimestral"),
       subtitle = paste("Última observação:", format(max(gdp_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Eurostat / Impactus UFRJ") +
  theme_pandora() +
  theme(legend.position = "none")


#-----
# Vendas no Varejo

regiao       <- "EA20"
label_regiao <- "Zona do Euro"

# Coleta das vendas no varejo - total
varejo_df <- get_macro_data_eurostat(
  id = "sts_trtu_m",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "nace_r2 == 'G47' & ",
    "s_adj == 'SCA' & unit == 'PCH_PRE'"
  ),
  time_format = "raw"
)

# Recodificação para legenda
varejo_df <- varejo_df %>%
  dplyr::mutate(nace_r2 = dplyr::recode(nace_r2,
                                        "G47" = "Total varejo"))

# Conversão da data e filtro de período
varejo_df <- varejo_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2018-01-01"))

# Paleta de cores personalizada
varejo_palette <- c("Total varejo" = "#37A6D9")

# Gráfico
ggplot(varejo_df, aes(x = date, y = values, color = nace_r2)) +
  geom_line(size = 1.2) +
  geom_text(
    data = varejo_df %>% group_by(nace_r2) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", values)),
    hjust = -0.1, vjust = 0.5, show.legend = FALSE, size = 3.5
  ) +
  scale_color_manual(values = varejo_palette) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  labs(title    = paste(label_regiao, ": Vendas no Varejo, AS - Variação Mensal"),
       subtitle = paste("Última observação:", format(max(varejo_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Eurostat / Impactus UFRJ") +
  theme_pandora() +
  theme(legend.position = "none")







