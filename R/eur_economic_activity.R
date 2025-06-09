#---- Economic Activity



#---- PRODUÇÃO INDUSTRIAL

# Configuração
regiao       <- "DE"
label_regiao <- "Alemanha"

# Coleta da produção industrial total e por categorias
industrial_df <- get_macro_data_eurostat(
  id = "sts_inpr_m",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "nace_r2 %in% c('B-D') & ",
    "s_adj == 'SCA' & unit == 'I21'"
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
  "Total indústria"             = "#D9773C"
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
  labs(title    = paste("Alemanha: Produção Industrial (índice ajustado, base 2021 = 100)"),
       subtitle = paste("Última observação:", format(max(industrial_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Eurostat / Impactus UFRJ") +
  theme_pandora() +
  theme(legend.position = "none")
