#---- Euro Labor market

# Negotiated wages: STS.Q.I9.N.INWR.000000.3.ANR
filter <- list(lastNObservations = 12, detail = "full")
wages_df <- get_macro_data_ecb("STS.Q.I9.N.INWR.000000.3.ANR", parse_date = "ym")

# Formatar
wages_df <- wages_df %>%
  select(date, value) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(date = as.Date(date))


ggplot(wages_df, aes(x = date, y = value)) +
  geom_line(color = "#37A6D9", linewidth = 1.2) +
  geom_point(data = wages_df %>% slice_tail(n = 1),
             aes(x = date, y = value),
             color = "#37A6D9", size = 2) +
  geom_text(data = wages_df %>% slice_tail(n = 1),
            aes(label = sprintf("%.1f%%", value)),
            hjust = -0.1, vjust = -0.5,
            color = "#37A6D9", size = 4,
            show.legend = FALSE) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "12 months",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title    = "Área do Euro: Salários Negociados",
       subtitle = paste("Última observação:", format(max(wages_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       caption  = "Fonte: ECB / Impactus UFRJ") +
  theme_pandora()


#---- Labor productivity

# Labor productivity: MNA.Q.Y.I9.W0.S1.S1._Z.LPR_PS._Z._T._Z.IX.LR.N
filter <- list(lastNObservations = 12, detail = "full")
productivity_df <- get_macro_data_ecb("MNA.Q.Y.I9.W0.S1.S1._Z.LPR_PS._Z._T._Z.IX.LR.N", parse_date = "ym")

# Formatar
productivity_df <- productivity_df %>%
  select(date, value) %>%
  filter(date >= as.Date("2016-01-01")) %>%
  mutate(date = as.Date(date))



ggplot(productivity_df, aes(x = date, y = value)) +
  geom_line(color = "#37A6D9", linewidth = 1.2) +
  geom_point(data = productivity_df %>% slice_tail(n = 1),
             aes(x = date, y = value),
             color = "#37A6D9", size = 2) +
  geom_text(data = productivity_df %>% slice_tail(n = 1),
            aes(label = sprintf("%.1f", value)),
            hjust = -0.1, vjust = -0.5,
            color = "#37A6D9", size = 4,
            show.legend = FALSE) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "12 months",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_number()) +  # Aqui a mudança
  labs(title    = "Área do Euro: Índice de Produtividade por Empregados (2010=100)",
       subtitle = paste("Última observação:", format(max(productivity_df$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       caption  = "Fonte: ECB / Impactus UFRJ") +
  theme_pandora()


#-------------
# Desemprego

# Configuração
regiao       <- "EA20"
label_regiao <- "Zona do Euro"

# Coleta da taxa de desemprego total (ajustada sazonalmente)
desemprego_df <- get_macro_data_eurostat(
  id = "une_rt_m",
  filter_expr = paste0(
    "geo == '", regiao, "' & ",
    "sex == 'T' & age == 'TOTAL' & ",
    "s_adj == 'SA' & unit == 'PC_ACT'"
  ),
  time_format = "raw"
)

# Recodificação para legenda
desemprego_df <- desemprego_df %>%
  dplyr::mutate(indicador = "Taxa de desemprego")

# Conversão da data e filtro de período
desemprego_df <- desemprego_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2018-01-01"))

# Paleta de cores personalizada
desemprego_palette <- c("Taxa de desemprego" = "#166083")

# Gráfico
ggplot(desemprego_df, aes(x = date, y = values, color = indicador)) +
  geom_line(size = 1.2) +
  geom_text(
    data = desemprego_df %>% group_by(indicador) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f%%", values)),
    hjust = -0.1, vjust = 0.5, show.legend = FALSE, size = 3.5
  ) +
  scale_color_manual(values = desemprego_palette) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = paste(label_regiao, ": Taxa de Desemprego (ajustada sazonalmente)"),
    subtitle = paste("Última observação:", format(max(desemprego_df$date), "%b %Y")),
    y        = NULL,
    x        = NULL,
    color    = NULL,
    caption  = "Fonte: Eurostat / Impactus UFRJ"
  ) +
  theme_pandora() +
  theme(legend.position = "none")






