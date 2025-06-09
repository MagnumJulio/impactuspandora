#---- Euro Labor market

# Negotiated wages: STS.Q.I9.N.INWR.000000.3.ANR
filter <- list(lastNObservations = 12, detail = "full")
wages_df <- get_macro_data_ecb("STS.Q.I9.N.INWR.000000.3.ANR")

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
productivity_df <- get_macro_data_ecb("MNA.Q.Y.I9.W0.S1.S1._Z.LPR_PS._Z._T._Z.IX.LR.N")

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


# ------



#---- CONFIGURAÇÃO ----#
regiao       <- "EA"             # Código do Eurostat
label_regiao <- "Zona do Euro"       # Nome exibido nos gráficos


#---- unemp ----#
unemp_df <- get_macro_data_eurostat(
  id = "une_rt_m",
  filter_expr = paste0("geo == '", regiao, "' & unit == 'pc_at' & s_adj == 'SA"),
  time_format = "raw",
  label = "label"
)

unemp_df <- unemp_df |>
  dplyr::mutate(coicop = dplyr::recode(coicop,
                                       "CP00" = "Geral",
                                       "FOOD" = "Alimentos, álcool e tabaco",
                                       "NRG" = "Energia",
                                       "TOT_X_NRG_FOOD" = "Núcleo"
  ))

unemp_filtered <- unemp_df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2017-01-01"))
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





