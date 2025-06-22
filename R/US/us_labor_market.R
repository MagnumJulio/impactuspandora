#--- Labor market

# Puxar as duas séries do FRED
desempregados <- get_fred_data("LNU03000000", start = "2021-01-01")
vagas_abertas <- get_fred_data("JTS1000JOL", start = "2021-01-01")

# Juntar e calcular a razão
unemp_per_job <- desempregados %>%
  inner_join(vagas_abertas, by = "date", suffix = c("_desemp", "_vagas")) %>%
  mutate(ratio = value_desemp / value_vagas)

# Gráfico

ggplot(unemp_per_job, aes(x = date, y = ratio)) +
  geom_line(color = "#cd6726", size = 1.2) +
  geom_text(data = unemp_per_job %>% dplyr::slice_tail(n = 1),
            aes(label = sprintf("%.2f", ratio)),
            hjust = -0.2, show.legend = FALSE,
            color = "#cd6726") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  labs(title    = "EUA: Pessoas Desempregadas por Vaga de Emprego",
       subtitle = paste("Última observação:", format(max(unemp_per_job$date), "%b %Y")),
       y        = NULL,
       x        = NULL,
       caption  = "Fonte: FRED / Impactus UFRJ") +
  theme_pandora()







