#----- FEAR

vix_df <- get_fred_data("VIXCLS", start = "2018-01-01")

library(ggplot2)

ggplot(vix_df, aes(x = date, y = value)) +
  # Faixa da crise de 2020
  annotate("rect", xmin = as.Date("2020-02-15"), xmax = as.Date("2020-05-15"),
           ymin = -Inf, ymax = Inf, fill = "gray70", alpha = 0.3) +
  geom_line(color = "#cd6726", size = 1.2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 0.4) +
  geom_text(data = vix_df %>% dplyr::slice_tail(n = 1),
            aes(label = sprintf("%.1f", value)), hjust = -0.2,
            color = "#cd6726", show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(title    = "VIX - Volatilidade Implícita (S&P 500)",
       subtitle = paste("Última observação:", format(max(vix_df$date), "%d/%m/%Y")),
       y        = NULL,
       x        = NULL,
       caption  = "Fonte: FRED / Impactus UFRJ") +
  theme_pandora()

# --- Períodos acima de 30

library(dplyr)
library(lubridate)

vix_df <- vix_df %>%
  mutate(acima_30 = value > 30)

# Cria grupos contínuos mantendo toda a série
vix_df <- vix_df %>%
  mutate(
    grupo = with(rle(acima_30), rep(seq_along(values), lengths))
  )

# Filtra apenas os grupos onde acima_30 é TRUE
vix_periodos <- vix_df %>%
  group_by(grupo) %>%
  filter(first(acima_30)) %>%  # garante que o grupo é de VIX > 30
  summarise(
    inicio = min(date),
    fim = max(date),
    duracao = as.integer(fim - inicio + 1),
    max_vix = max(value),
    .groups = "drop"
  ) %>%
  arrange(inicio)

print(vix_periodos)
