library(dplyr)
library(tidyr)
library(ggplot2)

# Tabela em formato wide
df_wide <- tibble::tribble(
  ~Data, ~`2023 - Outono`, ~`2024 - Outono`, ~`2025 - Primavera`,
  2024,         1.2,              0.8,              0.9,
  2025,         1.6,              1.3,              0.9,
  2026,         NA,               1.6,              1.4
)


df_long <- df_wide %>%
  pivot_longer(-Data, names_to = "projecao", values_to = "valor")


ggplot(df_long, aes(x = Data, y = valor, color = projecao)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_text(data = df_long %>% group_by(projecao) %>% slice_max(order_by = Data, n = 1),
            aes(label = sprintf("%.1f", valor)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = c(
    "2023 - Outono"  = "#A5480D",
    "2024 - Outono"  = "#D9773C",
    "2025 - Primavera"  = "#600100"
  )) +
  scale_x_continuous(breaks = 2024:2026, expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title    = "Projeções de Crescimento - PIB",
    subtitle = "Fonte: Projeções sazonais por ano",
    x        = NULL,
    y        = "%",
    color    = NULL,
    caption  = "Fonte: Simulada / Impactus UFRJ"
  ) +
  theme_minimal(base_size = 14) +
  theme_pandora()
