library(ggplot2)
library(dplyr)

# 1. Dados dos retornos e coordenadas (com nomes únicos para evitar conflitos)
retornos_etfs <- tibble::tribble(
  ~region,        ~ticker, ~retorno, ~x,     ~y,
  "USA",           "SPY",  0.024,    -98.5,  39.8,
  "Japan",         "EWJ",  0.1016,    138.2,  36.2,
  "Germany",       "EWG",  0.3464,     10.4,  51.0,
  "Brazil",        "EWZ",  0.22,    -51.9, -10.0,
  "China",         "EEM",  0.1236,    104.2,  35.9,
  "Taiwan",        "EEM",  0.1236,    121.0,  23.5,
  "India",         "EEM",  0.1236,     78.9,  21.0,
  "South Korea",   "EEM",  0.1236,    127.8,  36.5,
  "Saudi Arabia",  "EEM",  0.1236,     45.0,  24.0,
  "South Africa",  "EEM",  0.1236,     22.9, -30.6,
  "Mexico",        "EEM",  0.1236,   -102.6,  23.6,
  "United Arab Emirates", "EEM", 0.1236,  54.0, 24.4,
  "Malaysia",      "EEM",  0.1236,    102.0,   4.2,
  "Indonesia",     "EEM",  0.1236,    113.9,  -0.8,
  "Poland",        "EEM",  0.1236,     19.1,  52.1
)


# 2. Mapa mundial
world_map <- map_data("world")

# 3. Join para coloração
mapa_dados <- world_map %>%
  left_join(retornos_etfs, by = "region")

# 4. Gráfico
ggplot() +
  geom_polygon(data = mapa_dados,
               aes(x = long, y = lat, group = group, fill = retorno),
               color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "#853d0f",
    mid = "#d76f2e",
    high = "#ffba8f",
    midpoint = 0.08,
    na.value = "gray95",
    name = "Retorno",
    labels = scales::percent_format(accuracy = 1)
  ) +
  coord_fixed(1.3) +
  labs(
    title    = "Retornos de ETFs por Região",
    subtitle = "Exemplo: SPY, EWJ, EWG, EEM, EWZ",
    caption  = "Fonte: Simulada / Impactus UFRJ"
  ) +
  theme_void(base_size = 14) +
  theme(legend.position = "right")
