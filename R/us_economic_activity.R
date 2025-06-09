#---- Economic Activity
library(scales)

# Códigos e nomes dos componentes
series <- c(GDPC1 = "PIB",
            PCECC96 = "Consumo",
            GPDIC1 = "Investimento",
            NETEXC = "Exportações líquidas",
            GCEC1 = "Governo")

# Coletar os dados
pib_componentes <- purrr::imap_dfr(series, ~ get_fred_data(.y, start="2010-01-01") %>% mutate(componente = .x))

# Formatar para análise
pib_wide <- pib_componentes %>%
  select(date, componente, value) %>%
  pivot_wider(names_from = componente, values_from = value) %>%
  mutate(`PIB estimado` = Consumo + Investimento + `Exportações líquidas` + Governo)

# Long para gráfico empilhado
pib_long <- pib_wide %>%
  pivot_longer(cols = c("Consumo", "Investimento", "Exportações líquidas", "Governo"),
               names_to = "componente", values_to = "value")

# Gráfico final
ggplot(pib_long, aes(x = date, y = value, fill = componente)) +
  geom_area() +
  geom_line(data = pib_wide,
            aes(x = date, y = `PIB estimado`),
            color = "black", size = 1, inherit.aes = FALSE) +
  scale_fill_manual(values = c(
    "Consumo"               = "#166083",
    "Investimento"          = "#37A6D9",
    "Exportações líquidas"  = "#AFABAB",
    "Governo"               = "#82C1DB"
  )) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years",
               expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(labels = label_comma(scale = 1)) +
  labs(title    = "EUA: Componentes do PIB (Real, SAAR)",
       subtitle = paste("Última observação:", format(max(pib_wide$date), "%b %Y")),
       y        = "US$ bilhões (base 2012)",
       x        = NULL,
       caption  = "Fonte: FRED / Impactus UFRJ",
       fill     = NULL) +
  theme_pandora()

#---- QoQ

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# Cálculo da variação QoQ (%)
pib_qoq_all <- pib_componentes %>%
  arrange(componente, date) %>%
  group_by(componente) %>%
  mutate(valor_qoq = 100 * (value / lag(value) - 1)) %>%
  ungroup() %>%
  filter(!is.na(valor_qoq), date >= as.Date("2023-01-01"))  # ajustável

# Separar PIB (linha) e componentes (colunas)
pib_qoq <- pib_qoq_all %>% filter(componente == "PIB")
df_qoq  <- pib_qoq_all %>% filter(componente != "PIB")

# Ordenar os componentes (opcional)
df_qoq <- df_qoq %>%
  mutate(componente = factor(componente,
                             levels = c("Consumo", "Investimento", "Governo", "Exportações líquidas")))


# Remover Exportações líquidas
df_qoq <- pib_qoq_all %>%
  filter(componente != "PIB", componente != "Exportações líquidas") %>%
  mutate(componente = factor(componente,
                             levels = c("Consumo", "Investimento", "Governo")))

# Gráfico final
ggplot(df_qoq, aes(x = date, y = valor_qoq, fill = componente)) +
  geom_col(position = position_dodge(width = 60), width = 60) +
  geom_line(data = pib_qoq, aes(x = date, y = valor_qoq, color = "PIB"),
            size = 1.2, inherit.aes = FALSE) +
  geom_point(data = pib_qoq, aes(x = date, y = valor_qoq, color = "PIB"),
             size = 2.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c(
    "Consumo"               = "#A5480D",
    "Investimento"          = "#D9773C",
    "Governo"               = "#F58808",
    "Exportações líquidas" = "#600100"
  )) +
  scale_color_manual(values = c("PIB" = "black")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               expand = expansion(mult = c(0.01, 0.1))) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "Estados Unidos: PIB e Componentes (QoQ %)",
       subtitle = paste("Última observação:", format(max(pib_qoq$date), "%b %Y")),
       x        = NULL,
       y        = NULL,
       caption  = "Fonte: FRED / Impactus UFRJ",
       fill     = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0),
    plot.subtitle    = element_text(hjust = 0),
    plot.caption     = element_text(size = 9, color = "gray40", hjust = 0),
    legend.position  = "top",
  ) +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_pandora()







