# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(lubridate)
#df <- ecb::get_data("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3Y")
#df2 <- ecb::get_data("STS.Q.I9.N.INWR.000000.3.ANR")

# --- Coleta da taxa de juros de 3 anos da Zona do Euro (ECB)
euro_yield <- get_macro_data_ecb("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3Y", parse_date = "none") %>%
  select(date, value) %>%
  rename(euro_area = value) %>%
  mutate(date = as.Date(date))


# --- Coleta da taxa de juros de 3 anos dos EUA (FRED)
us_yield <- get_fred_data("DGS3", start = "2010-01-01") %>%
  rename(us = value) %>%
  mutate(date = as.Date(date))

# --- Juntar as bases por data
diff_yield_df <- euro_yield %>%
  inner_join(us_yield, by = "date") %>%
  mutate(spread = euro_area - us) %>%
  tidyr::drop_na() %>%
  filter(date >= as.Date("2021-01-01"))

# --- Cambio no ECB data portal

# Taxa de câmbio EUR/USD — série diária do ECB
eurusd_df <- get_macro_data_ecb("EXR.D.USD.EUR.SP00.A", parse_date = "none") %>%
  select(date, value) %>%
  rename(eurusd = value) %>%
  mutate(date = as.Date(date))

# Juntar com a série de câmbio
joint_df <- diff_yield_df %>%
  inner_join(eurusd_df %>% mutate(date = as.Date(date)), by = "date") %>%
  tidyr::drop_na()

# Normalização da escala do câmbio para o segundo eixo
# Vamos deslocar e reescalar o EUR/USD para caber junto com o spread
# Exemplo: trazer EUR/USD (~1.05–1.25) para algo como 0.5–2.5, sem distorcer a forma
scale_factor <- 4
joint_df <- joint_df %>%
  mutate(eurusd_scaled = (eurusd - mean(eurusd, na.rm = TRUE)) * scale_factor + mean(spread, na.rm = TRUE))


joint_df_plot <- joint_df %>%
  tidyr::pivot_longer(cols = c(spread, eurusd_scaled),
                      names_to = "serie",
                      values_to = "valor")
# Gráfico com dois eixos Y
# Paleta e rótulos
paleta <- c("spread" = "#082631", "eurusd_scaled" = "#37A6D9")
rotulos <- c("spread" = "Diferencial de Juros (p.p.)", "eurusd_scaled" = "EUR/USD (escala ajustada)")

# Gráfico
# Calcular último valor de cada série
last_labels <- joint_df %>%
  slice_max(order_by = date, n = 1) %>%
  transmute(
    date,
    spread,
    eurusd_scaled,
    label_spread = sprintf("%.2f", spread),
    label_eurusd = sprintf("%.3f", eurusd)  # valor real, não escalado
  )

# Calcular último valor de cada série
last_labels <- joint_df %>%
  slice_max(order_by = date, n = 1) %>%
  transmute(
    date,
    spread,
    eurusd_scaled,
    label_spread = sprintf("%.2f", spread),
    label_eurusd = sprintf("%.3f", eurusd)  # valor real, não escalado
  )


#--- Grafico sombreado

#----
intervalos_vix <- tribble(
  ~inicio,       ~fim,
  as.Date("2018-02-05"), as.Date("2018-02-05"),
  as.Date("2018-02-08"), as.Date("2018-02-08"),
  as.Date("2018-12-21"), as.Date("2018-12-24"),
  as.Date("2018-12-26"), as.Date("2018-12-26"),
  as.Date("2020-02-27"), as.Date("2020-04-09"),
  as.Date("2020-04-13"), as.Date("2020-05-07"),
  as.Date("2020-05-12"), as.Date("2020-05-15"),
  as.Date("2020-05-19"), as.Date("2020-05-19"),
  as.Date("2020-06-11"), as.Date("2020-06-30"),
  as.Date("2020-07-13"), as.Date("2020-07-13"),
  as.Date("2020-09-03"), as.Date("2020-09-04"),
  as.Date("2020-09-08"), as.Date("2020-09-08"),
  as.Date("2020-10-26"), as.Date("2020-11-03"),
  as.Date("2021-01-27"), as.Date("2021-02-01"),
  as.Date("2021-12-01"), as.Date("2021-12-01"),
  as.Date("2021-12-03"), as.Date("2021-12-03"),
  as.Date("2022-01-25"), as.Date("2022-01-27"),
  as.Date("2022-02-23"), as.Date("2022-02-24"),
  as.Date("2022-02-28"), as.Date("2022-03-14"),
  as.Date("2022-04-26"), as.Date("2022-04-27"),
  as.Date("2022-04-29"), as.Date("2022-05-02"),
  as.Date("2022-05-05"), as.Date("2022-05-12"),
  as.Date("2022-05-18"), as.Date("2022-05-18"),
  as.Date("2022-06-13"), as.Date("2022-06-14"),
  as.Date("2022-06-16"), as.Date("2022-06-21"),
  as.Date("2022-09-26"), as.Date("2022-10-03"),
  as.Date("2022-10-06"), as.Date("2022-10-19"),
  as.Date("2024-08-05"), as.Date("2024-08-05"),
  as.Date("2025-04-03"), as.Date("2025-04-16"),
  as.Date("2025-04-21"), as.Date("2025-04-22")
)

#----
# Filtra os intervalos de VIX para se restringirem às datas da base principal
intervalos_vix_plot <- intervalos_vix %>%
  filter(
    fim   >= min(joint_df$date),
    inicio <= max(joint_df$date)
  )

# Plot otimizado
# Filtrar intervalos de VIX para ficarem dentro do intervalo da série
intervalos_vix_plot <- intervalos_vix %>%
  filter(
    fim    >= min(joint_df$date),
    inicio <= max(joint_df$date)
  )

# Gráfico final com sombreamento correto e cores respeitadas
ggplot(joint_df, aes(x = date)) +
  # Fundo cinza nos períodos de estresse (limitado ao intervalo da série)
  geom_rect(data = intervalos_vix_plot,
            aes(xmin = inicio, xmax = fim),
            ymin = -Inf, ymax = Inf,
            fill = "grey80", alpha = 0.4, inherit.aes = FALSE) +

  # Linhas principais
  geom_line(aes(y = spread,       color = "spread"), size = 1.2) +
  geom_line(aes(y = eurusd_scaled, color = "eurusd"), size = 1.2) +

  # Labels finais
  geom_text(data = last_labels,
            aes(x = date + 30, y = spread, label = label_spread, color = "spread"),
            hjust = 0, size = 3.2, show.legend = FALSE) +
  geom_text(data = last_labels,
            aes(x = date + 30, y = eurusd_scaled, label = label_eurusd, color = "eurusd"),
            hjust = 0, size = 3.2, show.legend = FALSE) +

  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(
      trans = ~ (. - mean(joint_df$spread)) / scale_factor + mean(joint_df$eurusd),
      name = NULL
    )
  ) +
  scale_color_manual(
    values = c("spread" = "#082631", "eurusd" = "#37A6D9"),
    labels = c("spread" = "Diferencial de Juros (esq.)", "eurusd" = "EUR/USD (dir.)")
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Diferencial de Juros (3 anos) e Câmbio EUR/USD",
    subtitle = paste("Última observação:", format(max(joint_df$date), "%d/%m/%Y")),
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Fontes: ECB, FRED / Impactus UFRJ"
  ) +
  theme_pandora()




#----

#ggplot(joint_df, aes(x = date)) +
#  geom_line(aes(y = spread, color = "spread"), size = 1.2) +
#  geom_line(aes(y = eurusd_scaled, color = "eurusd"), size = 1.2) +

  # Labels finais — manualmente
#  geom_text(data = last_labels,
#            aes(x = date + 30, y = spread, label = label_spread, color = "spread"),
#            hjust = 0, size = 3.2, show.legend = FALSE) +
#  geom_text(data = last_labels,
#            aes(x = date + 30, y = eurusd_scaled, label = label_eurusd, color = "eurusd"),
#            hjust = 0, size = 3.2, show.legend = FALSE) +

#  scale_y_continuous(
#    name = NULL,
#    sec.axis = sec_axis(
#      trans = ~ (. - mean(joint_df$spread)) / scale_factor + mean(joint_df$eurusd),
#      name = NULL
#    )
#  ) +
#  scale_color_manual(
#    values = c("spread" = "#082631", "eurusd" = "#37A6D9"),
#    labels = c("spread" = "Diferencial de Juros (esq.)", "eurusd" = "EUR/USD (dir.)")
#  ) +
#  scale_x_date(date_labels = "%Y", date_breaks = "2 years",
#               expand = expansion(mult = c(0, 0.1))) +
#  labs(title    = "Diferencial de Juros (3 anos) e Câmbio EUR/USD",
#       subtitle = paste("Última observação:", format(max(joint_df$date), "%d/%m/%Y")),
#       x        = NULL,
#       y        = NULL,
#       color    = NULL,
#       caption  = "Fontes: ECB, FRED / Impactus UFRJ") +
#  theme_pandora()

