# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(lubridate)
df <- ecb::get_data("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3Y")
df2 <- ecb::get_data("STS.Q.I9.N.INWR.000000.3.ANR")

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

# Gráfico com dois eixos Y
ggplot(joint_df, aes(x = date)) +
  geom_line(aes(y = spread), color = "#082631", size = 1.2) +
  geom_line(aes(y = eurusd_scaled), color = "#37A6D9", size = 1.1) +
  scale_y_continuous(
    name = "Diferencial de Juros (p.p.)",
    sec.axis = sec_axis(~ (. - mean(joint_df$spread, na.rm = TRUE)) / scale_factor + mean(joint_df$eurusd, na.rm = TRUE),
                        name = "EUR/USD (câmbio)")
  ) +
  labs(title    = "Diferencial de Juros (3 anos) e Câmbio EUR/USD",
       subtitle = paste("Última observação:", format(max(joint_df$date), "%b %Y")),
       x        = NULL,
       caption  = "Fontes: ECB, FRED / Impactus UFRJ") +
  theme_pandora() +
  theme(
    axis.title.y.right = element_text(color = "#37A6D9"),
    axis.text.y.right  = element_text(color = "#37A6D9"),
    axis.title.y.left  = element_text(color = "#082631"),
    axis.text.y.left   = element_text(color = "#082631")
  )
