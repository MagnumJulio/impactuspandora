#Net Financial Account

# Chaves das séries
series_keys <- list(
  total     = "BPS.M.N.DE.W1.S1.S1.T.N.FA._T.F._Z.EUR._T._X.N.ALL",
  direct    = "BPS.M.N.DE.W1.S1.S1.T.N.FA.D.F._Z.EUR._T._X.N.ALL",
  portfolio = "BPS.M.N.DE.W1.S1.S1.T.N.FA.P.F._Z.EUR._T.M.N.ALL",
  other     = "BPS.M.N.DE.W1.S1.S1.T.N.FA.O.F._Z.EUR._T._X.N.ALL",
  reserve   = "BPS.M.N.DE.W1.S121.S1.T.A.FA.R.F._Z.EUR.X1._X.N.ALL",
  derivative= "BPS.M.N.DE.W1.S1.S1.T.N.FA.F.F7.T.EUR._T.T.N.ALL"
)

# PIB nominal mensal interpolado (exemplo fictício, substitua por série real se tiver)
gdp_monthly <- get_macro_data_ecb("MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.N", parse_date = "none") %>%
  select(date, value) %>%
  rename(gdp = value)%>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y-Q%q"))) %>%
  arrange(date)%>%
  complete(date = seq.Date(min(date), max(date), by = "month")) %>%
  arrange(date) %>%
  mutate(gdp = na.approx(gdp, na.rm = FALSE))


# Função para carregar e nomear
load_series <- function(key, label) {
  get_macro_data_ecb(key, parse_date = "ym") %>%
    select(date, value) %>%
    mutate(variable = label)
}

# Carregar todas as séries
df_list <- list(
  load_series(series_keys$direct, "Direct Investment"),
  load_series(series_keys$portfolio, "Portfolio Investment"),
  load_series(series_keys$other, "Other Investment Assets"),
  load_series(series_keys$reserve, "Reserve Assets"),
  load_series(series_keys$derivative, "Derivatives")
)

# Determinar a maior data das séries financeiras carregadas
ultima_data_fluxo <- df_list %>%
  bind_rows() %>%
  summarise(max_data = max(date, na.rm = TRUE)) %>%
  pull(max_data)

# Estender PIB até essa data
gdp_monthly <- gdp_monthly %>%
  complete(date = seq.Date(min(date), ultima_data_fluxo, by = "month")) %>%
  arrange(date) %>%
  fill(gdp, .direction = "down")


df_stacked <- bind_rows(df_list)

# Acumulado em 12 meses
df_stacked <- df_stacked %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = zoo::rollsum(value, k = 12, align = "right", fill = NA)) %>%
  ungroup()

# Trazer PIB para cada data
df_stacked <- df_stacked %>%
  left_join(gdp_monthly, by = "date") %>%
  mutate(value = 100 * value / gdp)  # % do PIB

# Recalcular série total
df_wide <- df_stacked %>%
  pivot_wider(names_from = variable, values_from = value)

df_wide <- df_wide %>%
  mutate(`Net Financial Account` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  slice(-(1:11)) %>%
  pivot_longer(cols = -c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Atualizar mapeamento de cores
color_map <- c(
  "Direct Investment"       = pandora_colors[3],
  "Other Investment Assets" = pandora_colors[4],
  "Portfolio Investment"    = pandora_colors[2],
  "Reserve Assets"          = pandora_colors[5],
  "Derivatives"             = pandora_colors[1]
)

# --- Filtrar período desejado
data_inicial <- as.Date("2013-01-01")

df_stacked <- df_stacked %>%
  filter(date >= data_inicial)

df_wide <- df_wide %>%
  filter(date >= data_inicial)

# Gráfico
ggplot(df_stacked, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 25) +
  geom_line(
    data = df_wide %>% filter(variable == "Net Financial Account"),
    aes(x = date, y = value, group = 1),
    color = "black", size = 1
  ) +
  geom_text(
    data = df_wide %>% filter(variable == "Net Financial Account") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5,
    color = "black", size = 4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = color_map) +
  labs(
    title = "Net Financial Account - Germany",
    subtitle = paste("% do PIB, 12M - Última observação:", format(max(df_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: ECB / Impactus Pandora"
  )+
  theme_pandora()+
  scale_x_pandora()


# ----
#BP

# --- Série de Balança de Pagamentos: Conta Corrente
bop_keys <- list(
  total     = "BPS.M.Y.DE.W1.S1.S1.T.B.CA._Z._Z._Z.EUR._T._X.N.ALL",
  goods     = "BPS.M.N.DE.W1.S1.S1.T.B.G._Z._Z._Z.EUR._T._X.N.ALL",
  services  = "BPS.M.N.DE.W1.S1.S1.T.B.S._Z._Z._Z.EUR._T._X.N.ALL",
  primary   = "BPS.M.N.DE.W1.S1.S1.T.B.IN1._Z._Z._Z.EUR._T._X.N.ALL",
  secondary = "BPS.M.N.DE.W1.S1.S1.T.B.IN2._Z._Z._Z.EUR._T._X.N.ALL"
)

# Carregar as séries componentes
bop_list <- list(
  load_series(bop_keys$goods, "Goods"),
  load_series(bop_keys$services, "Services"),
  load_series(bop_keys$primary, "Primary Income"),
  load_series(bop_keys$secondary, "Secondary Income")
)

# Determinar última data de fluxo da CA
ultima_data_ca <- bop_list %>%
  bind_rows() %>%
  summarise(max_data = max(date, na.rm = TRUE)) %>%
  pull(max_data)

# Estender o PIB até essa data (caso necessário)
gdp_monthly_ca <- gdp_monthly %>%
  complete(date = seq.Date(min(date), ultima_data_ca, by = "month")) %>%
  arrange(date) %>%
  fill(gdp, .direction = "down")

# Consolidar série CA
df_bop <- bind_rows(bop_list) %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = zoo::rollsum(value, k = 12, align = "right", fill = NA)) %>%
  ungroup()

df_bop <- df_bop %>%
  left_join(gdp_monthly_ca, by = "date") %>%
  mutate(value = 100 * value / gdp)

# Calcular total da Conta Corrente
df_bop_wide <- df_bop %>%
  pivot_wider(names_from = variable, values_from = value)

df_bop_wide <- df_bop_wide %>%
  mutate(`Current Account` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  slice(-(1:11)) %>%
  pivot_longer(cols = -c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Cores para a Conta Corrente
color_bop <- c(
  "Goods"            = pandora_colors[2],
  "Services"         = pandora_colors[3],
  "Primary Income"   = pandora_colors[4],
  "Secondary Income" = pandora_colors[5]
)

# --- Filtrar período desejado
data_inicial <- as.Date("2013-01-01")

df_bop <- df_bop %>%
  filter(date >= data_inicial)

df_bop_wide <- df_bop_wide %>%
  filter(date >= data_inicial)


# Gráfico
ggplot(df_bop, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 25) +
  geom_line(
    data = df_bop_wide %>% filter(variable == "Current Account"),
    aes(x = date, y = value, group = 1),
    color = "black", size = 1
  ) +
  geom_text(
    data = df_bop_wide %>% filter(variable == "Current Account") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5,
    color = "black", size = 4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = color_bop) +
  labs(
    title = "Current Account - Germany",
    subtitle = paste("% do PIB, 12M - Última observação:", format(max(df_bop_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: ECB / Impactus Pandora"
  ) +
  theme_pandora()+
  scale_x_pandora()

df_bop_raw <- bind_rows(bop_list)

# Estender PIB conforme a maior data dos dados
ultima_data_ca <- df_bop_raw %>%
  summarise(max_data = max(date, na.rm = TRUE)) %>%
  pull(max_data)

gdp_monthly_ca <- gdp_monthly %>%
  complete(date = seq.Date(min(date), ultima_data_ca, by = "month")) %>%
  arrange(date) %>%
  fill(gdp, .direction = "down")

# ================================
# 1. Acumulado em 3 meses (% PIB)
# ================================
df_bop_3m <- df_bop_raw %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = rollsum(value, k = 3, align = "right", fill = NA)) %>%
  ungroup() %>%
  left_join(gdp_monthly_ca, by = "date") %>%
  mutate(value = 100 * value / gdp)

df_bop_3m_wide <- df_bop_3m %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Current Account (3M)` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  slice(-(1:2)) %>%
  pivot_longer(-c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))



# ===============================
# 2. Dessazonalização (Mensal)
# ===============================

dessazonalizar <- function(df) {
  df_ts <- ts(df$value, start = c(year(min(df$date)), month(min(df$date))), frequency = 12)
  ajuste <- tryCatch(seas(df_ts), error = function(e) NULL)
  if (is.null(ajuste)) return(NULL)

  df %>%
    mutate(value = final(ajuste))
}

# --- Filtrar período desejado
data_inicial <- as.Date("2013-01-01")

df_bop_raw <- df_bop_raw %>%
  filter(date >= data_inicial)


df_bop_seasonal <- df_bop_raw %>%
  group_by(variable) %>%
  group_modify(~ dessazonalizar(.x)) %>%
  ungroup() %>%
  left_join(gdp_monthly_ca, by = "date") %>%
  mutate(value = 100 * value / gdp)

df_bop_seasonal_wide <- df_bop_seasonal %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Current Account (SA)` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  pivot_longer(-c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))


ggplot(df_bop_3m, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack") +
  geom_line(
    data = df_bop_3m_wide %>% filter(variable == "Current Account (3M)"),
    aes(x = date, y = value),
    color = "black", size = 1
  ) +
  scale_fill_manual(values = color_bop) +
  labs(
    title = "Current Account - Germany",
    subtitle = paste("% do PIB, 3M SA - Última observação:", format(max(df_bop_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: ECB / Impactus Pandora"
  ) +
  theme_pandora()+
  scale_x_pandora()

# -----
# NFA position

# --- Carregar PIB trimestral sem interpolação
gdp_quarterly <- get_macro_data_ecb("MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.N", parse_date = "none") %>%
  select(date, value) %>%
  rename(gdp = value) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y-Q%q")))

# --- Chaves das séries de posição líquida (estoque)
position_keys <- list(
  direct     = "BPS.Q.N.DE.W1.S1.S1.LE.N.FA.D.F._Z.EUR._T._X.N.ALL",
  portfolio_debt  = "BPS.Q.N.DE.W1.S1.S1.LE.N.FA.P.F3.T.EUR._T.M.N.ALL",
  portfolio_equity = "BPS.Q.N.DE.W1.S1.S1.LE.N.FA.P.F5._Z.EUR._T.M.N.ALL",
  other      = "BPS.Q.N.DE.W1.S1.S1.LE.N.FA.O.F._Z.EUR._T._X.N.ALL",
  reserve    = "BPS.Q.N.DE.W1.S121.S1.LE.A.FA.R.F._Z.EUR.X1._X.N.ALL",
  derivative = "BPS.Q.N.DE.W1.S1.S1.LE.N.FA.F.F7.T.EUR._T.T.N.ALL"
)

# --- Função para carregar e nomear séries
load_series <- function(key, label) {
  get_macro_data_ecb(key, parse_date = "none") %>%
    select(date, value) %>%
    mutate(date = as.Date(as.yearqtr(date, format = "%Y-Q%q")),
           variable = label)
}

# --- Carregar todas as séries de posição
df_position_list <- list(
  load_series(position_keys$direct, "Direct Investment"),
  load_series(position_keys$portfolio_debt, "Portfolio - debt"),
  load_series(position_keys$portfolio_equity, "Portfolio - Equity and invest. fund shares"),
  load_series(position_keys$other, "Other Investment Assets"),
  load_series(position_keys$reserve, "Reserve Assets"),
  load_series(position_keys$derivative, "Derivatives")
)

# --- Consolidar base e aplicar acumulado 4 trimestres
df_position_stacked <- bind_rows(df_position_list) %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = zoo::rollsum(value, k = 4, align = "right", fill = NA)) %>%
  ungroup()

# --- Consolidar e unir com PIB trimestral
df_position_stacked <- bind_rows(df_position_list)

df_position_stacked <- df_position_stacked %>%
  left_join(gdp_quarterly, by = "date") %>%
  mutate(value = 100 * value / gdp)

# --- Calcular série total (Net Foreign Asset Position)
df_position_wide <- df_position_stacked %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Net Foreign Asset Position` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  pivot_longer(cols = -c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# --- Mapeamento de cores padrão (com Portfolio Equity)
color_position <- c(
  "Direct Investment"       = pandora_colors[3],
  "Other Investment Assets" = pandora_colors[4],
  "Portfolio - debt"    = pandora_colors[2],
  "Portfolio - Equity and invest. fund shares"        = "#8266CC",
  "Reserve Assets"          = pandora_colors[5],
  "Derivatives"             = pandora_colors[1]
)

# --- Gráfico final
ggplot(df_position_stacked, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 60) +
  geom_line(
    data = df_position_wide %>% filter(variable == "Net Foreign Asset Position"),
    aes(x = date, y = value, group = 1),
    color = "black", size = 1
  ) +
  geom_text(
    data = df_position_wide %>% filter(variable == "Net Foreign Asset Position") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5,
    color = "black", size = 4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = color_position) +
  labs(
    title = "Net International Investment Position - Germany",
    subtitle = paste("% do PIB, 4Q - Última observação:", format(max(df_position_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: ECB / Impactus Pandora"
  ) +
  theme_pandora() +
  scale_x_pandora(breaks = "2 year")



