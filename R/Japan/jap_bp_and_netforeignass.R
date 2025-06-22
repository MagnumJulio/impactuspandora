#----
# BP

# --- Chaves das séries da Conta Corrente do Japão
bop_keys_japan <- list(
  total     = "BP01'BPBP6JYNCB",
  goods     = "BP01'BPBP6JYNTB",
  services  = "BP01'BPBP6JYNSN",
  primary   = "BP01'BPBP6JYNPIN",
  secondary = "BP01'BPBP6JYNSIN"
)

# --- Labels das séries
labels_japan <- c(
  "BP01'BPBP6JYNCB"  = "Current Account",
  "BP01'BPBP6JYNTB"  = "Goods",
  "BP01'BPBP6JYNSN"  = "Services",
  "BP01'BPBP6JYNPIN" = "Primary Income",
  "BP01'BPBP6JYNSIN" = "Secondary Income"
)

# --- Wrapper de carregamento individual
load_series_japan <- function(key, label) {
  get_macro_data_japan(series_codes = key, labels = setNames(label, key))
}

# --- Carregar todas as séries da CA japonesa
bop_list_japan <- list(
  load_series_japan(bop_keys_japan$goods, labels_japan[2]),
  load_series_japan(bop_keys_japan$services, labels_japan[3]),
  load_series_japan(bop_keys_japan$primary, labels_japan[4]),
  load_series_japan(bop_keys_japan$secondary, labels_japan[5])
)

df_bop_japan_raw <- bind_rows(bop_list_japan)

gdp_japan <- get_fred_data("JPNNGDP") %>%
  mutate(date = as.Date(as.yearqtr(date)), gdp = value) %>%
  select(date, gdp)

ultima_data_jp <- max(df_bop_japan_raw$date, na.rm = TRUE)

# --- Estender PIB até essa data
gdp_monthly_japan <- gdp_japan %>%
  complete(date = seq.Date(min(date), ultima_data_jp, by = "month")) %>%
  arrange(date) %>%
  fill(gdp, .direction = "down")

# --- Calcular acumulado em 12 meses (% do PIB)
df_bop_japan <- df_bop_japan_raw %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = zoo::rollsum(value, k = 12, align = "right", fill = NA)) %>%
  ungroup() %>%
  left_join(gdp_monthly_japan, by = "date") %>%
  mutate(value = 100 * value / gdp)

# --- Calcular total da Conta Corrente
df_bop_japan_wide <- df_bop_japan %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Current Account` = rowSums(select(., -date, -gdp), na.rm = FALSE)) %>%
  mutate(`Current Account` = ifelse(rowSums(is.na(select(., -date, -gdp))) == ncol(.) - 2, NA, `Current Account`)) %>%
  slice(-(1:11)) %>%
  pivot_longer(cols = -c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))


# --- Gráfico
color_bop <- c(
  "Goods"            = pandora_colors[2],
  "Services"         = pandora_colors[3],
  "Primary Income"   = pandora_colors[4],
  "Secondary Income" = pandora_colors[5]
)

data_inicial <- as.Date("2013-01-01")

df_bop_japan <- df_bop_japan %>%
  filter(date >= data_inicial)

df_bop_japan_wide <- df_bop_japan_wide %>%
  filter(date >= data_inicial)


ggplot(df_bop_japan, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 25) +
  geom_line(
    data = df_bop_japan_wide %>% filter(variable == "Current Account"),
    aes(x = date, y = value), color = "black", size = 1
  ) +
  geom_text(
    data = df_bop_japan_wide %>% filter(variable == "Current Account") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5,
    color = "black", size = 4, show.legend = FALSE
  ) +
  scale_fill_manual(values = color_bop) +
  labs(
    title = "Current Account - Japan",
    subtitle = paste("% do PIB, 12M - Última observação:", format(max(df_bop_japan_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: BOJ / FRED / Impactus Pandora"
  ) +
  theme_pandora() +
  scale_x_pandora()

#----
# Financial Account


# --- Conta Financeira do Japão (Net Financial Account)
keys_financial_jp <- list(
  direct     = "BP01'BPBP6JYNFB1",
  portfolio  = "BP01'BPBP6JYNFB2",
  other      = "BP01'BPBP6JYNFB3",
  derivatives= "BP01'BPBP6JYNFB5",
  reserves   = "BP01'BPBP6JYNFA4"
)

labels_financial_jp <- c(
  "BP01'BPBP6JYNFB1" = "Direct Investment",
  "BP01'BPBP6JYNFB2" = "Portfolio Investment",
  "BP01'BPBP6JYNFB3" = "Other Investment Assets",
  "BP01'BPBP6JYNFB5" = "Derivatives",
  "BP01'BPBP6JYNFA4" = "Reserve Assets"
)

# Wrapper de série do Japão
load_series_japan <- function(code, label) {
  get_macro_data_japan(series_codes = code, labels = setNames(label, code)) %>%
    mutate(variable = label)
}

# Carregar todas as séries
df_fin_list <- list(
  load_series_japan(keys_financial_jp$direct, "Direct Investment"),
  load_series_japan(keys_financial_jp$portfolio, "Portfolio Investment"),
  load_series_japan(keys_financial_jp$other, "Other Investment Assets"),
  load_series_japan(keys_financial_jp$derivatives, "Derivatives"),
  load_series_japan(keys_financial_jp$reserves, "Reserve Assets")
)

# Última data da balança financeira
ultima_data_fin <- max(df_fin_stacked$date, na.rm = TRUE)

# PIB nominal trimestral (FRED) -> interpolado e estendido até última data dos dados financeiros
gdp_jp <- get_fred_data("JPNNGDP") %>%
  rename(gdp = value) %>%
  mutate(date = as.Date(as.yearqtr(date))) %>%
  arrange(date) %>%
  complete(date = seq.Date(min(date), ultima_data_fin, by = "month")) %>%  # estende até a última data desejada
  mutate(gdp = zoo::na.approx(gdp, na.rm = FALSE)) %>%  # interpola valores intermediários
  fill(gdp, .direction = "down")  # repete último valor trimestral conhecido (ex: jan a mar)


# Consolidar e calcular acumulado 12M
df_fin_stacked <- bind_rows(df_fin_list) %>%
  arrange(variable, date) %>%
  group_by(variable) %>%
  mutate(value = zoo::rollsum(value, k = 12, align = "right", fill = NA)) %>%
  ungroup() %>%
  left_join(gdp_jp, by = "date") %>%
  mutate(value = 100 * value / gdp)

# Calcular total
df_fin_wide <- df_fin_stacked %>%
  pivot_wider(names_from = variable, values_from = value)

# Remove linhas onde todas as componentes estão ausentes
df_fin_wide <- df_fin_wide %>%
  filter(rowSums(across(-c(date, gdp), ~ !is.na(.x))) > 0)

# Calcula total
df_fin_wide <- df_fin_wide %>%
  mutate(`Net Financial Account` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  pivot_longer(cols = -c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))


# Cores
color_financial <- c(
  "Direct Investment"       = pandora_colors[3],
  "Other Investment Assets" = pandora_colors[4],
  "Portfolio Investment"    = pandora_colors[2],
  "Reserve Assets"          = pandora_colors[5],
  "Derivatives"             = pandora_colors[1]
)

# Corte temporal
data_inicial <- as.Date("2013-01-01")
df_fin_stacked <- df_fin_stacked %>% filter(date >= data_inicial)
df_fin_wide <- df_fin_wide %>% filter(date >= data_inicial)

# Gráfico
ggplot(df_fin_stacked, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 25) +
  geom_line(
    data = df_fin_wide %>% filter(variable == "Net Financial Account"),
    aes(x = date, y = value),
    color = "black", size = 1
  ) +
  geom_text(
    data = df_fin_wide %>% filter(variable == "Net Financial Account") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5,
    color = "black", size = 4,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = color_financial) +
  labs(
    title = "Net Financial Account - Japan",
    subtitle = paste("% do PIB, 12M - Última observação:", format(max(df_fin_wide$date), "%b %Y")),
    x = NULL, y = NULL,
    caption = "Fonte: BoJ / FRED / Impactus Pandora"
  ) +
  theme_pandora() +
  scale_x_pandora()


#----
#NFA o NIIP

assets_position_keys_jp <- list(
  direct          = "BP01'BPBP6PA1",
  portfolio_equity = "BP01'BPBP6PA21",
  portfolio_debt   = "BP01'BPBP6PA22",
  derivatives     = "BP01'BPBP6PA3",
  other           = "BP01'BPBP6PA4",
  reserve         = "BP01'BPBP6PA47"  # observar a documentação se inclui no "other"
)

liabilities_position_keys_jp <- list(
  direct          = "BP01'BPBP6PL1",
  portfolio_equity = "BP01'BPBP6PL21",
  portfolio_debt   = "BP01'BPBP6PL22",
  derivatives     = "BP01'BPBP6PL3",
  other           = "BP01'BPBP6PL4"  # observar a documentação se inclui no "other"
)


load_series_japan <- function(code, label) {
  get_macro_data_japan(series_codes = code, labels = setNames(label, code)) %>%
    mutate(variable = label)
}


df_asset_position_list_jp <- list(
  load_series_japan(assets_position_keys_jp$direct, "Direct Investment"),
  load_series_japan(assets_position_keys_jp$portfolio_equity, "Portfolio - Equity"),
  load_series_japan(assets_position_keys_jp$portfolio_debt, "Portfolio - Debt"),
  load_series_japan(assets_position_keys_jp$derivatives, "Derivatives"),
  load_series_japan(assets_position_keys_jp$other, "Other Investment Assets"),
  load_series_japan(assets_position_keys_jp$reserve, "Reserve Assets")
)

df_liabilities_position_list_jp <- list(
  load_series_japan(liabilities_position_keys_jp$direct, "Direct Investment"),
  load_series_japan(liabilities_position_keys_jp$portfolio_equity, "Portfolio - Equity"),
  load_series_japan(liabilities_position_keys_jp$portfolio_debt, "Portfolio - Debt"),
  load_series_japan(liabilities_position_keys_jp$derivatives, "Derivatives"),
  load_series_japan(liabilities_position_keys_jp$other, "Other Investment Assets")
)

# Consolidar Assets
df_asset_position_jp <- bind_rows(df_asset_position_list_jp) %>%
  mutate(date = as.Date(as.yearqtr(date))) %>%
  drop_na(value)

# Consolidar Liabilities
df_liabilities_position_jp <- bind_rows(df_liabilities_position_list_jp) %>%
  mutate(date = as.Date(as.yearqtr(date))) %>%
  drop_na(value)

# Carregar PIB (via FRED: JPNNGDP)
gdp_jp <- get_fred_data("JPNNGDP") %>%
  rename(gdp = value) %>%
  mutate(date = as.Date(as.yearqtr(date)))

# 1. Renomear colunas para diferenciar valores
df_asset_position_jp <- df_asset_position_jp %>%
  rename(value_asset = value)

df_liabilities_position_jp <- df_liabilities_position_jp %>%
  rename(value_liab = value)

# 2. Juntar assets e liabilities por data + variável
df_niip_jp <- df_asset_position_jp %>%
  full_join(df_liabilities_position_jp, by = c("date", "variable")) %>%
  mutate(
    value_asset = coalesce(value_asset, 0),
    value_liab  = coalesce(value_liab, 0),
    value = value_asset - value_liab
  ) %>%
  select(date, variable, value)


# 3. Integrar o PIB e calcular NIIP como % do PIB
df_niip_jp <- df_niip_jp %>%
  left_join(gdp_jp, by = "date") %>%
  mutate(value = 100 * value / gdp)

# Resultado: df_niip_jp com variável, data e valor (%PIB)

# Calcular série total
df_wide_jp <- df_niip_jp %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Net Foreign Asset Position` = rowSums(select(., -date, -gdp), na.rm = TRUE)) %>%
  pivot_longer(-c(date, gdp), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Cores
color_position <- c(
  "Direct Investment"               = pandora_colors[3],
  "Other Investment Assets"         = pandora_colors[4],
  "Portfolio - Debt"                = pandora_colors[2],
  "Portfolio - Equity"              = "#8266CC",
  "Reserve Assets"                  = pandora_colors[5],
  "Derivatives"                     = pandora_colors[1]
)

data_inicial <- as.Date("2013-01-01")

df_niip_jp <- df_niip_jp %>%
  filter(date >= data_inicial)

df_wide_jp <- df_wide_jp %>%
  filter(date >= data_inicial)

ggplot(df_niip_jp, aes(x = date, y = value, fill = variable)) +
  geom_col(position = "stack", width = 60) +
  geom_line(
    data = df_wide_jp %>% filter(variable == "Net Foreign Asset Position"),
    aes(y = value), color = "black", size = 1
  ) +
  geom_text(
    data = df_wide_jp %>% filter(variable == "Net Foreign Asset Position") %>% slice_tail(n = 1),
    aes(label = scales::comma(value, accuracy = 0.01)),
    hjust = -0.1, vjust = -0.5, color = "black", size = 4, show.legend = FALSE
  ) +
  scale_fill_manual(values = color_position) +
  labs(
    title = "Net International Investment Position - Japan",
    subtitle = paste("% do PIB, Última observação:", format(max(df_wide_jp$date), "%b %Y")),
    caption = "Fonte: BoJ / FRED / Impactus Pandora",
    x = NULL, y = NULL
  ) +
  theme_pandora() +
  scale_x_pandora(breaks = "2 year")





