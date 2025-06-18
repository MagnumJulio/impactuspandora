#---- Stocks
# Códigos
# ^GSPC = S&P
# GLD
# EWG


# Puxar dados SP500 vs GLD

dados_ativos <- get_yahoo_data(c("^GSPC", "GLD"), start = "2019-01-01")

# Transformar para base 100
ativos_base <- dados_ativos %>%
  arrange(date) %>%
  mutate(
    SP500 = `^GSPC` / first(na.omit(`^GSPC`)) * 100,
    GLD  = GLD     / first(na.omit(GLD))     * 100,

  ) %>%
  select(date, SP500, GLD) %>%
  tidyr::pivot_longer(-date, names_to = "ativo", values_to = "valor")

# Gráfico
ggplot(ativos_base, aes(x = date, y = valor, color = ativo)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", size = .3, color = "black") +
  geom_text(data = ativos_base %>% group_by(ativo) %>% slice_tail(n = 1),
            aes(label = sprintf("%.1f", valor)), hjust = -0.2,
            show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", breaks = "1 year",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous() +
  scale_color_manual(values = c(SP500 = "#082631", GLD = "#cd6726")) +
  labs(
    title    = "Performance Relativa: S&P 500 vs Ouro (GLD)",
    subtitle = paste("Base 100 em", format(min(ativos_base$date), "%b %Y")),
    y        = NULL,
    x        = NULL,
    caption  = "Fonte: Yahoo Finance / Impactus UFRJ"
  ) +
  theme_pandora()





# Puxar dados EWG

dados_ativos <- get_yahoo_data(c("RHM.DE"), start = "2025-01-01")

# Transformar para base 100
ativos_base <- dados_ativos %>%
  arrange(date) %>%
  mutate(
    RHM.DE = RHM.DE / first(na.omit(RHM.DE)) * 100
  ) %>%
  select(date, RHM.DE) %>%
  tidyr::pivot_longer(-date, names_to = "ativo", values_to = "valor")


ggplot(ativos_base, aes(x = date, y = valor, color = ativo)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.3, color = "black") +

  geom_text(
    data = ativos_base %>% group_by(ativo) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", valor), color = ativo),
    hjust = -0.2, size = 3.2, show.legend = FALSE
  ) +

  scale_x_date(
    date_labels = "%b/%y",
    date_breaks = "1 month",
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(name = NULL) +

  scale_color_manual(
    values = c("RHM.DE" = "#166083"),
    labels = c("RHM.DE" = "ETF RHM.DE (Rheinmetal)")
  ) +

  labs(
    title    = paste("Performance RHM.DE (Rheinmetal) - 100 =", format(min(ativos_base$date), "%b %Y")),
    subtitle = paste("Última observação:", format(max(ativos_base$date), "%d/%m/%Y")),
    x        = NULL,
    y        = NULL,
    color    = NULL,
    caption  = "Fonte: Yahoo Finance / Impactus UFRJ"
  ) +
  theme_pandora() +
  guides(color = "none")  # <- Remove a legenda da série

#----
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Definir ativos e nomes legíveis
ativos <- c("EWG",  "SPY", "EWJ", "EEM")
nomes_ativos <- c(
  EWG = "Alemanha (EWG)",
  SPY = "EUA (SPY)",
  EWJ = "Japão (EWJ)",
  EEM = "Emergentes (EEM)"
)

# 2. Definir paleta
names(pandora_colors) <- ativos

# 3. Coletar e transformar dados
dados_ativos <- get_yahoo_data(ativos, start = "2025-01-01")

ativos_base <- dados_ativos %>%
  arrange(date) %>%
  mutate(across(all_of(ativos), ~ .x / first(na.omit(.x)) * 100)) %>%
  select(date, all_of(ativos)) %>%
  pivot_longer(-date, names_to = "ativo", values_to = "valor") %>%
  mutate(ativo = factor(ativo, levels = ativos, labels = nomes_ativos[ativos]))

datas_eventos <- tibble::tibble(
  data = as.Date(c(
    "2025-03-04",  # Proposta de fundo por Merz
    "2025-03-05",  # Reação dos mercados
    "2025-03-18",  # Aprovação no Bundestag
    "2025-03-21",  # Aprovação no Bundesrat
    "2025-04-16",  # Relatório de Progresso Fiscal 2025
    "2025-05-19",  # Instruções para elaboração orçamentária
    "2025-06-04",  # Anúncio do pacote fiscal corporativo (€46bi)
    "2025-06-13"   # Detalhamento da execução do fundo (€22bi em 2025)
  )),
  evento = rep("", 8)
)




# Altura máxima ajustada para rótulos das linhas verticais
y_top <- max(ativos_base$valor, na.rm = TRUE) * 1.03


# 4. Gráfico final
ggplot(ativos_base, aes(x = date, y = valor, color = ativo)) +
  geom_line(size = 1.2) +
  # Linhas verticais com anotação
  geom_vline(
    data = datas_eventos,
    aes(xintercept = data),
    linetype = "dotted", color = "gray40", size = 0.6,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = datas_eventos,
    aes(x = data, y = y_top, label = evento),
    angle = 0, vjust = -0.5, hjust = 0,
    size = 3, color = "gray30",
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.3, color = "black") +

  geom_text(
    data = ativos_base %>% group_by(ativo) %>% slice_max(order_by = date, n = 1),
    aes(label = sprintf("%.1f", valor), color = ativo),
    hjust = -0.2, size = 3.2, show.legend = FALSE
  ) +

  scale_x_date(
    date_labels = "%b/%y",
    date_breaks = "1 month",
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(name = NULL) +

  scale_color_manual(
    values = setNames(pandora_colors, nomes_ativos)
  ) +

  labs(
    title    = paste("Performance ETFs - Base 100 em", format(min(ativos_base$date), "%b %Y")),
    subtitle = paste("Última observação:", format(max(ativos_base$date), "%d/%m/%Y")),
    x        = NULL,
    y        = NULL,
    color    = NULL,
    caption  = "Fonte: Yahoo Finance / Impactus UFRJ"
  ) +
  theme_pandora()


