#---- Stocks
# Puxar dados
# Puxar dados
dados_ativos <- get_yahoo_data(c("^GSPC", "GLD"), start = "2019-01-01")

# Transformar para base 100
ativos_base <- dados_ativos %>%
  arrange(date) %>%
  mutate(
    SP500 = `^GSPC` / first(na.omit(`^GSPC`)) * 100,
    GLD  = GLD     / first(na.omit(GLD))     * 100
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
