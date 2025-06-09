
#--- Confinança do consumidor

consumer_conf <- get_macro_data_eurostat(
  id = "ei_bsco_m",
  filter_expr = "geo == 'EA20' & indic == 'BS-CSMCI' & s_adj == 'SA' & unit == 'BAL'",
  time_format = "raw"
) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= as.Date("2000-01-01"))


consumer_conf <- consumer_conf %>%
  mutate(date = as.Date(paste0(date, "-01")))

crises <- tibble::tibble(
  inicio = as.Date(c("2008-09-01", "2011-07-01", "2020-03-01", "2022-03-01")),
  fim    = as.Date(c("2009-06-01", "2013-06-01", "2020-12-01", "2023-06-01"))
)

ggplot(consumer_conf, aes(x = date, y = values)) +
  geom_rect(data = crises,
            aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(size = 1.2, color = "#082631") +
  geom_hline(yintercept = mean(consumer_conf$values, na.rm = TRUE),
             linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = consumer_conf %>% slice_tail(n = 1),
            aes(label = sprintf("%.1f", values)),
            hjust = -0.2, vjust = 0, show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous() +
  labs(title    = "Zona do Euro: Indicador de Confiança do Consumidor",
       subtitle = paste("Última observação:", format(max(consumer_conf$date), "%b %Y")),
       y        = "Indicador (saldo de respostas)",
       x        = NULL,
       caption  = "Fonte: Eurostat/Impactus UFRJ") +
  theme_pandora()


#---- Gfk e IFO
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

# GfK: Clima do Consumidor
gfk <- read_csv("D:/Impactus/Projetos-Impactus/pyimpactus/Impactus_Pandora/Dados/sentimento_economico/gfk_clima_consumidor_alemanha_historico.csv") %>%
  filter(tipo == "actual") %>%
  mutate(
    date = as.Date(time),
    mm3m = rollapplyr(value, width = 3, FUN = mean, fill = NA, partial = TRUE)
  )

# Último ponto observado
ultimo_obs <- gfk %>% slice_max(date, n = 1)

ggplot(gfk, aes(x = date)) +
  geom_line(aes(y = value, color = "Valor observado"), size = 1.2) +
  geom_line(aes(y = mm3m, color = "Média móvel 3 meses"), size = 1, linetype = "dashed") +
  geom_text(data = ultimo_obs,
            aes(y = value, label = sprintf("%.1f", value)),
            hjust = -0.2, vjust = -0.2,
            color = "#082631", show.legend = FALSE) +
  scale_color_manual(values = c("Valor observado" = "#082631", "Média móvel 3 meses" = "gray40")) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(title    = "Alemanha: Clima do Consumidor (GfK)",
       subtitle = paste("Última observação:", format(max(gfk$date), "%b %Y")),
       y        = "Indicador (saldo de respostas)",
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: GfK / Impactus UFRJ") +
  theme_pandora()



# Ifo
ifo <- read_csv("D:/Impactus/Projetos-Impactus/pyimpactus/Impactus_Pandora/Dados/sentimento_economico/ifo_business_climate_index_alemanha_historico.csv") %>%
  filter(tipo == "actual") %>%
  mutate(
    date = as.Date(time),
    mm3m = rollapplyr(value, width = 3, FUN = mean, fill = NA, partial = TRUE)
  )

# Último ponto observado
ultimo_obs <- ifo %>% slice_max(date, n = 1)

ggplot(ifo, aes(x = date)) +
  geom_line(aes(y = value, color = "Valor observado"), size = 1.2) +
  geom_line(aes(y = mm3m, color = "Média móvel 3 meses"), size = 1, linetype = "dashed") +
  geom_text(data = ultimo_obs,
            aes(y = value, label = sprintf("%.1f", value)),
            hjust = -0.2, vjust = -0.2,
            color = "#082631", show.legend = FALSE) +
  scale_color_manual(values = c("Valor observado" = "#082631", "Média móvel 3 meses" = "gray40")) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(title    = "Alemanha: Clima de Negócios (Ifo)",
       subtitle = paste("Última observação:", format(max(ifo$date), "%b %Y")),
       y        = "Indicador (saldo de respostas)",
       x        = NULL,
       color    = NULL,
       caption  = "Fonte: Ifo / Impactus UFRJ") +
  theme_pandora()

