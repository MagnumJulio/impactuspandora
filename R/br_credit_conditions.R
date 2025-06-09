# =======================================================================
#  Impactus Pandora · Minipipeline BCB  ·  crédito (saldo + concessões)
# =======================================================================

# ------------  0.  Pacotes  ---------------------------------------------------
library(rbcb)      # acesso SGS
library(dplyr)     # wrangling
library(tidyr)     # pivot_longer
library(lubridate) # datas
library(ggplot2)


# --- Função de deflação robusta --------------------------------------
deflate_ipca <- function(df_nom, def_code = 433) {

  # Get IPCA series
  ipca_raw <- get_series(code = def_code,
                         start_date = min(df_nom$date),
                         end_date   = max(df_nom$date)) |>
    rename(ipca_monthly = !!as.character(def_code))

  # Convert monthly IPCA to cumulative index
  ipca <- ipca_raw |>
    arrange(date) |>
    mutate(
      # Convert percentage to multiplier and calculate cumulative index
      ipca_factor = (ipca_monthly/100) + 1,
      ipca_index = cumprod(ipca_factor)
    ) |>
    select(date, ipca_index)

  # Join with nominal data
  df <- full_join(df_nom, ipca, by = "date") |>
    arrange(date) |>
    filter(!is.na(value) & !is.na(ipca_index))

  # Use LATEST date as base (last observation)
  latest_date <- max(df$date)
  latest_ipca_index <- df$ipca_index[df$date == latest_date]
  stopifnot(length(latest_ipca_index) == 1)

  # Calculate real values (expressed in latest period's prices)
  df |>
    mutate(saldo_real = value * (latest_ipca_index / ipca_index)) |>
    select(date, saldo_real) |>
    arrange(date)
}



# ------------  2.  Wrappers específicos  --------------------------------------

# 2.1 Saldo total de crédito (nominal -> real)
get_credit_balance_real <- function(start = "2010-01-01") {
  nom <- get_sgs_series(20539, start)        # série nominal (saldo total)
  deflate_ipca(nom)  # ✅ Fixed: deflate_ipca already returns 'saldo_real' column
}

# 2.2 Concessões de crédito (PJ, PF, Total) – todas reais
get_credit_concessions_real <- function(start = "2010-01-01") {

  ids <- c(total = 20631, pj = 20632, pf = 20633)

  purrr::imap_dfr(ids, ~ {
    nom <- get_sgs_series(.x, start)
    deflate_ipca(nom) |>
      mutate(serie = .y) |>
      rename(value_real = saldo_real) |>  # ✅ Fixed: rename saldo_real to value_real
      select(date, serie, value_real)
  }) |>
    pivot_wider(names_from = serie, values_from = value_real) |>
    arrange(date)
}

# ------------  3.  Coleta e preparo para gráficos  ---------------------------

saldo_df <- get_credit_balance_real("2014-01-01")

concess_df <- get_credit_concessions_real("2014-01-01") |>
  pivot_longer(-date, names_to = "serie", values_to = "value_real")

# Para o gráfico, nomes legíveis
saldo_long <- saldo_df |>
  mutate(serie = "Saldo_total_real") |>
  rename(value_real = saldo_real)  # ✅ Fixed: rename saldo_real to value_real


# Impactus Pandora - Tema Padrão para ggplot2

# Cores oficiais (em ordem de prioridade)
pandora_colors <- c("#082631", "#166083", "#37A6D9", "#AFABAB", "#82C1DB")

# Função de tema

#' Tema padronizado para a identidade visual da Liga (Impactus Pandora)
#' @param base_size Tamanho base da fonte (default: 13)
#' @return Um objeto ggplot2::theme
#' @export
theme_pandora <- function(base_size = 15) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title    = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      plot.caption  = element_text(size = 10, color = "gray50", hjust = 0),
      legend.title  = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
}

# Função auxiliar para aplicar paleta de cores manualmente (nomes automáticos)
# Ex: scale_color_pandora(3) aplica 3 primeiras cores
scale_color_pandora <- function(n) {
  scale_color_manual(values = pandora_colors[seq_len(n)])
}

# Versão para fill (caso use geom_bar ou similares)
scale_fill_pandora <- function(n) {
  scale_fill_manual(values = pandora_colors[seq_len(n)])
}

# Eixo y com 1 dígito decimal (comma)
scale_y_pandora <- function() {
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.1))
}

# Labels de linha com 2 dígitos
label_2d <- function(x) {
  sprintf("%.2f", x)
}


# ------------  4.  Plot: Saldo total real ------------------------------------

ggplot(saldo_long, aes(x = date, y = value_real/1e6, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 3.5, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = saldo_long %>% slice_tail(n = 1),
            aes(label = scales::comma(value_real/1e6, accuracy = 0.01)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_pandora(1) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_pandora() +
  labs(title    = "Brasil: Saldo Real Total de Crédito (R$ bi)",
       subtitle = paste("Última observação:", format(max(saldo_long$date), "%b %Y")),
       y        = "R$ bilhões",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (série 20539) e IPCA (433)") +
  theme_pandora()

# ------------  5.  Plot: Concessões reais ------------------------------------

core_palette <- c(total = pandora_colors[1],
                  pj    = pandora_colors[2],
                  pf    = pandora_colors[3])

ggplot(concess_df, aes(x = date, y = value_real/1e6, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = concess_df %>% group_by(serie) %>% slice_tail(n = 1),
            aes(label = scales::comma(value_real/1e6, accuracy = 0.01)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = core_palette,
                     labels = c(total = "Total", pj = "PJ", pf = "PF")) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_pandora() +
  labs(title    = "Brasil: Concessões de Crédito Reais (R$ bi)",
       subtitle = paste("Última observação:", format(max(concess_df$date), "%b %Y")),
       y        = "R$ bilhões",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (20633, 20632, 20631) e IPCA (433)") +
  theme_pandora()

#----- Family debt ratio

get_family_debt_ratio <- function(start = "2010-01-01") {
  df <- get_series(code = 29037, start_date = start) |>
    rename(endividamento = `29037`) |>
    mutate(date = as.Date(date)) |>
    arrange(date)
  return(df)
}

df_divida <- get_family_debt_ratio("2014-01-01")

ggplot(df_divida, aes(x = date, y = endividamento, color = "Endividamento")) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 50, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = df_divida %>% slice_tail(n = 1),
            aes(label = sprintf("%.2f%%", endividamento)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = c("Endividamento" = pandora_colors[1])) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  labs(title    = "Brasil: Endividamento das Famílias (% da Renda Anual)",
       subtitle = paste("Última observação:", format(max(df_divida$date), "%b %Y")),
       y        = "% da renda acumulada 12m",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (série 29037)") +
  theme_pandora()


# ------------  6.  Plot: Inadimplência ----------------------------------------

inad_palette <- c(total = pandora_colors[1],
                  pj    = pandora_colors[2],
                  pf    = pandora_colors[3])

inad_df <- bind_rows(
  get_series(code = 21085, start_date = "2014-01-01") |>
    rename(date = date, value = `21085`) |>
    mutate(serie = "total"),
  get_series(code = 21112, start_date = "2014-01-01") |>
    rename(date = date, value = `21112`) |>
    mutate(serie = "pf"),
  get_series(code = 21086, start_date = "2014-01-01") |>
    rename(date = date, value = `21086`) |>
    mutate(serie = "pj")
)

# Plot

ggplot(inad_df, aes(x = date, y = value, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = inad_df %>% group_by(serie) %>% slice_tail(n = 1),
            aes(label = sprintf("%.2f%%", value)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = inad_palette,
                     labels = c(total = "Total", pj = "PJ", pf = "PF")) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  labs(title    = "Brasil: Inadimplência do Crédito Livre (%)",
       subtitle = paste("Última observação:", format(max(inad_df$date), "%b %Y")),
       y        = "% inadimplente",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (21085, 21112, 21086)") +
  theme_pandora()



get_credit_consignado_real <- function(start = "2010-01-01") {

  ids <- c(privado = 20668, publico = 20669, aposentados = 20670)

  purrr::imap_dfr(ids, ~ {
    nom <- get_sgs_series(.x, start)
    deflate_ipca(nom) |>
      mutate(serie = .y) |>
      rename(value_real = saldo_real) |>  # ✅ Ajuste: renomear saldo_real para value_real
      select(date, serie, value_real)
  }) |>
    pivot_wider(names_from = serie, values_from = value_real) |>
    arrange(date)
}


core_palette <- c(privado = pandora_colors[1],
                  publico = pandora_colors[2],
                  aposentados = pandora_colors[3])

credit_consignado_df <- get_credit_consignado_real()

credit_consignado_long <- credit_consignado_df |>
  pivot_longer(-date, names_to = "serie", values_to = "value_real")


ggplot(credit_consignado_long, aes(x = date, y = value_real/1e3, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = credit_consignado_long %>% group_by(serie) %>% slice_tail(n = 1),
            aes(label = scales::comma(value_real/1e3, accuracy = 0.01)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = core_palette,
                     labels = c(privado = "Privado", publico = "Público", aposentados = "Aposentados")) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Brasil: Concessões de Crédito Consignado Reais (R$ mi)",
       subtitle = paste("Última observação:", format(max(credit_consignado_long$date), "%b %Y")),
       y        = "R$ milhões",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (20670, 20668, 20669) e IPCA (433)") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption  = element_text(size = 8, color = "gray50", hjust = 0),
        legend.title  = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")


#----

get_icc_series <- function(start = "2011-01-01") {
  df <- get_sgs_series(25351, start)
  df <- df |>
    mutate(serie = "icc") |>
    select(date, serie, value) |>
    arrange(date)
  return(df)
}

icc_palette <- c(icc = pandora_colors[1])


icc_df <- get_icc_series()

ggplot(icc_df, aes(x = date, y = value, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = mean(icc_df$value, na.rm = TRUE), linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = icc_df %>% slice_tail(n = 1),
            aes(label = sprintf("%.2f%%", value)), hjust = -0.2, show.legend = FALSE) +
  scale_color_manual(values = icc_palette, labels = c(icc = "ICC")) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  labs(title    = "Brasil: Índice de Custo do Crédito (ICC)",
       subtitle = paste("Última observação:", format(max(icc_df$date), "%b %Y")),
       y        = "Taxa efetiva média (%)",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (25351)") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption  = element_text(size = 8, color = "gray50", hjust = 0),
        legend.title  = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")


# ---- Direcionado BNDES


get_sgs_series <- function(code, start = "2010-01-01", end = Sys.Date()) {
  out <- get_series(code = code,
                    start_date = start,
                    end_date   = end)
  # saída: tibble date | <code>
  rename(out, value = !!as.character(code))
}

get_credit_direct_balance_real <- function(start = "2010-01-01") {
  nom <- get_sgs_series(20604, start)        # série nominal (saldo total)
  deflate_ipca(nom)  # ✅ Fixed: deflate_ipca already returns 'saldo_real' column
}


saldo_df_direct <- get_credit_direct_balance_real("2014-01-01")

# Para o gráfico, nomes legíveis
saldo_long <- saldo_df_direct |>
  mutate(serie = "Saldo_total_direcionado_real") |>
  rename(value_real = saldo_real)  # ✅ Fixed: rename saldo_real to value_real


ggplot(saldo_long, aes(x = date, y = value_real/1e6, color = serie)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = .3, colour = "black") +
  geom_text(data = saldo_long %>% slice_tail(n = 1),
            aes(label = scales::comma(value_real/1e6, accuracy = 0.01)),
            hjust = -0.2, show.legend = FALSE) +
  scale_color_pandora(1) +
  scale_x_date(date_labels = "%Y", breaks = "2 years",
               expand = expansion(mult = c(0, 0.1))) +
  scale_y_pandora() +
  labs(title    = "Brasil: Saldo Real Total de Crédito BNDES (R$ bi)",
       subtitle = paste("Última observação:", format(max(saldo_long$date), "%b %Y")),
       y        = "R$ bilhões",
       x        = NULL,
       caption  = "Fonte: BCB/SGS (série 20604) e IPCA (433)") +
  theme_pandora()



#==== LIXO ====


# Saldo total do crédito (SGS 1424) —
# opcionalmente deflacionado pelo IPCA (SGS 433)
get_bcb_credit_balance <- function(start = "2005-01-01",
                                   end   = Sys.Date(),
                                   real  = FALSE,
                                   base_date = NULL) {

  # ── dependências mínimas ─────────────────────────────────────────────
  stopifnot(
    requireNamespace("rbcb",  quietly = TRUE),
    requireNamespace("dplyr", quietly = TRUE)
  )
  library(rbcb); library(dplyr)

  # 1.  consulta nominal ------------------------------------------------
  saldo_id  <- 20539
  saldo_nom <- get_series(code = saldo_id,
                          start_date = start,
                          end_date   = end) |>
    rename(saldo_nominal = !!as.character(saldo_id))

  # 2.  deflaciona (se pedido) ------------------------------------------
  if (real) {
    ipca_id <- 433
    ipca <- get_series(code = ipca_id,
                       start_date = start,
                       end_date   = end) |>
      rename(ipca = !!as.character(ipca_id))

    df <- saldo_nom |>
      left_join(ipca, by = "date") |>
      arrange(date)

    if (is.null(base_date)) base_date <- df$date[1]
    base_ipca <- df$ipca[df$date == base_date]
    if (!length(base_ipca))
      stop("`base_date` fora do intervalo IPCA.")

    df <- df |>
      mutate(deflator   = base_ipca / ipca,
             saldo_real = saldo_nominal * deflator) |>
      select(date, saldo_nominal, saldo_real)

    return(df)
  }

  select(saldo_nom, date, saldo_nominal)
}



df <- get_bcb_credit_balance(real = TRUE, start = "2010-01-01")
