library(dplyr)
library(eurostat)
library(janitor)
library(rbcb)
library(lubridate)
library(tidyr)
library(ggplot2)
library(tibble)
library(quantmod)
#install.packages("remotes")
#remotes::install_github("expersso/ecb")
library(ecb)
library(scales)
library(purrr)
library(broom)
library(stringr)


#' Acessar dados do Eurostat com filtros padronizados
#'
#' @param id Código do dataset (ex: "nama_10_gdp")
#' @param filters Lista com filtros (ex: list(geo = "EA", unit = "CLV10_MEUR"))
#' @param ... Argumentos adicionais passados para `get_eurostat()`
#'
#' @return Data frame com colunas padronizadas (tibble)
#' @export


# ------------  1.  Funções genéricas  ----------------------------------------

get_sgs_series <- function(code, start = "2010-01-01", end = Sys.Date()) {
  out <- get_series(code = code,
                    start_date = start,
                    end_date   = end)
  # saída: tibble date | <code>
  rename(out, value = !!as.character(code))
}


get_macro_data_eurostat <- function(id, filter_expr = NULL, ...) {
  if (!requireNamespace("eurostat", quietly = TRUE)) {
    stop("Pacote 'eurostat' não está instalado.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Pacote 'dplyr' não está instalado.")
  }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Pacote 'janitor' não está instalado.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Pacote 'rlang' não está instalado.")
  }

  df <- eurostat::get_eurostat(id, ...)
  df <- janitor::clean_names(df)

  if (!"time_period" %in% names(df)) {
    stop("Coluna 'time_period' não encontrada no dataset.")
  }

  df <- dplyr::rename(df, date = time_period)

  if (!is.null(filter_expr)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(filter_expr))
  }

  return(df)
}


get_macro_data_ecb <- function(series_key, filter = list(), verbose = TRUE, parse_date = "ym") {
  if (!requireNamespace("ecb", quietly = TRUE)) stop("Pacote 'ecb' não está instalado.")
  if (!requireNamespace("janitor", quietly = TRUE)) stop("Pacote 'janitor' não está instalado.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Pacote 'dplyr' não está instalado.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Pacote 'lubridate' não está instalado.")

  df <- ecb::get_data(series_key, filter)

  df <- df %>%
    janitor::clean_names() %>%
    dplyr::rename(date = obstime, value = obsvalue)

  # Tratamento de data com if simples
  if (parse_date == "ym") {
    df$date <- lubridate::ym(df$date)
  } else if (parse_date == "date") {
    df$date <- as.Date(df$date)
  } # se for "none", não faz nada

  if ("title" %in% names(df)) {
    df <- dplyr::mutate(df, title = factor(title))
  }

  if (verbose && "title" %in% names(df)) {
    msg <- unique(df$title)
    cat("Série(s) carregada(s):", paste(msg, collapse = ", "), "\n")
  }

  return(df)
}



get_fred_data <- function(ticker, start = "2000-01-01", end = Sys.Date()) {
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Pacote 'quantmod' não está instalado.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Pacote 'tibble' não está instalado.")
  }

  suppressWarnings({
    xt <- quantmod::getSymbols(ticker, src = "FRED", auto.assign = FALSE,
                               from = start, to = end)
  })

  df <- tibble::tibble(
    date = as.Date(index(xt)),
    value = as.numeric(xt[, 1])
  )

  return(df)
}


get_yahoo_data <- function(tickers = c("^GSPC", "GLD"),
                           start = "2010-01-01", end = Sys.Date()) {
  if (!requireNamespace("quantmod", quietly = TRUE)) stop("Pacote 'quantmod' necessário.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Pacote 'tibble' necessário.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Pacote 'dplyr' necessário.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Pacote 'tidyr' necessário.")

  dados <- lapply(tickers, function(tk) {
    symbol <- gsub("\\^", "", tk)  # remove ^ para formar o prefixo de colunas
    suppressWarnings({
      xt <- quantmod::getSymbols(tk, src = "yahoo", auto.assign = FALSE,
                                 from = start, to = end)
    })

    colname_adjusted <- paste0(symbol, ".Adjusted")
    if (!colname_adjusted %in% colnames(xt)) stop(paste("Coluna", colname_adjusted, "não encontrada."))

    tibble::tibble(
      date = as.Date(index(xt)),
      ticker = tk,
      value = as.numeric(xt[, colname_adjusted])
    )
  })

  bind_rows(dados) %>%
    pivot_wider(names_from = ticker, values_from = value)
}


#---- Tema ----#
theme_pandora <- function(base_size = 15, show_grid = FALSE) {
  base <- theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face = "bold", hjust = 0),
      plot.subtitle    = element_text(hjust = 0),
      plot.caption     = element_text(size = 10, color = "gray50", hjust = 0),
      legend.title     = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal"
    )

  if (!show_grid) {
    base <- base + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  }

  return(base)
}






