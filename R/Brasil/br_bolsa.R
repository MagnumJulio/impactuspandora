#---- Dados da B3

library(httr)
library(jsonlite)
library(dplyr)

# Substitua pela sua chave de API
api_key <- "SUA_CHAVE_AQUI"

# Endpoint para fluxo de investidores estrangeiros
url <- "https://api.dadosdemercado.com.br/v1/investors"

# Requisição GET com autenticação
res <- GET(url, add_headers("Authorization" = paste("Bearer", api_key)))

# Verificar se a requisição deu certo
stop_for_status(res)

# Converter resposta para data.frame
dados <- content(res, as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  as.data.frame()
