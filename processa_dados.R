# preprocessa_dados.R
# Executar uma vez para gerar a base pré-processada em .rds

library(dplyr)
library(data.table)
library(stringr)
library(janitor)
library(readxl)

# ----------------------------
# Configurações
# ----------------------------

anos_base <- 2016:2025
base_url_imp <- "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_%d.csv"

arquivo_ncm <- "ncm_lookup.csv"
arquivo_pais <- "pais_lookup.xlsx"
arquivo_saida <- "base_agregada_ncm_paises.rds"

# ----------------------------
# Funções auxiliares
# ----------------------------

guess_col <- function(nms, patterns) {
  hit <- nms[Reduce(`|`, lapply(patterns, function(p) {
    str_detect(nms, regex(p, ignore_case = TRUE))
  }))]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

parse_num <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)
  
  if (any(str_detect(x, ","), na.rm = TRUE)) {
    x <- gsub(".", "", x, fixed = TRUE)
    x <- gsub(",", ".", x, fixed = TRUE)
  }
  
  suppressWarnings(as.numeric(x))
}

read_csv_semicolon <- function(path_or_url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(path_or_url, tmp, mode = "wb", quiet = TRUE)
  fread(tmp, sep = ";", encoding = "UTF-8", showProgress = FALSE) |>
    clean_names()
}

read_imp_year <- function(ano) {
  url <- sprintf(base_url_imp, ano)
  tryCatch(read_csv_semicolon(url), error = function(e) NULL)
}

load_ncm_lookup <- function(path = arquivo_ncm) {
  if (!file.exists(path)) stop("Não encontrei o arquivo: ", path)
  
  out <- fread(path, showProgress = FALSE, colClasses = "character") |>
    clean_names()
  
  cod_col <- guess_col(
    names(out),
    c("^co_ncm$", "^cod_ncm$", "^ncm$", "^codigo_ncm$", "^cd_ncm$")
  )
  desc_col <- guess_col(
    names(out),
    c("^ds_ncm$", "^no_ncm$", "^nm_ncm$", "descricao.*ncm", "descricao")
  )
  
  if (is.na(cod_col) || is.na(desc_col)) {
    stop("O arquivo de NCM precisa ter colunas compatíveis com co_ncm e ds_ncm.")
  }
  
  out |>
    transmute(
      co_ncm = gsub("[^0-9]", "", as.character(.data[[cod_col]])),
      ds_ncm = as.character(.data[[desc_col]])
    ) |>
    filter(!is.na(co_ncm), co_ncm != "") |>
    mutate(ncm_len = nchar(co_ncm)) |>
    distinct(co_ncm, .keep_all = TRUE)
}

load_pais_lookup <- function(path = arquivo_pais) {
  if (!file.exists(path)) stop("Não encontrei o arquivo: ", path)
  
  out <- readxl::read_xlsx(path) |>
    as.data.frame() |>
    clean_names()
  
  cod_col <- guess_col(
    names(out),
    c("^co_pais$", "^cod_pais$", "^cd_pais$", "^pais$")
  )
  nome_col <- guess_col(
    names(out),
    c("^no_pais$", "^nm_pais$", "^ds_pais$", "descricao.*pais", "^pais$", "nome")
  )
  
  if (is.na(cod_col) || is.na(nome_col)) {
    stop("O arquivo de países precisa ter colunas compatíveis com co_pais e pais.")
  }
  
  out |>
    transmute(
      co_pais = as.character(.data[[cod_col]]),
      pais = as.character(.data[[nome_col]])
    ) |>
    distinct(co_pais, .keep_all = TRUE)
}

# ----------------------------
# Importação e tratamento
# ----------------------------

message("Carregando tabelas auxiliares...")
ncm_lookup <- load_ncm_lookup() |>
  mutate(co_ncm = as.character(co_ncm))

pais_lookup <- load_pais_lookup() |>
  mutate(co_pais = as.character(co_pais))

message("Baixando e processando dados anuais...")
base_raw <- bind_rows(lapply(anos_base, function(a) {
  message("Ano: ", a)
  x <- tryCatch(read_imp_year(a), error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0) return(NULL)
  
  x |>
    mutate(
      co_ano = as.integer(co_ano),
      co_mes = as.integer(co_mes),
      co_ncm = str_pad(gsub("[^0-9]", "", as.character(co_ncm)), 8, pad = "0"),
      co_pais = as.character(co_pais),
      kg_liquido = parse_num(kg_liquido),
      vl_fob = parse_num(vl_fob)
    ) |>
    select(co_ano, co_mes, co_ncm, co_pais, kg_liquido, vl_fob)
}))

message("Agregando base detalhada por ano / NCM 8 dígitos / país...")
base_detalhada <- base_raw |>
  filter(
    !is.na(co_ncm),
    !is.na(co_pais),
    !is.na(vl_fob),
    !is.na(kg_liquido),
    kg_liquido > 0
  ) |>
  group_by(co_ano, co_ncm, co_pais) |>
  summarise(
    vl_fob = sum(vl_fob, na.rm = TRUE),
    kg_liquido = sum(kg_liquido, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# Agregação somente para códigos que existem no lookup
# ----------------------------

message("Preparando agregações apenas para NCM válidos da tabela de lookup...")

lookup_ncm <- ncm_lookup |>
  mutate(
    ncm_len = nchar(co_ncm),
    co_ncm = as.character(co_ncm)
  ) |>
  filter(ncm_len %in% c(2, 4, 5, 6, 8)) |>
  distinct(co_ncm, .keep_all = TRUE)

base_dt <- as.data.table(base_detalhada)
lookup_dt <- as.data.table(lookup_ncm)
pais_dt <- as.data.table(pais_lookup)

lista_agregados <- lapply(sort(unique(lookup_dt$ncm_len)), function(lvl) {
  message("Processando nível NCM com ", lvl, " dígitos...")
  
  lookup_lvl <- lookup_dt[ncm_len == lvl, .(co_ncm, ds_ncm)]
  if (nrow(lookup_lvl) == 0) return(NULL)
  
  # Mantém somente os códigos do lookup como alvo
  base_lvl <- copy(base_dt)
  base_lvl[, co_ncm_alvo := substr(co_ncm, 1, lvl)]
  
  # Faz o join apenas com códigos válidos da tabela NCM
  base_lvl <- merge(
    base_lvl,
    lookup_lvl,
    by.x = "co_ncm_alvo",
    by.y = "co_ncm",
    all = FALSE,
    sort = FALSE
  )
  
  setnames(base_lvl, "co_ncm_alvo", "co_ncm")
  
  agg <- base_lvl[, .(
    vl_fob = sum(vl_fob, na.rm = TRUE),
    kg_liquido = sum(kg_liquido, na.rm = TRUE)
  ), by = .(co_ano, co_pais, co_ncm, ds_ncm)]
  
  agg <- merge(
    agg,
    pais_dt,
    by = "co_pais",
    all.x = TRUE,
    sort = FALSE
  )
  
  agg[, pais := ifelse(is.na(pais), co_pais, pais)]
  agg[, ncm_label := paste0(co_ncm, " - ", ds_ncm)]
  
  as.data.frame(agg)
})

base_agregada <- bind_rows(lista_agregados) |>
  mutate(
    co_ncm = as.character(co_ncm),
    co_pais = as.character(co_pais),
    pais = if_else(is.na(pais), co_pais, pais),
    ds_ncm = if_else(is.na(ds_ncm), "Descrição não encontrada", ds_ncm),
    ncm_label = paste0(co_ncm, " - ", ds_ncm)
  ) |>
  arrange(co_ncm, co_ano, co_pais)

message("Salvando arquivo RDS: ", arquivo_saida)
saveRDS(base_agregada, arquivo_saida)

message("Pronto. Arquivo salvo em: ", normalizePath(arquivo_saida))