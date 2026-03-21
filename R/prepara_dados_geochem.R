prepara_dados_geochem <- function(dir_out, info_os, ca, dados_pivo, classe_am) {
library(tidyverse)

  out <- list()

library(tidyverse)

# --- 1. Padronização Robusta de Nomes ---
# Forçamos as colunas essenciais para MAIÚSCULO antes de começar
# Força TODAS as colunas para maiúsculo de uma vez
colnames(dados_pivo) <- toupper(colnames(dados_pivo))
colnames(ca)         <- toupper(colnames(ca))
colnames(info_os)    <- toupper(colnames(info_os))


# --- 2. Pivotagem (Resolvendo o erro de agrupamento) ---
dados_processados <- dados_pivo |> dplyr::filter(ANALITO %in% ca$EL)|>
  # Remove unidade se existir, mas mantém as outras
  dplyr::select(-any_of("unidade")) |>
  # Filtra apenas se a coluna METODO existir
  dplyr::filter(!is.na(METODO)) |>
  # Agrupamento explícito garantindo que as colunas existem
  group_by(across(any_of(c("NUM_LAB", "CLASSE", "METODO", "BOLETIM", "ANALITO")))) |>
  summarise(VALOR = max(VALOR, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = "ANALITO",
    values_from = "VALOR",
    names_sort = TRUE
  ) 

# --- 3. Join e Padronização de Strings ---
info_os_clean <- info_os |>
  dplyr::select(VALUE, LONGITUDE, LATITUDE, NUM_CAMPO, LOTE, NUM_LAB, C.C, PROJETO)

dados <- dplyr::left_join(info_os_clean, dados_processados, by = "NUM_LAB") |>
  mutate(
    # Padronização 4 dígitos
    NUM_CAMPO = str_replace(NUM_CAMPO, "(\\d+)([A-Z]?)$", function(m) {
      num <- str_extract(m, "\\d+")
      letra <- str_extract(m, "[A-Z]+") |> replace_na("")
      paste0(str_pad(num, 4, pad = "0"), letra)
    }),
    # Sufixos -A, -D, -DUP viram A
    NUM_CAMPO = str_replace(NUM_CAMPO, "-(A|DUP|D)$", "A"),
    # Cria ESTACAO
    ESTACAO = NUM_CAMPO |> 
      str_replace_all("-S-", "-") |> 
      str_replace("A$", "")
  )

# --- 4. Processamento de Duplicatas e SMP ---
# Criamos o SMP primeiro para servir de base para o VALUE das duplicatas
dados_smp <- dados |>
  arrange(NUM_CAMPO) |>
  distinct(LONGITUDE, LATITUDE, METODO, .keep_all = TRUE) |>
  mutate(COD = "SMP")

dup_campo <- dados |>
  group_by(LONGITUDE, LATITUDE, METODO) |>
  filter(n_distinct(NUM_CAMPO) > 1) |>
  arrange(LONGITUDE, LATITUDE, NUM_CAMPO) |>
  mutate(COD = if_else(row_number() == 1, "SMP", "DUP")) |>
  ungroup() |>
  select(-any_of("VALUE"))

# --- 5. Recuperação de VALUE para Duplicatas ---
tabela_chaves <- dados_smp |>
  select(VALUE, ESTACAO) |>
  distinct(ESTACAO, .keep_all = TRUE)

dup_campo <- dup_campo |>
  left_join(tabela_chaves, by = "ESTACAO") |>
  group_by(LONGITUDE, LATITUDE, METODO) |>
  fill(VALUE, .direction = "updown") |>
  ungroup()

# --- 6. Preparação para Exportação ---
dados_smp_final <- dados_smp |>
  relocate(VALUE, COD, ESTACAO, LONGITUDE, LATITUDE, C.C, PROJETO, NUM_LAB, CLASSE, METODO, LOTE, BOLETIM)

# Filtro de linhas vazias (Analitos começam após a coluna 12)
selcol <- colnames(dados_smp_final)[13:ncol(dados_smp_final)]
dados_smp_final <- dados_smp_final |> 
  filter(!if_all(all_of(selcol), is.na))

# Criar objeto SF antes de remover lat/long
dados_smp_sf <- sf::st_as_sf(dados_smp, coords = c("LONGITUDE", "LATITUDE"), crs = 4674, remove = FALSE)

# Remover coordenadas apenas do CSV final
dados_smp_final_csv <- dados_smp_final |> select(-LONGITUDE, -LATITUDE)

# --- 7. Escrita de Arquivos ---
caminho_subpasta <- dir_out
if (!dir.exists(caminho_subpasta)) dir.create(caminho_subpasta, recursive = TRUE)

write.csv2(dados_smp_final_csv, file.path(caminho_subpasta, "mydata.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv2(dup_campo, file.path(caminho_subpasta, "duplicatas_campo.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv2(ca, file.path(caminho_subpasta, "myjob.csv"), fileEncoding = "latin1", row.names = FALSE)

# Shapefile com tratamento de erro
arquivo_shp <- file.path(caminho_subpasta, "myoutlet.shp")
if (file.exists(arquivo_shp)) {
  file.remove(list.files(caminho_subpasta, pattern = "myoutlet", full.names = TRUE))
}
sf::st_write(dados_smp_sf, dsn = arquivo_shp, driver = "ESRI Shapefile", quiet = TRUE)

out <- list(dados_smp_final, dados_smp_sf, dup_campo, ca)
names(out) <- c("amostras e resultados analíticos", "estações das amostras analisadas", "duplicatas de campo", "condições analíticas")

  return(out)
}
