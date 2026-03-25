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
# Criando o objeto espacial sem repetições de VALUE
dados_smp_sf <- dados_smp_final |> 
  # 1. Seleciona apenas o que importa para o mapa
  dplyr::select(VALUE, LONGITUDE, LATITUDE) |> 
  # 2. Remove linhas onde o VALUE e as coordenadas sejam idênticos
  dplyr::distinct(VALUE, LONGITUDE, LATITUDE, .keep_all = TRUE) |> 
  # 3. Transforma em SF (SAD69/SIRGAS 2000 conforme seu CRS 4674)
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4674, remove = FALSE)

# Remover coordenadas apenas do CSV final
dados_smp_final_csv <- dados_smp_final |> select(-LONGITUDE, -LATITUDE)
# --- 7. Consolidação Final por NUM_LAB ---

# Identificamos as colunas que NÃO são analitos (metadados)
meta_cols <- c("VALUE", "COD", "ESTACAO", "C.C", "PROJETO", "NUM_LAB", "CLASSE", "LOTE", "BOLETIM")

# Identificamos as colunas de ANALITOS (as que restaram)
analito_cols <- setdiff(colnames(dados_smp_final_csv), c(meta_cols, "METODO"))

dados_smp_final_csv <- dados_smp_final_csv |>
  # 1. Removemos a coluna METODO conforme solicitado
  dplyr::select(-any_of("METODO")) |>
  # 2. Agrupamos pelos metadados principais
  dplyr::group_by(across(any_of(meta_cols))) |>
  # 3. Para cada analito, pegamos o valor que não seja NA
  # Se houver mais de um valor, o max() garante que pegamos o dado analítico
  dplyr::summarise(
    across(all_of(analito_cols), ~ if(all(is.na(.))) NA_real_ else max(., na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # 4. Reorganiza as colunas para manter o padrão original
  dplyr::relocate(all_of(meta_cols))
# --- 8. Consolidação Final de Duplicatas de Campo ---

# Definimos os metadados específicos para as duplicatas
meta_cols_dup <- c("VALUE", "COD", "ESTACAO", "C.C", "PROJETO", "NUM_LAB", "CLASSE", "LOTE", "BOLETIM", "NUM_CAMPO")

# Identificamos as colunas de ANALITOS dinamicamente
analito_cols_dup <- setdiff(colnames(dup_campo), c(meta_cols_dup, "METODO", "LONGITUDE", "LATITUDE"))

dup_campo_final_csv <- dup_campo |>
  # 1. Removemos as coordenadas e o método para o CSV
  dplyr::select(-any_of(c("METODO", "LONGITUDE", "LATITUDE"))) |>
  # 2. Agrupamos pelos identificadores da amostra
  dplyr::group_by(across(any_of(meta_cols_dup))) |>
  # 3. Consolidação: busca o primeiro valor que não seja NA para cada analito
  dplyr::summarise(
    across(all_of(analito_cols_dup), ~ {
      val_limpo <- .[!is.na(.)]
      if (length(val_limpo) == 0) NA_real_ else val_limpo[1]
    }),
    .groups = "drop"
  ) |>
  # 4. Organização estética
  dplyr::relocate(all_of(meta_cols_dup))

# Opcional: Filtro para garantir que não exportamos linhas sem nenhum resultado analítico
dup_campo_final_csv <- dup_campo_final_csv |>
  dplyr::filter(!if_all(all_of(analito_cols_dup), is.na))

# --- 7. Escrita de Arquivos ---
caminho_subpasta <- dir_out
if (!dir.exists(caminho_subpasta)) dir.create(caminho_subpasta, recursive = TRUE)

write.csv2(dados_smp_final_csv, file.path(caminho_subpasta, "mydata.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv2(dup_campo_final_csv , file.path(caminho_subpasta, "duplicatas_campo.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv2(ca, file.path(caminho_subpasta, "myjob.csv"), fileEncoding = "latin1", row.names = FALSE)

# Shapefile com tratamento de erro
arquivo_shp <- file.path(caminho_subpasta, "myoutlet.shp")
if (file.exists(arquivo_shp)) {
  file.remove(list.files(caminho_subpasta, pattern = "myoutlet", full.names = TRUE))
}
sf::st_write(dados_smp_sf, dsn = arquivo_shp, driver = "ESRI Shapefile", quiet = TRUE)

out <- list(dados_smp_final_csv, dados_smp_sf, dup_campo_final_csv, ca)
names(out) <- c("amostras e resultados analíticos", "estações das amostras analisadas", "duplicatas de campo", "condições analíticas")

  return(out)
}
