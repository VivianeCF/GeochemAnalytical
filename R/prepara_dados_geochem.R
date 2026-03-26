prepara_dados_geochem <- function(dir_out, info_os, ca, dados_pivo, classe_am) {
  library(tidyverse)
  out <- list()

  # --- 1. Padronização Robusta de Nomes ---
  colnames(dados_pivo) <- toupper(colnames(dados_pivo))
  colnames(ca)         <- toupper(colnames(ca))
  colnames(info_os)    <- toupper(colnames(info_os))

  # --- 2. Pivotagem ---
  dados_processados <- dados_pivo |> 
    dplyr::filter(ANALITO %in% ca$EL) |>
    dplyr::select(-any_of("UNIDADE")) |>
    dplyr::filter(!is.na(METODO)) |>
    group_by(across(any_of(c("NUM_LAB", "CLASSE", "METODO", "BOLETIM", "ANALITO")))) |>
    summarise(VALOR = max(VALOR, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "ANALITO", values_from = "VALOR", names_sort = TRUE) 

  # --- 3. Join e Padronização ---
  info_os_clean <- info_os |>
    dplyr::select(LONGITUDE, LATITUDE, NUM_CAMPO, LOTE, NUM_LAB, C.C, PROJETO)

  dados <- dplyr::left_join(info_os_clean, dados_processados, by = "NUM_LAB") |>
    mutate(
      NUM_CAMPO = str_replace(NUM_CAMPO, "(\\d+)([A-Z]?)$", function(m) {
        num <- str_extract(m, "\\d+")
        letra <- str_extract(m, "[A-Z]+") |> replace_na("")
        paste0(str_pad(num, 4, pad = "0"), letra)
      }),
      NUM_CAMPO = str_replace(NUM_CAMPO, "-(A|DUP|D)$", "A"),
      ESTACAO = NUM_CAMPO |> str_replace_all("-S-", "-") |> str_replace("A$", "")
    )

  # --- 4. Processamento de Duplicatas e SMP ---
  dados_smp <- dados |>
    arrange(NUM_CAMPO) |>
    distinct(LONGITUDE, LATITUDE, METODO, .keep_all = TRUE) |>
    mutate(COD = "SMP")

  dup_campo <- dados |>
    group_by(LONGITUDE, LATITUDE, METODO) |>
    filter(n_distinct(NUM_CAMPO) > 1) |>
    arrange(LONGITUDE, LATITUDE, NUM_CAMPO) |>
    mutate(COD = if_else(row_number() == 1, "SMP", "DUP")) |>
    ungroup() 

  # --- 6. Preparação para Exportação (CORREÇÃO AQUI) ---
  # Criamos o objeto dados_smp_final a partir de dados_smp
  dados_smp_final <- dados_smp |>
    relocate(COD, ESTACAO, LONGITUDE, LATITUDE, C.C, PROJETO, NUM_LAB, CLASSE, METODO, LOTE, BOLETIM)

  meta_fixas <- c("COD", "ESTACAO", "LONGITUDE", "LATITUDE", "C.C", "PROJETO", "NUM_LAB", "CLASSE", "METODO", "LOTE", "BOLETIM")
  selcol <- setdiff(colnames(dados_smp_final), meta_fixas)

  if (length(selcol) > 0) {
    dados_smp_final <- dados_smp_final |> 
      dplyr::filter(!if_all(all_of(selcol), is.na))
  }

  # --- 7. Consolidação Final por NUM_LAB ---
  meta_cols <- c("LONGITUDE", "LATITUDE", "COD", "ESTACAO", "C.C", "PROJETO", "NUM_LAB", "CLASSE", "LOTE", "BOLETIM")
  analito_cols <- setdiff(colnames(dados_smp_final), c(meta_cols, "METODO"))

  if (length(analito_cols) > 0) {
    dados_smp_final_csv <- dados_smp_final |>
      dplyr::select(-any_of("METODO")) |>
      dplyr::group_by(across(any_of(meta_cols))) |>
      dplyr::summarise(
        across(all_of(analito_cols), ~ if(all(is.na(.))) NA_real_ else max(., na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    dados_smp_final_csv <- dados_smp_final |>
      dplyr::select(-any_of("METODO")) |>
      dplyr::distinct(across(any_of(meta_cols)))
  }

  # Criar VALUE para indexação
  dados_smp_final_csv <- dados_smp_final_csv |> dplyr::mutate(VALUE = 1:n()) |> dplyr::relocate(VALUE)

  # --- 8. Objetos Espaciais (SF) ---
  dados_smp_sf <- dados_smp_final_csv |> 
    dplyr::select(VALUE, LONGITUDE, LATITUDE) |> 
    dplyr::distinct(VALUE, LONGITUDE, LATITUDE, .keep_all = TRUE) |> 
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4674, remove = FALSE)

  # Limpeza para o CSV final
  dados_smp_final_csv <- dados_smp_final_csv |> dplyr::select(-LONGITUDE, -LATITUDE)

  # --- 9. Duplicatas de Campo ---
  meta_cols_dup <- c("COD", "ESTACAO", "C.C", "PROJETO", "NUM_LAB", "CLASSE", "LOTE", "BOLETIM", "NUM_CAMPO")
  analito_cols_dup <- setdiff(colnames(dup_campo), c(meta_cols_dup, "METODO", "LONGITUDE", "LATITUDE"))

  dup_campo_final_csv <- dup_campo |>
    dplyr::select(-any_of(c("METODO", "LONGITUDE", "LATITUDE"))) |>
    dplyr::group_by(across(any_of(meta_cols_dup))) |>
    dplyr::summarise(
      across(all_of(analito_cols_dup), ~ {
        val_limpo <- .[!is.na(.)]
        if (length(val_limpo) == 0) NA_real_ else val_limpo[1]
      }),
      .groups = "drop"
    )

  if (length(analito_cols_dup) > 0) {
    dup_campo_final_csv <- dup_campo_final_csv |> dplyr::filter(!if_all(all_of(analito_cols_dup), is.na))
  }

  # --- 10. Escrita e Retorno ---
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  write.csv2(dados_smp_final_csv, file.path(dir_out, "mydata.csv"), fileEncoding = "latin1", row.names = FALSE)
  write.csv2(dup_campo_final_csv , file.path(dir_out, "duplicatas_campo.csv"), fileEncoding = "latin1", row.names = FALSE)
  write.csv2(ca, file.path(dir_out, "myjob.csv"), fileEncoding = "latin1", row.names = FALSE)

  arquivo_shp <- file.path(dir_out, "myoutlet.shp")
  if (file.exists(arquivo_shp)) file.remove(list.files(dir_out, pattern = "myoutlet", full.names = TRUE))
  sf::st_write(dados_smp_sf, dsn = arquivo_shp, driver = "ESRI Shapefile", quiet = TRUE)

  out <- list(dados_smp_final_csv, dados_smp_sf, dup_campo_final_csv, ca)
  names(out) <- c("amostras e resultados analíticos", "estações das amostras analisadas", "duplicatas de campo", "condições analíticas")
  return(out)
}