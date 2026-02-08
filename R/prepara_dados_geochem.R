prepara_dados_geochem <- function(dir_out, info_os, ca, dados_pivo, classe_am) {
  library(dplyr)
  library(tidyr)

  out <- list()

  # 1. Padronização de nomes
  colnames(dados_pivo) <- gsub(
    "metodo",
    "METODO",
    colnames(dados_pivo),
    fixed = TRUE
  )
  colnames(dados_pivo) <- gsub(
    "Boletim",
    "BOLETIM",
    colnames(dados_pivo),
    fixed = TRUE
  )
  colnames(ca) <- gsub("metodo", "METODO", colnames(ca), fixed = TRUE)
  colnames(ca) <- gsub("Boletim", "BOLETIM", colnames(ca), fixed = TRUE)

  # 2. Pivotagem e Limpeza
  dados_pivo <- dados_pivo |>
    dplyr::select(-any_of("unidade")) |>
    dplyr::select(NUM_LAB, CLASSE, METODO, BOLETIM, analito, valor)

  metodos <- unique(ca$METODO)
  lista_dados <- list()

  for (i in seq_along(metodos)) {
    dados_filtro <- dados_pivo |> dplyr::filter(METODO == metodos[i])
    if (nrow(dados_filtro) > 0) {
      lista_dados[[i]] <- dados_filtro |>
        tidyr::pivot_wider(
          names_from = "analito",
          values_from = "valor",
          names_sort = TRUE
        )
    }
  }

  dados <- do.call(dplyr::bind_rows, lista_dados)
  dados <- dados[!is.na(dados$METODO), ]

  info_os <- info_os |>
    dplyr::select(LONGITUDE, LATITUDE, NUM_CAMPO, LOTE, NUM_LAB, C.C, PROJETO)

  dados <- dplyr::left_join(info_os, dados, by = "NUM_LAB")

  cod_classes <- c("B", "S", "R", "L", "A")
  sufixo <- paste0("-", cod_classes[classe_am], "-")

  # 4. Processamento de Duplicatas (Onde estava o erro do NA)
  dup_campo <- dados |>
    group_by(LONGITUDE, LATITUDE) |>
    filter(n_distinct(NUM_CAMPO) > 1) |>
    arrange(LONGITUDE, LATITUDE, NUM_CAMPO) |>
    mutate(COD = if_else(row_number() == 1, "SMP", "DUP")) |>
    ungroup() |>
    mutate(ESTACAO = gsub(sufixo, "-", NUM_CAMPO, fixed = TRUE))

  
    # Removendo sufixos de comprimento extra se houver
    dup_campo <- dup_campo |>
      mutate(
        ESTACAO = if_else(nchar(ESTACAO) == 13, substr(ESTACAO, 1, 12), ESTACAO)
      )

    # IMPORTANTE: Remover VALUE antigo antes do join para evitar VALUE.x e VALUE.y
    dup_campo <- dup_campo |> select(-any_of("VALUE"))

    # 3. Criação de dados_smp (Base de referência para os IDs)
    if (nrow(dup_campo) > 0) {
      dados_smp <- dados |>
        arrange(NUM_CAMPO) |>
        distinct(LONGITUDE, LATITUDE, .keep_all = TRUE) |>
        mutate(
          COD = "SMP",
          VALUE = row_number(),
          # Garante que a ESTACAO seja limpa para o Join
          ESTACAO = gsub(sufixo, "-", NUM_CAMPO, fixed = TRUE)
        )
    } else {
      dados_smp <- dados |>
        arrange(NUM_CAMPO) |>
        mutate(
          COD = "SMP",
          VALUE = row_number(),
          # Garante que a ESTACAO seja limpa para o Join
          ESTACAO = gsub(sufixo, "-", NUM_CAMPO, fixed = TRUE)
        )
    }

    # Join para trazer o VALUE correto da amostra SMP correspondente
    # Usamos distinct em dados_smp para garantir que a chave seja única
    tabela_chaves <- dados_smp |>
      select(VALUE, ESTACAO) |>
      distinct(ESTACAO, .keep_all = TRUE)

    dup_campo <- dup_campo |>
      left_join(tabela_chaves, by = "ESTACAO") |>
      # Se ainda restarem NAs por erro de digitação na ESTACAO, o fill resolve
      group_by(LONGITUDE, LATITUDE) |>
      fill(VALUE, .direction = "updown") |>
      ungroup()

    dup_campo <- dup_campo |>
      relocate(
        VALUE,
        COD,
        ESTACAO,
        LONGITUDE,
        LATITUDE,
        C.C,
        PROJETO,
        NUM_LAB,
        CLASSE,
        METODO,
        LOTE,
        BOLETIM
      )


  # 5. Finalização e Exportação
  dados_smp_sf <- sf::st_as_sf(
    dados_smp,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4674,
    remove = FALSE
  ) |>
    select(VALUE, LONGITUDE, LATITUDE)

  dados_smp_final <- dados_smp |>
    relocate(
      VALUE,
      COD,
      ESTACAO,
      LONGITUDE,
      LATITUDE,
      C.C,
      PROJETO,
      NUM_LAB,
      CLASSE,
      METODO,
      LOTE,
      BOLETIM
    ) |>
    select(-LONGITUDE, -LATITUDE)

  selcol <- colnames(dados_smp_final)[12:ncol(dados_smp_final)]
  dados_smp_final <- dados_smp_final |> filter(!if_all(all_of(selcol), is.na))

  # Escrita dos arquivos
  caminho_subpasta <- file.path(dir_out)
  if (!dir.exists(caminho_subpasta)) {
    dir.create(caminho_subpasta, recursive = TRUE)
  }

  write.csv2(
    dados_smp_final,
    file.path(caminho_subpasta, "mydata.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  # sf::write_sf(
  #   dados_smp_sf,
  #   file.path(caminho_subpasta, "myoutlet.shp"),
  #   delete_dsn = TRUE
  # )
arquivo_shp <- file.path(caminho_subpasta, "myoutlet.shp")

# 1. Forçar a limpeza de qualquer tentativa anterior (evita conflito de acesso)
if (file.exists(arquivo_shp)) {
  file.remove(list.files(caminho_subpasta, pattern = "myoutlet", full.names = TRUE))
}
  suppressMessages(suppressWarnings({
  sf::st_write(
    obj = dados_smp_sf, 
    dsn = arquivo_shp, 
    driver = "ESRI Shapefile", 
    delete_dsn = TRUE, 
    quiet = TRUE  # O GDAL ignora avisos internos com isso
  )
})
  )
  write.csv2(
    dup_campo,
    file.path(caminho_subpasta, "duplicatas_campo.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  write.csv2(
    ca,
    file.path(caminho_subpasta, "myjob.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )

  out <- list(dados_smp_final, dados_smp_sf, dup_campo, ca)
  names(out) <- c(
    "amostras e resultados analíticos",
    "estações das amostras analisadas",
    "duplicatas de campo",
    "condições analíticas"
  )
  return(out)
}
