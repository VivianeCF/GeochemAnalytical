prepara_dados_geochem <- function(dir_out, info_os, ca, dados_pivo) {
  out <- list()
  
# dados_pivo <- read.csv2("outputs/dados_transformados_pivotados.csv", fileEncoding = "latin1")
colnames(dados_pivo) <- gsub("metodo", "METODO", colnames(dados_pivo), fixed = TRUE)
colnames(dados_pivo) <- gsub("Boletim", "BOLETIM", colnames(dados_pivo), fixed = TRUE)
# info_os <- read.csv2("outputs/dados_os.csv", fileEncoding = "latin1")

  
# ca <- read.csv2("outputs/myjob.csv", fileEncoding = "latin1")
  

  dados_pivo <- dados_pivo |> 
  dplyr::select(-unidade) |>
  dplyr::select(NUM_LAB, METODO, BOLETIM, analito, valor)

  metodos <- unique(ca$METODO)

  lista_dados <- list()

  for(i in seq(metodos)){

    dados_filtro <- dados_pivo |> dplyr::filter(METODO == metodos[i])

    lista_dados[[i]] <- dados_filtro |> 
    tidyr::pivot_wider(
      names_from = "analito",
      values_from = "valor",
      names_sort = TRUE
    )
      }

  dados <- do.call(dplyr::bind_rows, lista_dados)

  info_os <- info_os |>
    dplyr::select(LONGITUDE, LATITUDE, NUM_CAMPO, LOTE, NUM_LAB, C.C, PROJETO)
  dados <- dplyr::left_join(info_os, dados, by = "NUM_LAB")

  dup_campo <- dados |>
    dplyr::group_by(LONGITUDE, LATITUDE) |>
    dplyr::filter(n() > 1) 
  
  dup_campo$COD <- rep(c("SMP", "DUP"), nrow(dup_campo) / 2)

  dup_campo <- dup_campo |> dplyr::relocate(COD, .after = NUM_CAMPO)

  # dup_campo <- dup_campo |> dplyr::select(!c( C.C, PROJETO, LONGITUDE, LATITUDE))
  
  dados_smp <- dados |>
    dplyr::arrange(NUM_CAMPO) |>
    dplyr::distinct(LONGITUDE, LATITUDE, .keep_all = TRUE)
  
  dados_smp$COD <- "SMP"
  dados_smp$VALUE <- 1:nrow(dados_smp)

  dados_smp$ESTACAO <- gsub("-S-", "-", dados_smp$NUM_CAMPO, fixed = TRUE)
  # dados_smp <- dados_smp |> dplyr::rename(ESTACAO = NUM_CAMPO)
  dados_smp <- dados_smp |> dplyr::relocate(VALUE,  COD, ESTACAO, LONGITUDE, LATITUDE, C.C, PROJETO,  NUM_LAB, METODO, LOTE, BOLETIM)


  dados_smp_sf <- sf::st_as_sf(
    dados_smp,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4674,
    remove = FALSE
  )
  dados_smp_sf <- dados_smp_sf |> dplyr::select(VALUE, LONGITUDE, LATITUDE)
  # dados_smp <- dados_smp |> dplyr::select(!c(COD, LONGITUDE, LATITUDE))
  dup_campo$ESTACAO <- gsub("-S-", "-", dup_campo$NUM_CAMPO, fixed = TRUE)

  dup_campo <- dup_campo |>
  dplyr::mutate(ESTACAO = dplyr::if_else(stringr::str_length(ESTACAO) == 13, 
                           stringr::str_sub(ESTACAO, 1, -2), 
                           ESTACAO))
  
  dup_campo <- dplyr::left_join(dup_campo,dados_smp[, c("VALUE", "ESTACAO")],  by= "ESTACAO")
  dup_campo <- dup_campo |> dplyr::relocate(VALUE,  COD, ESTACAO, LONGITUDE, LATITUDE, C.C, PROJETO,  NUM_LAB, METODO, LOTE, BOLETIM)
  dados_smp <- dados_smp |> dplyr::select(-LONGITUDE, -LATITUDE)
  out[[1]] <- dados_smp # dados amostrados
  out[[2]] <- dados_smp_sf # estações com dados amostrados
  out[[3]] <- dup_campo # duplicatas de campo
  out[[4]] <- ca # condições analíticas

  caminho_subpasta <- file.path(dir_out)
  if (!dir.exists(caminho_subpasta)) {
    dir.create(caminho_subpasta, recursive = TRUE, showWarnings = FALSE)
  }
  colnames(dados_smp) <- gsub("metodo", "METODO", colnames(dados_smp), fixed = TRUE)
  write.csv2(
    dados_smp,
    file.path(caminho_subpasta, "mydata.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  
  sf::write_sf(
    dados_smp_sf,
    file.path(caminho_subpasta, "myoutlet.shp"),
    fileEncoding = "latin1",
    row.names = FALSE
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

  names(out) <- c(
    "amostras e resultados analíticos",
    "estações das amostras analisadas",
    "duplicatas de campo",
    "condições analíticas"
  )
  return(out)
}
