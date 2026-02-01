for (i in 1:length(list_bol)) {
  df_tudo = readxl::read_excel(list_bol[i], col_names = FALSE)
  colnames(df_tudo) <- paste0("V", 1:ncol(df_tudo))
  # 2. Defina o valor alvo
  tipo_bol <- "RESULTADOS DE ANÁLISE MINERALOMÉTRICA"
  info_ra <- "INFORMAÇÕES DO CONSIGNMENT"
  info_cliente <- "INFORMAÇÃO DO CLIENTE"
  tab_quant <- "TABELA QUANTITATIVA"
  tab_semiquant <- "TABELA SEMIQUANTITATIVA"
  nota <- "NOTA"
  legenda <- "LEGENDA"
  data <- "DATA"
  prep_am <- "Preparação da Amostra (g)"
  n_lab <- "Número de Laboratório"
  seq <- "Sequência"

  # 3. Use which() com arr.ind = TRUE
  indices_tipo_bol <- which(df_tudo == tipo_bol, arr.ind = TRUE)
  indices_info_ra <- which(df_tudo == info_ra, arr.ind = TRUE)
  indices_info_cliente <- which(df_tudo == info_cliente, arr.ind = TRUE)
  indices_tab_quant <- which(df_tudo == tab_quant, arr.ind = TRUE)
  indices_tab_semiquant <- which(df_tudo == tab_semiquant, arr.ind = TRUE)
  indices_nota <- which(df_tudo == nota, arr.ind = TRUE)
  indices_legenda <- which(df_tudo == legenda, arr.ind = TRUE)
  indices_data <- which(df_tudo == data, arr.ind = TRUE)
  indices_prep_am <- which(df_tudo == prep_am, arr.ind = TRUE)
  indices_n_lab <- which(df_tudo == n_lab, arr.ind = TRUE)
  indices_seq <- which(df_tudo == seq, arr.ind = TRUE)

  n <- ncol(df_tudo)
  r <- nrow(df_tudo)

  laboratorio_unidade <- df_tudo[indices_tipo_bol[1] - 2, indices_tipo_bol[2]]
  laboratorio <- df_tudo[indices_tipo_bol[1] - 6, indices_tipo_bol[2]]
  tipo_analise <- df_tudo[indices_tipo_bol[1], indices_tipo_bol[2]]
  n_job <- df_tudo[indices_tipo_bol[1] + 1, indices_tipo_bol[2] + 12]

  cliente_nome <- df_tudo[indices_info_cliente[1] + 2, indices_info_cliente[2]]
  cliente_contato <- df_tudo[
    indices_info_cliente[1] + 2,
    indices_info_cliente[2] + 11
  ]
  cliente_endereco <- df_tudo[
    indices_info_cliente[1] + 5,
    indices_info_cliente[2]
  ]
  cliente_cep <- df_tudo[
    indices_info_cliente[1] + 5,
    indices_info_cliente[2] + 9
  ]
  cliente_pais <- df_tudo[
    indices_info_cliente[1] + 5,
    indices_info_cliente[2] + 13
  ]

  data_recebido <- df_tudo[
    indices_data[1] + 2,
    indices_data[2]
  ]

data_recebido <- lubridate::as_date(as.numeric(data_recebido), origin = "1899-12-30")
data_recebido <- format(data_recebido, "%d/%m/%Y")
  
  data_fin <- df_tudo[
    indices_data[1] + 5,
    indices_data[2]
  ]
data_fin <- lubridate::as_date(as.numeric(data_fin), origin = "1899-12-30")
data_fin <- format(data_fin, "%d/%m/%Y")

  data_ini <- df_tudo[
    indices_data[1] + 2,
    indices_data[2] + 6
  ]
data_ini <- lubridate::as_date(as.numeric(data_ini), origin = "1899-12-30")
data_ini <- format(data_ini, "%d/%m/%Y")

  data_env <- df_tudo[
    indices_data[1] + 5,
    indices_data[2] + 6
  ]
data_env <- lubridate::as_date(as.numeric(data_env), origin = "1899-12-30")
data_env <- format(data_env, "%d/%m/%Y")

  no_amostras <- df_tudo[indices_info_ra[1] + 2, indices_info_ra[2] + 5]
  contrato <- df_tudo[indices_info_ra[1] + 2, indices_info_ra[2] + 10]
  ne <- df_tudo[indices_info_ra[1] + 2, indices_info_ra[2] + 13]
  projeto <- df_tudo[indices_info_ra[1] + 5, indices_info_ra[2] + 5]
  lote <- df_tudo[indices_info_ra[1] + 5, indices_info_ra[2] + 10]
  ra <- df_tudo[indices_info_ra[1] + 5, indices_info_ra[2] + 12]
  cc <- df_tudo[indices_info_ra[1] + 5, indices_info_ra[2] + 16]

  metodo <- as.character(t(df_tudo[
    indices_prep_am[1],
    (indices_prep_am[2]):n
  ]))
  analito <- as.character(t(df_tudo[
    indices_prep_am[1] + 1,
    (indices_prep_am[2]):n
  ]))

  unidades <- tolower(t(df_tudo[
    indices_prep_am[1] + 3,
    (indices_prep_am[2]):n
  ]))

  unidades[is.na(unidades)] <- "g"

  boletim <- df_tudo[
    (indices_n_lab[1] + 4):r,
    (indices_n_lab[2]):n
  ]

  colnames(boletim) <- c("NUM_LAB", analito)
  boletim <- boletim[!is.na(boletim$NUM_LAB), ]

  job_boletim <- rep(as.character(n_job), nrow(boletim))
  boletim <- cbind(boletim, job_boletim)
  colnames(boletim)[ncol(boletim)] <- "Boletim"
  # Cria tabela das condições analíticas
  LAB <- rep(as.character(laboratorio), length(metodo))
  condicoes_analiticas <-
    data.frame(metodo, analito, unidades, job_boletim, LAB)

  # Preenchendo valores de metodo
  condicoes_analiticas <- condicoes_analiticas |>
    tidyr::fill(metodo, .direction = "down")
  condicoes_analiticas$metodo <- gsub(
    " (g)",
    "",
    condicoes_analiticas$metodo,
    fixed = TRUE
  )
  condicoes_analiticas$metodo <- stringr::str_trim(condicoes_analiticas$metodo)
  condicoes_analiticas$analito <- gsub(
    "\n",
    " ",
    condicoes_analiticas$analito,
    fixed = TRUE
  )
  condicoes_analiticas$analito <- gsub(
    "  ",
    " ",
    condicoes_analiticas$analito,
    fixed = TRUE
  )

  var.name <-
    c('metodo', 'analito', 'unidades', 'Boletim', "Laborat\u00f3rio")
  colnames(condicoes_analiticas) <- var.name
  boletim <- boletim |>
    dplyr::select(where(~ !all(is.na(.))))
  analito <- analito[!is.na(analito)]
  unidades <- unidades[!is.na(unidades)]
  metodo <- metodo[!is.na(metodo)]

  # Cria tabela com informações do boletim
  info_boletim <- data.frame(
    
    laboratorio,
    cliente_nome,
    cliente_contato,
    cliente_endereco,
    cliente_cep,
    cliente_pais,
    contrato,
    ne,
    lote,
    n_job,
    no_amostras,
    projeto,
    cc,
    ra,
    tipo_analise,
    data_recebido,
    data_ini,
    data_fin,
    data_env
  )
  colnames(info_boletim) <- c(
    "Laboratório",
    "Nome do cliente",
    "Contato do cliente",
    "Endereço do cliente",
    "CEP do cliente",
    "Cliente país",
    "Contrato",
    "Nota de Empenho",
    "Lote",
    "Boletim",
    "No. de amostras",
    "Projeto",
    "C.C.",
    "RA",
    "Tipo de análise",
    "Recebido",
    "Início",
    "Finalizado",
    "Enviado"
  )


  # Função aprimorada para não gerar avisos e limpar espaços
  limpar_numeros_texto <- function(x) {
    # Remove espaços em branco extras que podem vir do Excel
    x <- trimws(x)

    # suppressWarnings evita a mensagem "NAs introduced by coercion"
    # Substituímos a vírgula por ponto apenas para o teste numérico
    num_val <- suppressWarnings(as.numeric(gsub(",", ".", x)))

    # Se for número, formata. Se não, retorna o texto original x
    ifelse(
      !is.na(num_val),
      format(
        round(num_val, 3),
        decimal.mark = ",",
        scientific = FALSE,
        drop0trailing = TRUE
      ),
      x
    )
  }
  colnames(boletim) <- gsub("\n", " ", colnames(boletim), fixed = TRUE)
  colnames(boletim) <- gsub("  ", " ", colnames(boletim), fixed = TRUE)
  
    write.csv2(
    info_boletim,
    file.path(caminho_subpasta, "informações_boletins.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )

    write.csv2(
    condicoes_analiticas,
    file.path(caminho_subpasta, "condicoes_analiticas.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )

    write.csv2(
    boletim,
    file.path(caminho_subpasta, "dados_brutos.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )


  # Adiciona às listas
  out[[1]] <- boletim  
  out[[2]] <- condicoes_analiticas
  out[[3]] <- info_boletim
  
    names(out) <- c(
    "dados brutos",
    "condições de análise",
    "informações dos boletins"
  )
}
