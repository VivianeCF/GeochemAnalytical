#' Lê boletins de análises químicas
#'
#' Esta função lê os boletins csv e extrai as informações na forma de tabelas
#' que são gravadas no diretório de saída.
#'
#' @param classe_am Classe da amostra: 1 = concentrado de bateia, 2 = sedimento de
#'   corrente, 3 = rocha, 4 = solo, 5 = água
#' @param dir_bol Diretório dos boletins analíticos ex: "inputs/quimica/R/"
#' @param ref_ucc Planilha com valores da Concentração Média da Crosta Superior
#' Rudnick e Gao 2004
#' @param dir_ucc Diretório do arquivo UCC
#'
#' @return Retorna uma lista com todos os dados do boletim: resultados
#'   analíticos, condições analíticas, QA/QC, requisição das análises e
#'   preparação das amostras.
#' @export
#' @examples
#' # le_boletim_quimica()
le_boletim_quimica_acme <- function(
  classe_am,
  dir_bol,
  dir_ucc,
  ref_ucc,
  dir_out
) {
  source("R/ltdl.fix.df.R")
  ## Diretórios de entrada dos dados
  classes <-
    c(
      "Concentrado de bateia",
      "Sedimento de corrente",
      "Rocha",
      "Solo",
      "\u00c1gua"
    )
  cod_classes <- c("B", "S", "L", "R", "A")
  nome_bol <-
    c(
      "CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "SOLO",
      "ROCHA",

      "\u00c1GUA"
    )

  ## Gera o camionho para os arquivos
  ## Entrada

  list_bol <- list.files(dir_bol, pattern = "\\.xls$", full.names = TRUE, recursive = TRUE, 
  ignore.case = TRUE)
if (length(list_bol) == 0) {
    stop("Nenhum arquivo .xls encontrado na pasta das boletins.")
  }
  ## Cria listas com cada informação do boletim
  datalist = list()
  datalist2 = list()
  datalist3 = list()
  datalist4 = list()
  datalist5 = list()
  datalist6 = list()
  datalist7 = list()
  out <- list()
  ## Condição para leitura do boletim
  # if (nome_bol[classe_am] == "ROCHA") {
  #   ini = 6
  # } else {
  #   ini = 12
  # }

  ## Ler cada boletim do diretório e extrair as informações
  ## DADOS analíticos
  for (i in 1:length(list_bol)) {
    df_tudo = readxl::read_excel(
      list_bol[i],
      col_names = FALSE,
      sheet = "Analytical Data"
    )

    # 2. Defina o valor alvo
    valor_metodo <- "Method"
    valor_status <- "Final Report"
    valor_cliente <- "Client:"
    valor_amostra <- "Sample"

    # 3. Use which() com arr.ind = TRUE
    indices_metodo <- which(df_tudo == valor_metodo, arr.ind = TRUE)
    indices_status <- which(df_tudo == valor_status, arr.ind = TRUE)
    indices_cliente <- which(df_tudo == valor_cliente, arr.ind = TRUE)
    indices_amostra <- which(df_tudo == valor_amostra, arr.ind = TRUE)

    n <- ncol(df_tudo)
    r <- nrow(df_tudo)
    if (!is.na(indices_status[1])) {
      status <- df_tudo[indices_status[1], indices_status[2]]
    } else {
      status = ""
    }

    laboratorio <- df_tudo[indices_cliente[1] - 1, indices_cliente[2]]

    cliente <- df_tudo[indices_cliente[1], indices_cliente[2] + 1]
    data_criacao_arquivo <- df_tudo[
      indices_cliente[1] + 1,
      indices_cliente[2] + 1
    ]
    n_job <- df_tudo[indices_cliente[1] + 2, indices_cliente[2] + 1]
    no_amostras <- df_tudo[indices_cliente[1] + 3, indices_cliente[2] + 1]
    projeto <- df_tudo[indices_cliente[1] + 4, indices_cliente[2] + 1]
    ship <- df_tudo[indices_cliente[1] + 5, indices_cliente[2] + 1]
    recebido <- df_tudo[indices_cliente[1] + 7, indices_cliente[2] + 1]
    metodo <- t(df_tudo[indices_metodo[1], (indices_metodo[2] + 1):n])
    analito <- t(df_tudo[indices_metodo[1] + 1, (indices_metodo[2] + 1):n])
    analito <- gsub(".", "_", analito, fixed = TRUE)
    unidades <- tolower(t(df_tudo[
      indices_metodo[1] + 2,
      (indices_metodo[2] + 1):n
    ]))
    unidades <- gsub("%", "pct", unidades)
    MDL <- t(df_tudo[indices_metodo[1] + 3, (indices_metodo[2] + 1):n])
    boletim <- df_tudo[(indices_amostra[1] + 1):r, indices_amostra[2]:n]

    job_boletim <- rep(as.character(n_job), nrow(boletim))
    boletim <- cbind(boletim, job_boletim)
    # Cria tabela das condições analíticas
    LAB <- rep(as.character(laboratorio), length(metodo))
    condicoes_analiticas <-
      data.frame(metodo, analito, unidades, MDL, n_job, LAB)
    colnames(condicoes_analiticas)

    var.name <-
      c('metodo', 'analito', 'unidades', 'MDL', 'Boletim', "Laborat\u00f3rio")
    colnames(condicoes_analiticas) <- var.name
    colnames(boletim) <-
      c(
        "NUM_LAB",
        "classe_am",
        paste0(analito, "_", unidades, "_", metodo),
        "Boletim"
      )
    # Cria tabela com informações do boletim
    info_boletim <- data.frame(
      status,
      laboratorio,
      cliente,
      data_criacao_arquivo,
      n_job,
      no_amostras,
      projeto,
      ship,
      recebido
    )
    colnames(info_boletim) <- c(
      "status",
      "laboratorio",
      "cliente",
      "data do arquivo",
      "Boletim",
      "no. de amostras",
      "projeto",
      "ship",
      "entrega dos resultados"
    )
    # Adiciona às listas
    datalist[[i]] <- info_boletim
    datalist2[[i]] <- condicoes_analiticas
    datalist3[[i]] <- boletim
  }

  # DADOS QAQC
  for (i in 1:length(list_bol)) {
    df_tudo = readxl::read_excel(
      list_bol[i],
      col_names = FALSE,
      sheet = "QC Data"
    )
    if (nrow(df_tudo) != 0) {
      # 2. Defina o valor alvo
      valor_metodo <- "Method"
      valor_status <- "Final Report"
      valor_cliente <- "Client:"
      valor_amostra <- "Sample"

      # 3. Use which() com arr.ind = TRUE
      indices_metodo <- which(df_tudo == valor_metodo, arr.ind = TRUE)
      indices_status <- which(df_tudo == valor_status, arr.ind = TRUE)
      indices_cliente <- which(df_tudo == valor_cliente, arr.ind = TRUE)
      indices_amostra <- which(df_tudo == valor_amostra, arr.ind = TRUE)

      n <- ncol(df_tudo)
      r <- nrow(df_tudo)
      if (!is.na(indices_status[1])) {
        status <- df_tudo[indices_status[1], indices_status[2]]
      } else {
        status = ""
      }

      laboratorio <- df_tudo[indices_cliente[1] - 1, indices_cliente[2]]
      cliente <- df_tudo[indices_cliente[1], indices_cliente[2] + 1]
      data_criacao_arquivo <- df_tudo[
        indices_cliente[1] + 1,
        indices_cliente[2] + 1
      ]
      n_job <- df_tudo[indices_cliente[1] + 2, indices_cliente[2] + 1]
      no_amostras <- df_tudo[indices_cliente[1] + 3, indices_cliente[2] + 1]
      projeto <- df_tudo[indices_cliente[1] + 4, indices_cliente[2] + 1]
      ship <- df_tudo[indices_cliente[1] + 5, indices_cliente[2] + 1]
      recebido <- df_tudo[indices_cliente[1] + 7, indices_cliente[2] + 1]

      metodo <- t(df_tudo[indices_metodo[1], (indices_metodo[2] + 1):n])
      metodo <- gsub("1F30", "1F", metodo)
      analito <- t(df_tudo[indices_metodo[1] + 1, (indices_metodo[2] + 1):n])
      analito <- gsub("_", ".", analito)
      unidades <- tolower(t(df_tudo[
        indices_metodo[1] + 2,
        (indices_metodo[2] + 1):n
      ]))
      unidades <- gsub("%", "pct", unidades)
      MDL <- t(df_tudo[indices_metodo[1] + 3, (indices_metodo[2] + 1):n])
      boletim <- df_tudo[(indices_amostra[1] + 1):r, indices_amostra[2]:n]
      job_boletim <- rep(as.character(n_job), nrow(boletim))
      boletim <- cbind(boletim, job_boletim)

      # Cria tabela das condições analíticas
      LAB <- rep(as.character(laboratorio), length(metodo))
      condicoes_analiticas <-
        data.frame(metodo, analito, unidades, MDL, n_job, LAB)
      colnames(condicoes_analiticas)

      var.name <-
        c('metodo', 'analito', 'unidades', 'MDL', 'Boletim', "Laborat\u00f3rio")
      colnames(condicoes_analiticas) <- var.name
      colnames(boletim) <-
        c(
          "NUM_LAB",
          "classe_am",
          paste0(analito, "_", unidades, "_", metodo),
          "Boletim"
        )

      # Cria tabela com informações do boletim
      info_boletim <- data.frame(
        status,
        laboratorio,
        cliente,
        data_criacao_arquivo,
        n_job,
        no_amostras,
        projeto,
        ship,
        recebido
      )
      colnames(info_boletim) <- c(
        "status",
        "laboratorio",
        "cliente",
        "data do arquivo",
        "Boletim",
        "no. de amostras",
        "projeto",
        "ship",
        "data do recebimento"
      )

      library(dplyr)

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

      # 2. Aplicamos às colunas desejadas
      # Substitua 'c(2, 5, 8)' pelos índices ou 'c("Col1", "Col2")' pelos nomes
      boletim <- boletim |>
        dplyr::mutate(dplyr::across(
          paste0(analito, "_", unidades, "_", metodo),
          ~ limpar_numeros_texto(.)
        )) # Exemplo com colunas 1, 2 e 3

      # Adiciona às listas
      datalist4[[i]] <- info_boletim
      datalist5[[i]] <- condicoes_analiticas
      datalist6[[i]] <- boletim
    }
  }

  ib_da = do.call(dplyr::bind_rows, datalist)
  ca_da = do.call(dplyr::bind_rows, datalist2)

  df_da = do.call(dplyr::bind_rows, datalist3)

  ib_q = do.call(dplyr::bind_rows, datalist4)
  ca_q = do.call(dplyr::bind_rows, datalist5)

  df_q = do.call(dplyr::bind_rows, datalist6)

  colnames(df_da) <- gsub("%", "pct", colnames(df_da))

  # Coloca Boletim  no final
  df_da <- df_da |> dplyr::relocate(Boletim, .after = last_col())

  # Padroniza nome de laboratório
  df_da$NUM_LAB <- gsub("-", "", df_da$NUM_LAB)
  df_da$NUM_LAB <- gsub(" ", "", df_da$NUM_LAB)

  # Arruma nomes dos analitos e inserir coluna dos métodos
  lista_metodos <-
    unique(ca_da[, c("analito", "metodo")])$metodo # lista dos métodos com artifícios

  ## Retira linhas sem NUM_LAB
  df_sc <- df_da[!is.na(df_da$NUM_LAB), ]

  ## Substitui valores nulos por NA
  df_sc <- data.frame(lapply(df_sc, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))
  df_sc <- data.frame(lapply(df_sc, function(x) {
    gsub("I,N,F,", NA, x, fixed = TRUE)
  }))
  df_sc$classe_am <- classes[classe_am]
  df_sc <- df_sc |> dplyr::relocate(Boletim, .after = classe_am)
  
  lista_classes <- read.csv2("inputs/lista_classes.csv", fileEncoding = "latin1")
  recode_map <- setNames(lista_classes$novo, lista_classes$original)
  
  # df_sc$classe_am <- gsub("FOSFATO", "ROCHA", df_sc$classe_am, fixed = TRUE)
  df_sc <- df_sc |>
  dplyr::mutate(classe_am = dplyr::recode(classe_am, !!!recode_map))

  ## Pivoteia os dados analíticos
  df_bruto_pivo <- df_sc |>
    tidyr::pivot_longer(
      cols = 4:(ncol(df_sc)),
      names_to = "analito",
      values_to = "valor"
    )

  ## Retira valores com NA
  df_bruto_pivo <- df_bruto_pivo[!is.na(df_bruto_pivo$valor), ]

  df_bruto_pivo <- df_bruto_pivo |>
    # 1. Extração — limpa espaços em branco antes
    dplyr::mutate(
      valor = stringr::str_trim(valor),
      Valor_Num_Char = stringr::str_extract(valor, "^[^\\s]+"),
      Qualificador_Temp = stringr::str_extract(valor, "[<>]")
    ) |>

    # 2. Conversão — suprime warnings de coerção e trata valores inválidos
    dplyr::mutate(
      Valor_Numerico_Convertido = suppressWarnings(as.numeric(Valor_Num_Char))
    ) |>

    # 3. Lógica Condicional (case_when)
    dplyr::mutate(
      # Cria a coluna corrigida com um nome auxiliar para ser renomeada depois
      Valor_Final_Corrigido = dplyr::case_when(
        is.na(Valor_Numerico_Convertido) ~ valor,
        TRUE ~ paste0(
          dplyr::if_else(is.na(Qualificador_Temp), "", Qualificador_Temp),
          as.character(Valor_Numerico_Convertido)
        )
      )
    ) |> # 4. Substituição do Ponto Decimal pela Vírgula
    dplyr::mutate(
      # APLICAR A SUBSTITUIÇÃO: substitui todas as ocorrências de "." por ","
      Valor_Final_Corrigido = stringr::str_replace_all(
        Valor_Final_Corrigido,
        "\\.",
        ","
      )
    ) |>

    # 4. Seleciona APENAS as colunas que você quer e renomeia:
    dplyr::select(
      # Mantém todas as outras colunas originais do df_bruto_pivo (se houver),
      # e renomeia a coluna corrigida para 'valor'.
      everything(), # Mantém todas as colunas que não são explicitamente removidas ou renomeadas
      valor = Valor_Final_Corrigido, # RENOMEIA: novo_nome = nome_antigo

      # Remove as colunas auxiliares e o 'valor' original
      -valor,
      -Qualificador_Temp,
      -Valor_Num_Char,
      -Valor_Numerico_Convertido
    )

  ## Cria colunas analito e unidade
  ## Separa apenas na primeira ocorrência de "_" para lidar com nomes com múltiplos underscores
  df_bruto_pivo <- df_bruto_pivo |>
    tidyr::separate(
      analito,
      c("analito", "unidade", "metodo"),
      "_",
      extra = "merge",
      fill = "right"
    )

  ## Volta para a forma inicioal (sem NA)
  dpivo <-
    tidyr::pivot_wider(
      df_bruto_pivo,
      names_from = "analito",
      values_from = "valor"
    )

  # transformação < para -
  # Substitui dados qualificados
  ### Substitui srting < por - e elimina >
  df_sc_transf <- data.frame(lapply(dpivo, function(x) {
    gsub("<", "-", x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub(">", "", x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub("I.S.", NA, x, fixed = TRUE)
  }))

  df_sc_transf <- data.frame(lapply(df_sc_transf, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))

  df_sc_transf <-
    as.data.frame(apply(df_sc_transf, 2, function(x) {
      gsub(",", "\\.", x)
    }))
  df_sc_transf <-
    df_sc_transf |>
    dplyr::mutate(dplyr::across(
      6:ncol(df_sc_transf),
      ~ suppressWarnings(as.numeric(.))
    ))

  df_sc_05ld <- ltdl.fix.df(df_sc_transf)

  ## Pivoteia os dados transformados
  df2 <- df_sc_05ld |>
    tidyr::pivot_longer(
      cols = 7:ncol(df_sc_05ld),
      names_to = "analito",
      values_to = "valor"
    )

  ## Retira linhas com valor = NA
  df2 <- df2[!is.na(df2$valor), ]

  ## QAQC
  df_bk <-
    df_q[
      df_q$classe_am == "BLK" |
        df_q$NUM_LAB == "BLK" |
        df_q$NUM_LAB == "QUARTZ_GO",
    ]

  df_rp <- df_q[df_q$classe_am == "Sediment" | df_q$classe_am == "REP", ]
  df_rp$classe_am <- gsub("Sediment", "SMP", df_rp$classe_am)
  df_sd <- df_q[df_q$classe_am == "STD", ]

  QAQC_orig <- rbind(df_rp, df_bk, df_sd)

  # colnames(QAQC_orig) <- gsub("xx", "", colnames(QAQC_orig))
  QAQC_orig <- QAQC_orig |> dplyr::relocate(Boletim, .after = classe_am)
  QAQC_orig <- QAQC_orig[!is.na(QAQC_orig$NUM_LAB), ]
  # QAQC_orig$ID <- 1:nrow(QAQC_orig)
  # QAQC_orig <- QAQC_orig |> dplyr::relocate(ID)

  ## Pivoteia os dados analíticos
  QAQC_orig_pivo <- QAQC_orig |>
    tidyr::pivot_longer(
      cols = 4:(ncol(QAQC_orig)),
      names_to = "analito",
      values_to = "valor"
    )
  ## Cria colunas analito e unidade
  ## Separa apenas na primeira ocorrência de "_" para lidar com nomes com múltiplos underscores
  QAQC_orig_pivo <- QAQC_orig_pivo |>
    tidyr::separate(
      analito,
      c("analito", "unidade", "metodo"),
      "_",
      extra = "merge",
      fill = "right"
    )

  ## Retira valores com NA
  QAQC_orig_pivo <- QAQC_orig_pivo[!is.na(QAQC_orig_pivo$valor), ]

  QAQC_orig_pivo <- QAQC_orig_pivo |>
    # 1. Extração — limpa espaços em branco antes
    dplyr::mutate(
      valor = stringr::str_trim(valor),
      Valor_Num_Char = stringr::str_extract(valor, "^[^\\s]+"),
      Qualificador_Temp = stringr::str_extract(valor, "[<>]")
    ) |>

    # 2. Conversão — suprime warnings de coerção e trata valores inválidos
    dplyr::mutate(
      Valor_Numerico_Convertido = suppressWarnings(as.numeric(Valor_Num_Char))
    ) |>

    # 3. Lógica Condicional (case_when)
    dplyr::mutate(
      # Cria a coluna corrigida com um nome auxiliar para ser renomeada depois
      Valor_Final_Corrigido = dplyr::case_when(
        is.na(Valor_Numerico_Convertido) ~ valor,
        TRUE ~ paste0(
          dplyr::if_else(is.na(Qualificador_Temp), "", Qualificador_Temp),
          as.character(Valor_Numerico_Convertido)
        )
      )
    ) |> # 4. Substituição do Ponto Decimal pela Vírgula
    dplyr::mutate(
      # APLICAR A SUBSTITUIÇÃO: substitui todas as ocorrências de "." por ","
      Valor_Final_Corrigido = stringr::str_replace_all(
        Valor_Final_Corrigido,
        "\\.",
        ","
      )
    ) |>

    # 4. Seleciona APENAS as colunas que você quer e renomeia:
    dplyr::select(
      # Mantém todas as outras colunas originais do df_bruto_pivo (se houver),
      # e renomeia a coluna corrigida para 'valor'.
      everything(), # Mantém todas as colunas que não são explicitamente removidas ou renomeadas
      valor = Valor_Final_Corrigido, # RENOMEIA: novo_nome = nome_antigo

      # Remove as colunas auxiliares e o 'valor' original
      -valor,
      -Qualificador_Temp,
      -Valor_Num_Char,
      -Valor_Numerico_Convertido
    )

  ## Volta para a forma inicioal (sem NA)
  QAQC_orig <-
    tidyr::pivot_wider(
      QAQC_orig_pivo,
      names_from = "analito",
      values_from = "valor"
    )
  # Substitui dados qualificados
  ### Substitui srting < por - e elimina >
  QAQC_transf <- data.frame(lapply(QAQC_orig, function(x) {
    gsub("<", "-", x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub(">", "", x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("N.A.", NA, x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("<NA>", NA, x, fixed = TRUE)
  }))

  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("--", NA, x, fixed = TRUE)
  }))
  QAQC_transf <- data.frame(lapply(QAQC_transf, function(x) {
    gsub("I.S.", NA, x, fixed = TRUE)
  }))

  ## Substitui todos os dados ausentes codificados como -9999 por NAs
  # e  valores negativos que representam os valores menores do que valor
  # de detecção por Abs(valor)/2

  QAQC_transf <-
    as.data.frame(apply(QAQC_transf, 2, function(x) {
      gsub(",", "\\.", x)
    }))
  QAQC_transf <-
    QAQC_transf |>
    dplyr::mutate(dplyr::across(
      6:(ncol(QAQC_transf)),
      ~ suppressWarnings(as.numeric(.))
    ))
  QAQC_05ld <- ltdl.fix.df(QAQC_transf)

  # Cria tabela com a relação de boletim e laboratório
  lab_bol <- unique(ca_da[, c('Boletim', 'Laborat\u00f3rio')])

  ref = ca_da
  ref = unique(ref[, c("analito", "unidades", "metodo", "MDL")])
  ref$MDL <- gsub("<", "", ref$MDL)
  ref$MDL <- as.numeric(gsub(",", ".", ref$MDL))
  count_decimals = function(x) {
    #length zero input
    if (length(x) == 0) {
      return(numeric())
    }

    # Conta casas decimais
    x_nchr = x |> abs() |> as.character() |> nchar() |> as.numeric()
    x_int = floor(x) |> abs() |> nchar()
    x_nchr = x_nchr - 1 - x_int
    x_nchr[x_nchr < 0] = 0

    x_nchr
  }

  ref <- ref |>
    dplyr::group_by(analito, metodo, unidades) |>
    dplyr::summarise(MDL = min(MDL, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup()
  ref$DIG <- count_decimals(ref$MDL)
  # Lê UCC dos elementos

  ucc <- read.csv2(paste0(dir_ucc, ref_ucc), fileEncoding = "latin1")
  ref <- merge(
    ref,
    ucc[, c("EL", "UN", "Nome", "UCC")],
    by.x = c("analito", "unidades"),
    by.y = c("EL", "UN"),
    all.x = FALSE
  )

  ref <- unique(ref)
  colnames(ref) <- c("EL", "UN", "METODO", "LDI", "DIG", "Nome", "UCC")
  colnames(QAQC_05ld) <- gsub("classe_am", "COD", colnames(QAQC_05ld))
  colnames(QAQC_orig) <- gsub("classe_am", "COD", colnames(QAQC_orig))

  colnames(df_sc_05ld) <- gsub("classe_am", "CLASSE", colnames(df_sc_05ld))
  colnames(dpivo) <- gsub("classe_am", "CLASSE", colnames(dpivo))
  colnames(df_bruto_pivo) <- gsub("classe_am", "CLASSE", colnames(df_bruto_pivo))  
  colnames(df2) <- gsub("classe_am", "CLASSE", colnames(df2))  

  out[[1]] <- dpivo # dados analíticos brutos
  out[[2]] <- df_sc_05ld # dados analíticos transformados
  out[[3]] <- df_bruto_pivo # dados analíticos brutos pivotados
  out[[4]] <- df2 # dados transformados pivotados
  out[[5]] <- QAQC_orig # dados de qaqc bruto
  out[[6]] <- QAQC_05ld # dados de qaqc transformados
  out[[7]] <- ref # dados de informação do boletim
  out[[8]] <- ib_da 
  caminho_subpasta <- file.path(dir_out)
  if (!dir.exists(caminho_subpasta)) {
    dir.create(caminho_subpasta, recursive = TRUE, showWarnings = FALSE)
  }
  write.csv2(
    dpivo,
    file.path(caminho_subpasta, "dados_analíticos_brutos.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  write.csv2(
    df_sc_05ld,
    file.path(caminho_subpasta, "dados_analíticos_transformados.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
      write.csv2(
    df_bruto_pivo,
    file.path(caminho_subpasta, "dados_brutos_pivotados.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  write.csv2(
    df2,
    file.path(caminho_subpasta, "dados_transformados_pivotados.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  write.csv2(
    QAQC_05ld,
    file.path(caminho_subpasta, "dados_qaqc_transformados.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  write.csv2(
    ib_da,
    file.path(caminho_subpasta, "informações_boletins.csv"),
    fileEncoding = "latin1",
    row.names = FALSE
  )
  # write.csv2(
  #   refa,
  #   file.path(caminho_subpasta, "condições_analíticas.csv"),
  #   fileEncoding = "latin1",
  #   row.names = FALSE
  # )


  names(out) <- c(
    "dados brutos",
    "dados transformados",
    "dados brutos pivotados",
    "dados transformados pivotados",
    "dados qaqc bruto",
    "dados qaqc transformados",
    "condições de análise",
    "informações dos boletins"

  )

  return(out)
}
