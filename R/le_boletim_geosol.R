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
le_boletim_geosol <- function(
  classe_am,
  dir_bol,
  dir_ucc,
  ref_ucc,
  dir_out,
  tipo = "Química"
) {
  ## Diretórios de entrada dos dados
  classes <-
    c(
      "Concentrado de bateia",
      "Sedimento de corrente",
      "Rocha",
      "Solo",
      "\u00c1gua"
    )
  cod_classes <- c("B", "S", "R", "L", "A")
  nome_bol <-
    c(
      "CONCENTRADO DE BATEIA",
      "SEDIMENTO CORRENTE",
      "ROCHA",
      "SOLO",
      "\u00c1GUA"
    )

  ## Gera o camionho para os arquivos
  ## Entrada

  list_bol <- list.files(
    dir_bol,
    pattern = "\\.xls$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(list_bol) == 0) {
    stop("Nenhum arquivo encontrado na pasta das boletins.")
  }
  out <- list()
  if (tipo == "Química") {
    source("R/ltdl.fix.df.R")

    ## Cria listas com cada informação do boletim
    datalist = list()
    datalist2 = list()
    datalist3 = list()
    datalist4 = list()
    datalist5 = list()
    datalist6 = list()
    datalist7 = list()

    ## Condição para leitura do boletim
    # if (nome_bol[classe_am] == "ROCHA") {
    #   ini = 6
    # } else {
    #   ini = 12
    # }

    read.csv2(
      "inputs/nomes_info.csv",
      fileEncoding = "latin1",
      header = TRUE
    ) -> nomes_info
    ## Ler cada boletim do diretório e extrair as informações
    ## DADOS analíticos
    for (i in 1:length(list_bol)) {
      df_tudo = readxl::read_excel(list_bol[i], col_names = FALSE)
      colnames(df_tudo) <- paste0("V", 1:ncol(df_tudo))
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
      metodo <- as.character(t(df_tudo[
        indices_metodo[1],
        (indices_metodo[2] + 1):n
      ]))
      analito <- as.character(t(df_tudo[
        indices_metodo[1] + 1,
        (indices_metodo[2] + 1):n
      ]))
      df_temp <- data.frame(valor = analito)

      analito_transformado_df <- df_temp |>
        # Junta o df temporário com o df_mapa, usando 'valor' e 'original' como chaves
        dplyr::left_join(nomes_info, by = c("valor" = "original"))

      # O vetor final está na nova coluna 'novo'
      analito <- analito_transformado_df$novo

      unidade <- tolower(t(df_tudo[
        indices_metodo[1] + 2,
        (indices_metodo[2] + 1):n
      ]))
      unidade <- gsub("%", "pct", unidade)
      MDL <- as.character(t(df_tudo[
        indices_metodo[1] + 3,
        (indices_metodo[2] + 1):n
      ]))
      df_tudo$V1 <- gsub(" - (", " ", df_tudo$V1, fixed = TRUE)
      df_tudo$V1 <- gsub(")", "", df_tudo$V1, fixed = TRUE)
      df_tudo$V1 <- df_tudo$V1 |> stringr::str_squish()

      if ((indices_metodo[2] - indices_amostra[2]) < 2) {
        boletim <- df_tudo[(indices_amostra[1] + 1):r, indices_amostra[2]:n]
      } else {
        df_tudo <- df_tudo |>
          dplyr::mutate(
            V1 = dplyr::if_else(
              # 1. Condição: Verifica se V2 não está faltando E não está vazio.
              !is.na(V2) & V2 != "",

              # 2. Se VERDADEIRO: Concatena V1 e V2
              paste(V1, V2, sep = " "),

              # 3. Se FALSO: Mantém o valor original de V1
              V1
            )
          )
        # Remove a segunda palavra do valor da coluna V1

        boletim <- df_tudo[
          (indices_amostra[1] + 1):r,
          c(indices_amostra[2], (indices_amostra[2] + 2):n)
        ]
      }

      # 1. Usando stringr::word()

      job_boletim <- rep(as.character(n_job), nrow(boletim))
      boletim <- cbind(boletim, job_boletim)
      # Cria tabela das condições analíticas
      LAB <- rep(as.character(laboratorio), length(metodo))
      condicoes_analiticas <-
        data.frame(metodo, analito, unidade, MDL, n_job, LAB)

      var.name <-
        c('metodo', 'analito', 'unidade', 'MDL', 'Boletim', "Laborat\u00f3rio")
      colnames(condicoes_analiticas) <- var.name
      boletim <- boletim %>%
        dplyr::select(where(~ !all(is.na(.))))
      analito <- analito[!is.na(analito)]
      unidade <- unidade[!is.na(unidade)]
      metodo <- metodo[!is.na(metodo)]

      colnames(boletim) <-
        c(
          "NUM_LAB",
          "classe_am",
          paste0(analito, "_", unidade, "_", metodo),
          "Boletim"
        )

      boletim$NUM_LAB <- ifelse(
        stringr::str_detect(boletim$NUM_LAB, " "),
        stringr::word(boletim$NUM_LAB, 2),
        boletim$NUM_LAB
      )
      boletim <- boletim |>
        dplyr::mutate(
          classe_am = dplyr::if_else(classe_am == "1", "DUP", classe_am),
          classe_am = dplyr::if_else(classe_am == "2", "DUP", classe_am)
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
          paste0(analito, "_", unidade, "_", metodo),
          ~ limpar_numeros_texto(.)
        )) # Exemplo com colunas 1, 2 e 3

      # Adiciona às listas
      datalist[[i]] <- info_boletim
      datalist2[[i]] <- condicoes_analiticas
      datalist3[[i]] <- boletim
    }

    ib_da = do.call(dplyr::bind_rows, datalist)
    ca_da = do.call(dplyr::bind_rows, datalist2)

    df_da = do.call(dplyr::bind_rows, datalist3)

    colnames(df_da) <- gsub("%", "pct", colnames(df_da))

    # Coloca Boletim
    df_da <- df_da |> dplyr::relocate(Boletim, .after = last_col())

    colnames(ca_da) <- gsub("._", "_", colnames(ca_da), fixed = TRUE)
    colnames(ca_da) <- gsub(".", "_", colnames(ca_da), fixed = TRUE)
    # Padroniza nome de laboratório
    df_da$NUM_LAB <- gsub("-", "", df_da$NUM_LAB)
    df_da$NUM_LAB <- gsub(" ", "", df_da$NUM_LAB)

    # Arruma nomes dos analitos e inserir coluna dos métodos
    lista_metodos <-
      unique(ca_da[, c("analito", "metodo")])$metodo # lista dos métodos com artifícios

    ## Retira linhas sem NUM_LAB
    df_sc <- df_da[!is.na(df_da$NUM_LAB), ]
    df_sc <- df_sc[df_sc$classe_am != "", ]

    ## Substitui valores nulos por NA
    df_sc <- data.frame(lapply(df_sc, function(x) {
      gsub("N.A.", NA, x, fixed = TRUE)
    }))
    df_sc <- data.frame(lapply(df_sc, function(x) {
      gsub("I,N,F,", NA, x, fixed = TRUE)
    }))

    # df_sc$classe_am <- classes[classe_am]
    df_sc <- df_sc |> dplyr::relocate(c(Boletim), .after = classe_am)
    df_sc <- df_sc |>
      dplyr::distinct(NUM_LAB, classe_am, Boletim, .keep_all = TRUE)
    lista_classes <- read.csv2(
      "inputs/lista_classes.csv",
      fileEncoding = "latin1"
    )
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
    df_bruto_pivo$valor <- gsub(" ", "", df_bruto_pivo$valor, fixed = TRUE)
    df_bruto_pivo$valor <- gsub(".", ",", df_bruto_pivo$valor, fixed = TRUE)
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

    df_bruto_pivo <- df_bruto_pivo[
      !(df_bruto_pivo$classe_am %in% c("BRANCO_PREP", "REP", "DUP", "STD")),
    ]
    df_bruto_pivo <- df_bruto_pivo[
      !(df_bruto_pivo$NUM_LAB %in% c("BRANCO_PREP", "REP", "DUP", "STD")),
    ]
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
        values_from = "valor",
        names_sort = TRUE
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
        cols = 6:ncol(df_sc_05ld),
        names_to = "analito",
        values_to = "valor"
      )

    ## Retira linhas com valor = NA
    df2 <- df2[!is.na(df2$valor), ]

    df_sc$classe_am <- gsub(nome_bol[classe_am], "SMP", df_sc$classe_am)
    ## QAQC
    df_bk <-
      df_sc[df_sc$NUM_LAB == "BRANCO_PREP", ]

    df_rp <- df_sc[
      df_sc$classe_am == "REP" |
        df_sc$classe_am == "DUP" |
        df_sc$classe_am == "STD",
    ]

    df_sd <- df_sc[df_sc$NUM_LAB == "STD", ]

    QAQC_orig <- rbind(df_rp, df_bk, df_sd)

    # colnames(QAQC_orig) <- gsub("xx", "", colnames(QAQC_orig))
    QAQC_orig <- QAQC_orig |>
      dplyr::relocate(c(Boletim), .after = classe_am)
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
    ref = unique(ref[, c("analito", "unidade", "metodo", "MDL")])
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

    # 💡 CORREÇÃO 1: Filtrar NA's na coluna de agrupamento (analito) antes de sumarizar
    ref <- ref[!is.na(ref$analito), ]

    ref <- ref |>
      dplyr::group_by(analito, metodo, unidade) |>

      # 💡 CORREÇÃO 2 (Opcional, mais robusta): Usar coalesce para garantir que Inf vire NA
      dplyr::summarise(
        MDL = dplyr::coalesce(min(MDL, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) |>
      dplyr::ungroup()

    ref$DIG <- count_decimals(ref$MDL) # Mantido

    # ref <- ref[!is.na(ref$analito),] # Linha removida/movida (redundante após a correção 1)
    # Lê UCC dos elementos

    ucc <- read.csv2(paste0(dir_ucc, ref_ucc), fileEncoding = "latin1")

    ref <- merge(
      ref,
      ucc[, c("EL", "UN", "Nome", "UCC")],
      by.x = c("analito", "unidade"),
      by.y = c("EL", "UN"),
      all.x = FALSE
    )
    ref <- unique(ref)
    colnames(QAQC_05ld) <- gsub("classe_am", "COD", colnames(QAQC_05ld))
    colnames(QAQC_orig) <- gsub("classe_am", "COD", colnames(QAQC_orig))

    colnames(df_sc_05ld) <- gsub("classe_am", "CLASSE", colnames(df_sc_05ld))
    colnames(dpivo) <- gsub("classe_am", "CLASSE", colnames(dpivo))
    colnames(df_bruto_pivo) <- gsub(
      "classe_am",
      "CLASSE",
      colnames(df_bruto_pivo)
    )
    colnames(df2) <- gsub("classe_am", "CLASSE", colnames(df2))

    colnames(ref) <- c("EL", "UN", "METODO", "LDI", "DIG", "Nome", "UCC")

    out[[1]] <- dpivo # dados analíticos brutos
    out[[2]] <- df_sc_05ld # dados analíticos transformados
    out[[3]] <- df_bruto_pivo # dados analíticos brutos pivotados
    out[[4]] <- df2 # dados transformados pivotados
    out[[5]] <- QAQC_orig # dados de qaqc bruto
    out[[6]] <- QAQC_05ld # dados de qaqc transformados
    out[[7]] <- ref # dados de informação do boletim
    out[[8]] <- ib_da # dados da relação boletim e laboratório

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
    # write.csv2(
    #   ref,
    #   file.path(caminho_subpasta, "condições_analíticas.csv"),
    #   fileEncoding = "latin1",
    #   row.names = FALSE
    # )
    write.csv2(
      ib_da,
      file.path(caminho_subpasta, "informações_boletins.csv"),
      fileEncoding = "latin1",
      row.names = FALSE
    )

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
  } else {
    ## Cria listas com cada informação do boletim
    datalist = list()
    datalist2 = list()
    datalist3 = list()

    for (i in 1:length(list_bol)) {
      df_tudo = readxl::read_excel(list_bol[i], col_names = FALSE)
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

      laboratorio_unidade <- df_tudo[
        indices_tipo_bol[1] - 2,
        indices_tipo_bol[2]
      ]
      laboratorio <- df_tudo[indices_tipo_bol[1] - 6, indices_tipo_bol[2]]
      tipo_analise <- df_tudo[indices_tipo_bol[1], indices_tipo_bol[2]]
      n_job <- df_tudo[indices_tipo_bol[1] + 1, indices_tipo_bol[2] + 12]

      cliente_nome <- df_tudo[
        indices_info_cliente[1] + 2,
        indices_info_cliente[2]
      ]
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

      data_recebido <- lubridate::as_date(
        as.numeric(data_recebido),
        origin = "1899-12-30"
      )
      data_recebido <- format(data_recebido, "%d/%m/%Y")

      data_fin <- df_tudo[
        indices_data[1] + 5,
        indices_data[2]
      ]
      data_fin <- lubridate::as_date(
        as.numeric(data_fin),
        origin = "1899-12-30"
      )
      data_fin <- format(data_fin, "%d/%m/%Y")

      data_ini <- df_tudo[
        indices_data[1] + 2,
        indices_data[2] + 6
      ]
      data_ini <- lubridate::as_date(
        as.numeric(data_ini),
        origin = "1899-12-30"
      )
      data_ini <- format(data_ini, "%d/%m/%Y")

      data_env <- df_tudo[
        indices_data[1] + 5,
        indices_data[2] + 6
      ]
      data_env <- lubridate::as_date(
        as.numeric(data_env),
        origin = "1899-12-30"
      )
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

      unidade <- tolower(t(df_tudo[
        indices_prep_am[1] + 3,
        (indices_prep_am[2]):n
      ]))

      unidade[is.na(unidade)] <- "g"

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
        data.frame(metodo, analito, unidade, job_boletim, LAB)

      # Preenchendo valores de metodo
      condicoes_analiticas <- condicoes_analiticas |>
        tidyr::fill(metodo, .direction = "down")
      condicoes_analiticas$metodo <- gsub(
        " (g)",
        "",
        condicoes_analiticas$metodo,
        fixed = TRUE
      )
      condicoes_analiticas$metodo <- stringr::str_trim(
        condicoes_analiticas$metodo
      )
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
        c('metodo', 'analito', 'unidade', 'Boletim', "Laborat\u00f3rio")
      colnames(condicoes_analiticas) <- var.name
      boletim <- boletim |>
        dplyr::select(where(~ !all(is.na(.))))
      analito <- analito[!is.na(analito)]
      unidade <- unidade[!is.na(unidade)]
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

      # Adiciona às listas
      datalist[[i]] <- info_boletim
      datalist2[[i]] <- condicoes_analiticas
      datalist3[[i]] <- boletim
    }

    df_dados_brutos <- do.call(dplyr::bind_rows, datalist3)

    df_dados_brutos$CLASSE <- classe_am

    df_pivo <- df_dados_brutos |>
      tidyr::pivot_longer(
        cols = 2:(ncol(df_dados_brutos) - 1),
        names_to = "analito",
        values_to = "valor",
        values_drop_na = TRUE
      )

    ca <- do.call(dplyr::bind_rows, datalist2)

    df_pivo <- merge(df_pivo, ca, by = "analito")
    prep_amostra <- df_pivo |> dplyr::filter(!(metodo %in% c("ANALISE_SEMIQ", "CONT_PINTAS")))
    df_pivo <- df_pivo |> dplyr::filter(metodo %in% c("ANALISE_SEMIQ", "CONT_PINTAS"))
    
    ## Transforma para intervalos de classes
    valor_unitario <- c("1", "3", "15", "40", "60", "85")
    valor_intervalo <- c(
      "< 1 %",
      "1 - 5 %",
      "5 - 25 %",
      "25 - 50 %",
      "50 - 75 %",
      "75 - 100 %"
    )
   df_legenda <- data.frame(valor=valor_unitario, valor_novo=valor_intervalo)
   df_legenda$metodo <- "ANALISE_SEMIQ"
  
# 4. Transformação condicional
# Usamos left_join para não perder nenhuma linha do df_pivo
 df_pivo_transf <- df_pivo  |> # Garante compatibilidade de tipo
  dplyr::left_join(df_legenda, by = c("metodo", "valor")) |>
  dplyr::mutate(
    # Se existe valor_novo (deu match na legenda), usa ele. 
    # Senão, mantém o valor original.
    valor = dplyr::if_else(!is.na(valor_novo), valor_novo, valor)
  ) |>
  dplyr::select(-valor_novo) |>
  dplyr::relocate(NUM_LAB, CLASSE, Boletim, analito, unidade, metodo, valor)
  
  df_dados_transf <- df_pivo_transf |> dplyr::arrange(metodo) |>
    tidyr::pivot_wider(values_from = "valor", names_from = "analito")  |>
  dplyr::relocate(NUM_LAB, CLASSE, Boletim,unidade, metodo,Laboratório, 'OURO < 0,5 mm' , 'OURO >1mm' ,                                 
 'OURO 0,5 – 1mm' )
    
    
  df_pivo <- df_pivo |> dplyr::relocate(c(NUM_LAB, CLASSE, Boletim, analito, unidade, metodo, valor))
  
  
  
  df_dados_brutos <- df_pivo |> tidyr::pivot_wider(names_from = "analito", values_from = "valor")|>
  dplyr::relocate(NUM_LAB, CLASSE, Boletim,unidade, metodo,Laboratório, 'OURO < 0,5 mm' , 'OURO >1mm' ,                                 
 'OURO 0,5 – 1mm' )
  df_dados_brutos <- df_dados_brutos[,colnames(df_dados_transf)]
    
    out[[1]] <- df_dados_brutos
    out[[2]] <- df_dados_transf
    out[[3]] <- df_pivo
    out[[4]] <- df_pivo_transf
    out[[5]] <- ca
    info_bol <- do.call(dplyr::bind_rows, datalist)
    out[[6]] <- info_bol
    out[[7]] <- prep_amostra

    names(out) <- c(
      "dados brutos",
      "dados transformados",
      "dados brutos pivotados",
      "dados transformados pivotados",
      "condições de análise",
      "informações dos boletins",
      "preparação da amostra"
    )
      caminho_subpasta <- file.path(dir_out)
      if (!dir.exists(caminho_subpasta)) {
        dir.create(caminho_subpasta, recursive = TRUE, showWarnings = FALSE)
      }
        
      write.csv2(
        info_bol,
        file.path(caminho_subpasta, "informações_boletins.csv"),
        fileEncoding = "latin1",
        row.names = FALSE
      )

      write.csv2(
        ca,
        file.path(caminho_subpasta, "condicoes_analiticas.csv"),
        fileEncoding = "latin1",
        row.names = FALSE
      )

      write.csv2(
        df_dados_brutos,
        file.path(caminho_subpasta, "dados_analíticos_brutos.csv"),
        fileEncoding = "latin1",
        row.names = FALSE
      )
          write.csv2(
        df_dados_transf,
        file.path(caminho_subpasta, "dados_analíticos_transformados.csv"),
        fileEncoding = "latin1",
        row.names = FALSE
      )
          write.csv2(
      df_pivo,
      file.path(caminho_subpasta, "dados_brutos_pivotados.csv"),
      fileEncoding = "latin1",
      row.names = FALSE
    )
    write.csv2(
      df_pivo_transf,
      file.path(caminho_subpasta, "dados_transformados_pivotados.csv"),
      fileEncoding = "latin1",
      row.names = FALSE
    )
    write.csv2(
      prep_amostra,
      file.path(caminho_subpasta, "preparacao_amostras.csv"),
      fileEncoding = "latin1",
      row.names = FALSE
    )
  }
  return(out)
}
