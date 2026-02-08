extrai_dados_os <- function(
  dir_os,
  dir_out,
  tipo = "Química"){
  ## Entrada
  files_os <- list.files(dir_os, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE, 
  ignore.case = TRUE)
if (length(files_os) == 0) {
    stop("Nenhum arquivo .xlsx encontrado na pasta das os.")
  }

lista_os_ra <- list()
lista_os_envio <- list()
lista_os_info <- list()
lista_os_aceite <- list()

for (i in 1:length(files_os)) {
  df_tudo = readxl::read_excel(files_os[i], col_names = TRUE, sheet = 2)
  df_info = readxl::read_excel(files_os[i], col_names = TRUE, sheet = 1)

  r <- nrow(df_tudo)
  n <- ncol(df_tudo)
  # LE Tabelas do arquivo de amostras da litoteca - projeto Eldorado
  if(tipo == "Mineralógica"){
indices_long <- which(df_tudo == "Long", arr.ind = TRUE)
if (nrow(indices_long) == 0) {
   stop(paste("Palavra 'Long' não encontrada no arquivo:", files_os[i]))
}
    boletim <- df_tudo[(indices_long[1] + 1):r, (indices_long[2] - 2):n]
    nomes_bol <- df_tudo[(indices_long[1] - 1), (indices_long[2] + 2):(n-1)]
    nomes_extra <- df_tudo[1, (n-1):n]
    colnames(boletim) <- c("Sequência", "Longitude", "Latitude",  nomes_bol, nomes_extra)
    indices_cliente <- which(df_info == "INFORMAÇÃO DO CLIENTE", arr.ind = TRUE)
    indices_envio <- which(
    df_info == "* E-MAIL´S PARA RECEBIMENTO DOS RESULTADOS",
    arr.ind = TRUE)

  tab_envio1 <- df_info[
    (indices_envio[1] + 1):(indices_envio[1] + 4),
    indices_envio[2]:(indices_envio[2] + 2)
  ]
    colnames(tab_envio1) <- c("Ordem", "Função", "E-mail")
    indices_aceite <- which(df_info == "ACEITE DO CLIENTE", arr.ind = TRUE)
  nota_empenho <- df_info[indices_aceite[1] + 4, indices_aceite[2] + 2]
  ra <- df_info[indices_aceite[1] + 5, indices_aceite[2] + 2]
  contrato <- df_info[indices_aceite[1] + 6, indices_aceite[2] + 2]
  }else{
   indices_long <- which(df_tudo == "LONG", arr.ind = TRUE)
if (nrow(indices_long) == 0) {
   stop(paste("Palavra 'Long' não encontrada no arquivo:", files_os[i]))
}
    boletim <- df_tudo[(indices_long[1] + 2):r, (indices_long[2] - 2):n]
    nomes_info <- df_tudo[(indices_long[1] - 3), (indices_long[2] + 2):10]
    nomes_analise <- df_tudo[(indices_long[1]), (indices_long[2] + 9):(n-3)]
    nomes_extra <- df_tudo[1, (n-2):n]
    colnames(boletim) <- c("Sequência", "Longitude", "Latitude",  nomes_info, nomes_analise, nomes_extra)
    df_info = readxl::read_excel(files_os[i], col_names = TRUE, sheet = 1)
    indices_cliente <- which(df_info == "INFORMAÇÃO DO SOLICITANTE", arr.ind = TRUE)
    indices_envio <- which(
    df_info == "* E-MAIL´S PARA RECEBIMENTO DOS RESULTADOS",
    arr.ind = TRUE
  )
    
    tab_envio1 <- df_info[
    (indices_envio[1] + 1):(indices_envio[1] + 4),
    indices_envio[2]:(indices_envio[2] + 3)
  ]
    tab_envio1 <- tab_envio1 |>
    dplyr::select(where(~ !all(is.na(.))))
    colnames(tab_envio1) <- c("Ordem", "Função", "E-mail")
    indices_aceite <- which(df_info == "RESUMO DA SOLICITAÇÃO", arr.ind = TRUE)
   
   ra    <- df_info[indices_aceite[1] + 4, indices_aceite[2] + 2]
   contrato <- df_info[indices_aceite[1] + 5, indices_aceite[2] + 2]
   nota_empenho <- df_info[indices_aceite[1] + 6, indices_aceite[2] + 2]

  }

  nome_cliente <- df_info[indices_cliente[1] + 1, indices_cliente[2] + 4]
  empresa_cliente <- df_info[indices_cliente[1] + 2, indices_cliente[2] + 4]
  endereco_cliente <- df_info[indices_cliente[1] + 3, indices_cliente[2] + 4]
  cep_cliente <- df_info[indices_cliente[1] + 4, indices_cliente[2] + 4]
  fone_fax_cliente <- df_info[indices_cliente[1] + 5, indices_cliente[2] + 4]
  email_cliente <- df_info[indices_cliente[1] + 5, indices_cliente[2] + 8]

  indices_fatura <- which(
    df_info == "ENDEREÇO E DADOS PARA O ENVIO DA FATURA",
    arr.ind = TRUE
  )
  nome_fatura <- df_info[indices_fatura[1] + 1, indices_fatura[2] + 4]
  empresa_fatura <- df_info[indices_fatura[1] + 2, indices_fatura[2] + 4]
  endereco_fatura <- df_info[indices_fatura[1] + 3, indices_fatura[2] + 4]
  cep_fatura <- df_info[indices_fatura[1] + 4, indices_fatura[2] + 4]
  cpf_cnpj_fatura <- df_info[indices_fatura[1] + 5, indices_fatura[2] + 4]
  cidade_uf_fatura <- df_info[indices_fatura[1] + 3, indices_fatura[2] + 9]
  fone_fax_fatura <- df_info[indices_fatura[1] + 4, indices_fatura[2] + 9]
  email_fatura <- df_info[indices_fatura[1] + 5, indices_fatura[2] + 9]

  tab_envio2 <- df_info[
    (indices_envio[1] + 1):(indices_envio[1] + 4),
    (indices_envio[2] + 6):(indices_envio[2] + 7)
  ]

  colnames(tab_envio2) <- c("Ordem", "E-mail")
  tab_envio <- dplyr::bind_rows(tab_envio1, tab_envio2)
  tab_envio <- tab_envio[!is.na(tab_envio$`E-mail`), ]
  tab_envio[is.na(tab_envio$Função), "Função"] <- "Gestor de Dados"

  n_amostra <- df_info[indices_aceite[1] + 1, indices_aceite[2] + 2]
  projeto <- df_info[indices_aceite[1] + 2, indices_aceite[2] + 2]
  centro_custo <- stringr::str_trim(df_info[
    indices_aceite[1] + 3,
    indices_aceite[2] + 2
  ])


  data_ra <- df_info[indices_aceite[1] + 7, indices_aceite[2] + 2]
  reponsavel <- df_info[indices_aceite[1] + 8, indices_aceite[2] + 2]

  df_aceite <-
    data.frame(
      n_amostra,
      projeto,
      centro_custo,
      nota_empenho,
      ra,
      contrato,
      data_ra,
      reponsavel
    )
  nomes_aceite <- c(
    "Número de amostras",
    "Projeto",
    "Centro de Custo",
    "Nota de Empenho",
    "RA",
    "Contrato",
    "Data da RA",
    "Responsável"
  )
  colnames(df_aceite) <- nomes_aceite
  df_aceite$`Informação` <- "RESUMO DA SOLICITAÇÃO"
  lista_os_aceite[[i]] <- df_aceite


  df_cliente <-
    data.frame(
      nome_cliente,
      empresa_cliente,
      endereco_cliente,
      cep_cliente,
      cidade_uf_fatura,
      fone_fax_cliente,
      email_cliente
    )
  nomes_cliente <- c(
    "Nome",
    "Empresa",
    "Endereço",
    "CEP",
    "Cidade / UF",
    "Fone / Fax",
    "E-mail"
  )
  colnames(df_cliente) <- nomes_cliente
  df_cliente$`Informação` <- "INFORMAÇÃO DO CLIENTE"

  df_fatura <- data.frame(
    nome_fatura,
    empresa_fatura,
    cpf_cnpj_fatura,
    endereco_fatura,
    cep_fatura,
    cidade_uf_fatura,
    fone_fax_fatura,
    email_fatura
  )

  nomes_fatura <- c(
    "Nome",
    "Empresa",
    "CPF / CNPJ",
    "Endereço",
    "CEP",
    "Cidade – UF",
    "Fone / Fax",
    "E-mail"
  )
  colnames(df_fatura) <- nomes_fatura

  df_fatura$`Informação` <- "ENDEREÇO E DADOS PARA O ENVIO DA FATURA"
  tab_envio$`Informação` <- "E-MAIL´S PARA RECEBIMENTO DOS RESULTADOS"
  
  boletim$C.C <- as.character(centro_custo)
  boletim$PROJETO <- as.character(projeto)

  lista_os_aceite[[i]] <- df_aceite
  ra <- as.character(ra)
  boletim$RA <- ra
  lista_os_ra[[i]] <- boletim
  df_cliente$RA <- ra
  df_fatura$RA <- ra
  lista_os_info[[i]] <- dplyr::bind_rows(df_cliente, df_fatura)
  tab_envio$RA <- ra
  lista_os_envio[[i]] <- tab_envio
}

dados_amostras <- do.call(dplyr::bind_rows, lista_os_ra)

dados_info <- do.call(dplyr::bind_rows, lista_os_info)

dados_envio <- do.call(dplyr::bind_rows, lista_os_envio)
dados_aceite <- do.call(dplyr::bind_rows, lista_os_aceite)


if(tipo == "Mineralógica"){
  primeiras_colunas <- c(
  "ORDEM",
  "LONGITUDE",
  "LATITUDE",
  "NUM_CAMPO",
  "NUM_LAB",
  "LOTE", 
  "CLASSE"
)
  colnames(dados_amostras)[1:7] <- primeiras_colunas
  dados_amostras$CLASSE <- "Concentrado de bateia"
}else{
  primeiras_colunas <- c(
  "ORDEM",
  "LONGITUDE",
  "LATITUDE",
  "NUM_CAMPO",
  "LITOTIPO",
  "LOTE", 
  "NUM_LAB",
  "CLASSE",
  "MATRIZ"
)
  colnames(dados_amostras)[1:9] <- primeiras_colunas
  dados_amostras$CLASSE <- "Sedimento de corrente"
}


dados_amostras$NUM_LAB <- gsub("-", "", dados_amostras$NUM_LAB)
dados_amostras <- dados_amostras |> dplyr::filter(!is.na(NUM_LAB))

caminho_subpasta <- file.path(dir_out)
if (!dir.exists(caminho_subpasta)) {
  dir.create(caminho_subpasta, recursive = TRUE, showWarnings = FALSE)
}
write.csv2(
  dados_amostras,
  file.path(caminho_subpasta, "dados_os.csv"),
  fileEncoding = "latin1",
  row.names = FALSE
)

write.csv2(
  dados_info,
  file.path(caminho_subpasta, "dados_info.csv"),
  fileEncoding = "latin1",
  row.names = FALSE
)
  
write.csv2(
  dados_envio,
  file.path(caminho_subpasta, "dados_envio.csv"),
  fileEncoding = "latin1",
  row.names = FALSE
)
  
write.csv2(
  dados_aceite,
  file.path(caminho_subpasta, "dados_aceite.csv"),
  fileEncoding = "latin1",
  row.names = FALSE
)

# dados_amostras <- unique(dados_amostras)
return(dados_amostras)
}