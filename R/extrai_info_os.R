
# Leitura da OS -----------------------------
os <- readxl::read_xlsx("~/Projetos/Temp/os/OS_RA_002_SUREG_SP_2025_Autorizado.xlsx", sheet = 1, col_names  = FALSE)

# Dados da empresa solicitante --------------------------
busca <- "SERVIÇO GEOLÓGICO DO BRASIL-SGB"
indice_dados_empresa <- which(os == busca, arr.ind = TRUE)

de_linha <- list()
j <- 0
for(i in 1:4){
  de_linha[[i]] <- os[indice_dados_empresa[1]+j, indice_dados_empresa[2]]
  j <- j+1
}

dados_empresa <- paste(unlist(de_linha), collapse = "\n")
cat(dados_empresa)


# Título da ordem de serviço (contrato)
indices_tos <- which(matrix(grepl("ORDEM DE SERVIÇO PARA EXECUÇÃO CONTRATUAL", as.matrix(os)), 
                            nrow = nrow(os)), 
                     arr.ind = TRUE)

titulo_os <- as.character(os[indices_tos[1], indices_tos[2]])
cat(titulo_os)

# Informações da empresa contratada ----------------------
indices_info_empresa <- which(matrix(grepl("INFORMAÇÃO DA EMPRESA CONTRATADA", as.matrix(os)), 
                            nrow = nrow(os)), 
                     arr.ind = TRUE)
ie_linha <- list()
for(i in 1:4){
  ie_linha[[i]] <- os[indices_info_empresa[1]+1+i, indices_info_empresa[2]]
}

info_empresa  <- paste(unlist(ie_linha), collapse = "\n")
cat(info_empresa)

# Informações do solicitante ------------------------
campos_info_so <- c(
"* Nome do requerente das análises:",
"* Unidade Regional:",
"Endereço:",
"CEP:",
"Cidade – UF:",
"Fone / Fax:",
"E-mail:"
)


indices_nome_so <- which(os == campos_info_so[1], arr.ind = TRUE)
indices_un_reg_so <- which(os == campos_info_so[2], arr.ind = TRUE)
indices_endereco_so <- which(os == campos_info_so[3], arr.ind = TRUE)
indices_cp_so <- which(os == campos_info_so[4], arr.ind = TRUE)
indices_uf_so <- which(os == campos_info_so[5], arr.ind = TRUE)
indices_fone_fax_so <- which(os == campos_info_so[6], arr.ind = TRUE)
indices_email_so <- which(os == campos_info_so[7], arr.ind = TRUE)


nome_so <- os[indices_nome_so[1], (indices_nome_so[2]+4)]

un_reg_so <- os[indices_un_reg_so[1], (indices_un_reg_so[2]+2)]

endereco_so <- os[indices_endereco_so[1,1], (indices_endereco_so[1,2]+1)]

cp_so <- os[indices_cp_so[1,1], (indices_cp_so[1,2]+1)]

uf_so <- os[indices_uf_so[1,1], (indices_uf_so[1,2]+2)]

fone_fax_so <- os[indices_fone_fax_so[1,1], (indices_fone_fax_so[1,2]+1)]

email_so <- os[indices_email_so[1,1], (indices_email_so[1,2]+2)]

info_so <- data.frame(
  nome_so,
  un_reg_so,
  endereco_so,
  cp_so,
  uf_so,
  fone_fax_so,
  email_so)

colnames(info_so) <- c("Nome", "Unidade Regional", "Endereço", "CEP", "UF", "Fone FAX", "E-mail")


# Endereço e dados para o envio da fatura --------------------------------

campos_info_fa <- c(
"Nome responsável:",
"Empresa:",
"Endereço:",
"CEP:",
"CPF / CNPJ:",
"Cidade – UF:",
"Fone / Fax:",
"E-mail:"
)


indices_nome_fa <- which(os == campos_info_fa[1], arr.ind = TRUE)
indices_empresa <- which(os == campos_info_fa[2], arr.ind = TRUE)
indices_endereco_fa <- which(os == campos_info_fa[3], arr.ind = TRUE)
indices_cep_fa <- which(os == campos_info_fa[4], arr.ind = TRUE)
indices_cpf_fa <- which(os == campos_info_fa[5], arr.ind = TRUE)
indices_uf_fa <- which(os == campos_info_fa[6], arr.ind = TRUE)
indices_fone_fax_fa <- which(os == campos_info_fa[7], arr.ind = TRUE)
indices_email_fa <- which(os == campos_info_fa[8], arr.ind = TRUE)


nome_fa <- os[indices_nome_fa[1], (indices_nome_fa[2]+2)]

un_empresa <- os[indices_empresa[1], (indices_empresa[2]+1)]

endereco_fa <- os[indices_endereco_fa[2,1], (indices_endereco_fa[2,2]+1)]

cep_fa <- os[indices_cep_fa[2,1], (indices_cep_fa[2,2]+1)]
cpf_fa <- os[indices_cpf_fa[1], (indices_cpf_fa[2]+1)]

uf_fa <- os[indices_uf_fa[2,1], (indices_uf_fa[2,2]+2)]

fone_fax_fa <- os[indices_fone_fax_fa[2,1], (indices_fone_fax_fa[2,2]+2)]

email_fa <- os[indices_email_fa[2,1], (indices_email_fa[2,2]+2)]

info_fa <- data.frame(
  nome_fa,
  un_empresa,
  cpf_fa,
  endereco_fa,
  cep_fa,
  uf_fa,
  fone_fax_fa,
  email_fa)

colnames(info_fa) <- c("Nome", "Empresa", "CNPJ", "Endereço", "CEP", "UF", "Fone FAX", "E-mail")



# E-mails para recebimento dos resultados -------------------------
email_info <- c()
email_info[1] <- "01."
email_info[2] <- "02."
email_info[3] <- "03."
email_info[4] <- "04."
email_info[5] <- "05."
email_info[6] <- "06."
email_info[7] <- "07."
email_info[8] <- "08."

df_nome_email <- list()
for(i in 1:8){
  indice <- which(os == email_info[i], arr.ind = TRUE)
if(i %in% 1:4){  
  cargo <- os[indice[1], (indice[2])+1]
  email <- os[indice[1], (indice[2]+3)]}else{
  cargo <- NA
  email <- os[indice[1], (indice[2]+1)]
}
df <- data.frame(cargo, email)
  colnames(df) <- c("Cargo", "E-mail")
  df_nome_email[[i]] <- df
}
emails_res <- do.call(rbind,df_nome_email)
row.names(emails_res) <- email_info
# Instruções espaciais ---------------------------
instrucoes_especiais <- "INSTRUÇÕES ESPECIAIS:"
indice_inst <- which(os == instrucoes_especiais, arr.ind = TRUE)

instrucoes <- as.character(os[indice_inst[1]+1, (indice_inst[2])])
cat(instrucoes)

# Resumo da Solicição ----------------------------
campos <- c("Total de amostras:",
"* Projeto :",
"* Centro de Custo:",
"* Nº Requisição R.A.:",
"* Nº Contrato:",
"* Nº da Nota de Empenho",
"* Data :"
)

indices_num_am <- which(os == campos[1], arr.ind = TRUE)
indices_projeto <- which(os == campos[2], arr.ind = TRUE)
indices_cc <- which(os == campos[3], arr.ind = TRUE)
indices_ra <- which(os == campos[4], arr.ind = TRUE)
indices_contrato <- which(os == campos[5], arr.ind = TRUE)
indices_ne <- which(os == campos[6], arr.ind = TRUE)
indices_data_os <- which(os == campos[7], arr.ind = TRUE)

num_am <- os[indices_num_am[1], (indices_num_am[2]+2)]
projeto <- os[indices_projeto[1], (indices_projeto[2]+2)]
cc <- os[indices_cc[1], (indices_cc[2]+2)]
ra <- os[indices_ra[1], (indices_ra[2]+2)]
contrato <- os[indices_contrato[1], (indices_contrato[2]+2)]
ne <- os[indices_ne[1], (indices_ne[2]+2)]
data_os <- as.numeric(os[indices_data_os[1], (indices_data_os[2]+2)])

data_os <- excel_numeric_to_date(data_os)
data_os <- format(data_os, "%d/%m/%Y")

res_solicitacao <- data.frame(
  num_am,
  projeto,
  cc,
  ra,
  contrato,
  ne,
  data_os
)

colnames(res_solicitacao) <- 
  c("Total de amostras", "Projeto", "Centro de Custo", "No. de RA", "No. de Contrato", "No. da Nota de Empenho", "Data da OS")



