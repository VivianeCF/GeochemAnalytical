os <- readxl::read_xlsx("inputs/eldorado/os/OS_SUREG_SP_eldorado/OS_RA001_SUREG_SP_lote1513.xlsx", sheet = 1)
campos <- c("Total de amostras:",
"* Projeto :",
"* Centro de Custo:",
"* Nº Requisição R.A.:",
"* Nº Contrato:",
"* Nº da Nota de Empenho",
"* Data :"
)


# 3. Use which() com arr.ind = TRUE
indices_num_am <- which(os == campos[1], arr.ind = TRUE)
indices_projeto <- which(os == campos[2], arr.ind = TRUE)
indices_cc <- which(os == campos[3], arr.ind = TRUE)
indices_ra <- which(os == campos[4], arr.ind = TRUE)
indices_contrato <- which(os == campos[5], arr.ind = TRUE)
indices_ne <- which(os == campos[6], arr.ind = TRUE)
indices_data_os <- which(os == campos[7], arr.ind = TRUE)

num_am <- os[indices_num_am[1], (indices_projeto[2]+2)]
projeto <- os[indices_projeto[1], (indices_projeto[2]+2)]
projeto <- os[indices_projeto[1], (indices_projeto[2]+2)]