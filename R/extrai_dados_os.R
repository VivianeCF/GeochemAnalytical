extrai_dados_os <- function(
  dir_os,
  dir_out,
  projeto_nome,
  centro_custo){
   ## Gera o camionho para os arquivos
  ## Entrada
  files_os <- list.files(dir_os, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE, 
  ignore.case = TRUE)
if (length(files_os) == 0) {
    stop("Nenhum arquivo .xlsx encontrado na pasta das boletins.")
  }

  lista_os <- list()
  for (i in 1:length(files_os)) {
    lista_os[[i]] = readxl::read_excel(files_os[i], col_names = TRUE, skip = 11, sheet = 2)
    # LE Tabelas do arquivo de amostras da litoteca - projeto Eldorado
}

dados_amostras <- do.call(rbind, lista_os)
nomes_colunas <- c("ORDEM", "LONG",	"LAT",	"Número de Campo", 	"litotipo",
 "Número de Lote",	"Número Laboratório",
  	"Tipo de amostra", "Matriz", "Peso da amostra (g)", "PRP102_E-A (R$/amostra)",	
    "PRP102_E-B (R$/kg)", "PRP102_Y-A (R$/amostra)", "PRP102_Y-B (R$/kg)",
     "PREPS80P-A (R$/amostra)",	 "PREPS80P-B (R$/kg)",
         	"DUP_Brita", "DUP_Polpa",	 "PRP250", 	"PRPGA",	"LOG03",
          "PHY04V",	"DRY10",	"CCR",	"CCR_NL",	"CCR_GR",
          "CCR_Mn",	"CCR_Fe",	"CCR_PO",	"CCR_PTR",	"CCR_LS",	
          "CCR_PL",	"CCSS I",	"CCSS II", "XRF79C_13",	"XRF72FE",
        	"XRF72LS",	"XRF72PO",	"XRF72PTR",	"XRF82MN",	"XRF82GR",
        	"XRF76R",	"XRF80B",	"XRF82CR",	"IMS95A",	"ICP95A/IMS95A",	
          "ICP95A",	"IMS95RS",	"ICM40B",	"ICM14B_10", "CSA17V_C",
          "GC_CSA17V_C",	"CSA03V",	"CSA02V",	"CSA05V",	"CSA20V",	
          "CSA17V_S",	"GC_CSA17V_S",	"CLA80C",	"CLA70C",	"ISE03A",	
          "XRF75V",	"ICM90A_B",	"ICP40B",	"ICP40B_S",	"ICP90A_S",	"XRF83B",	
          "XRF72NL",	"XRF72BX",	"ICP05V",	"ICP90A",	"ICM90A",	"ICM42Q",	
          "FAA313 / FAA323",	"FAI515",	"Material de referência utilizado pelo laboratório p/ XRF", "Massa mínima de amostra")

colnames(dados_amostras) <- nomes_colunas
  
dados_amostras$C.C <- centro_custo
dados_amostras$PROJETO <- projeto_nome
remover <- c("Número de amostras por método", "R$ por método", "R$ total do lote", "Número total de amostras no lote")  
  
dados_amostras <- dados_amostras  |> dplyr::filter(!(ORDEM %in% remover) )
  dados_amostras <- dados_amostras |>
    dplyr::select(where(~ !all(is.na(.))))
primeiras_colunas <- c("ORDEM", "LONGITUDE", "LATITUDE", "NUM_CAMPO", "LOTE", "NUM_LAB",  "CLASSE")  
colnames(dados_amostras)[1:7] <- primeiras_colunas

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
# dados_amostras <- unique(dados_amostras)
return(dados_amostras)
}