
# 2. PREPARA OS DADOS ANALITICOS ---------------------------------------
## Define os diretórios
pastas_alvos <- "inputs/boletins_raw/acme/sedimento de corrente/"

## 1.1 Dados Analíticos
source("R/le_boletim_acme.R")
dir_out <- "outputs/base_geoquimica/acme/"
base_acme <- le_boletim_quimica_acme(classe_am = 2, dir_bol = pastas_alvos , 
                   dir_ucc = "inputs/ucc/", ref_ucc = "ucc.csv", dir_out= dir_out)

pastas_alvos <- "inputs/boletins_raw/geosol/rocha/"

## 1.1 Dados Analíticos
source("R/le_boletim_geosol.R")
dir_out <- "outputs/base_geoquimica/geosol/"
base_geosol <- le_boletim_quimica_geosol(classe_am = 3, dir_bol = pastas_alvos , 
                   dir_ucc = "inputs/ucc/", ref_ucc = "ucc.csv", dir_out= dir_out)

