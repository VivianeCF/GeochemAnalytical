## Garantir que os pacotes necessários estejam instalados e carregados
required_pkgs <- c("shiny", "shinydashboard", "DT")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    try(
      install.packages(pkg, repos = "https://cran.rstudio.com"),
      silent = TRUE
    )
  }
}
suppressPackageStartupMessages({
  lapply(required_pkgs, require, character.only = TRUE)
})

# permitir uploads maiores (200 MB)
options(shiny.maxRequestSize = 200 * 1024^2)

# Simple Shiny app to run le_boletim_acme / le_boletim_geosol
# and provide CSV downloads of the returned list elements.

if (file.exists("R/le_boletim_acme.R")) {
  source("R/le_boletim_acme.R")
}
if (file.exists("R/le_boletim_geosol.R")) {
  source("R/le_boletim_geosol.R")
}
if (file.exists("R/extrai_dados_os.R")) {
  source("R/extrai_dados_os.R")
}
if (file.exists("R/prepara_dados_geochem.R")) {
  source("R/prepara_dados_geochem.R")
}
# Cria um caminho de recurso explícito para a pasta www/
# O nome 'icons' é o que será usado na URL web
addResourcePath("icons", "www")

# class names used by both labs (index 1..5)
classes <- c(
  "Concentrado de bateia",
  "Sedimento de corrente",
  "Rocha",
  "Solo",
  "\u00c1gua"
)

# 🚀 NOVO: Obtém a data de modificação do arquivo app.R
# Assume-se que o arquivo Shiny está sendo executado como 'app.R'
app_file_path <- "app.R"
app_date_info <- tryCatch(
  {
    if (file.exists(app_file_path)) {
      format(file.info(app_file_path)$mtime, "%Y-%m-%d %H:%M:%S")
    } else {
      ""
    }
  },
  error = function(e) {
    "Erro ao ler data do arquivo"
  }
)


# --- Definição da Interface do Usuário (ui) com dashboardPage ---

ui <- dashboardPage(
  # 1. Cabeçalho (Header)
  dashboardHeader(title = "GeochemAnalytical"),

  # 2. Barra Lateral (Sidebar)
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Entrada de Dados",
        tabName = "tab_upload",
        icon = icon("file-upload")
      ),
      menuItem(
        "Configurações de leitura",
        tabName = "tab_leitura",
        icon = icon("sliders")
      ),
      menuItem(
        "Processamento",
        tabName = "tab_processamento",
        icon = icon("cogs")
      ),
      menuItem(
        "Visualização",
        tabName = "tab_visualizacao",
        icon = icon("table")
      ),
      menuItem(
        "Estatísticas",
        tabName = "tab_estatisticas",
        icon = icon("chart-bar")
      ),
      menuItem("Downloads", tabName = "tab_downloads", icon = icon("download")),
      menuItem("Sobre o App", tabName = "tab_sobre", icon = icon("info-circle"))
    )
  ),

  # 3. Corpo (Body)
  dashboardBody(
    tags$style(HTML(
      "
      #tab_sobre { margin-bottom: 10px; line-height: 1.5; text-align: justify; font-size: 1.5em; }
      .logo-image { display: block; margin-left: auto; margin-right: auto; }
    "
    )),

    tabItems(
      # --- TAB 1: ENTRADA DE DADOS (UPLOAD) ---
      tabItem(
        tabName = "tab_upload",
        h2("Upload de Arquivos"),
        fluidRow(
          # Box 1: Boletins
          box(
            title = "Boletins de Análise",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            fileInput(
              "upload",
              "Enviar arquivo .zip com os boletins.",
              multiple = FALSE,
              accept = c(".zip")
            ),
            p(em("O arquivo deve conter os boletins de análise."))
          ),

          # Box 2: Dados das OS (Novo item solicitado)
          box(
            title = "Dados das OS",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            fileInput(
              "upload_os",
              "Enviar arquivo .zip com dados das OS.",
              multiple = FALSE,
              accept = c(".zip")
            ),
            p(em("O arquivo deve conter as informações das Ordens de Serviço."))
          )
        )
      ),

      # --- AS DEMAIS TABS CONTINUAM IGUAIS AO SEU CÓDIGO ---
      # (tab_leitura, tab_processamento, tab_visualizacao, etc.)

      # Exemplo simplificado da continuação para contexto:
      # Sub-item: Configurações de Leitura
      tabItem(
        tabName = "tab_leitura",
        h2("Configurações do Processamento"),
        fluidRow(
          box(
            title = "Opções do Laboratório e Amostra",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            radioButtons(
              "tipo_analise",
              "Tipo de análise",
              choices = c("Química", "Mineralógica"),
              selected = "Química"
            ),
            radioButtons(
              "lab",
              "Laboratório",
              choices = c( "GEOSOL", "ACME"),
              selected = "GEOSOL"
            ),
            selectInput(
              "classe_am",
              "Classe da amostra",
              choices = classes,
              selected = classes[2]
            )
          )
        )
      ),

      # Sub-item: Ação e Status
      tabItem(
        tabName = "tab_processamento",
        h2("Execução e Status"),
        fluidRow(
          box(
            title = "Controle e Status",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p(
              "Clique em 'Processar' após configurar as opções e carregar o arquivo ZIP."
            ),
            actionButton(
              "run",
              "Processar Boletins",
              icon = icon("sync-alt"),
              class = "btn-success"
            ),
            actionButton(
              "reset",
              "Nova Leitura",
              icon = icon("undo"),
              class = "btn-secondary"
            ),
            hr(),
            strong("Status da Execução:"),
            verbatimTextOutput("status")
          )
        )
      ),

      # --- TAB 3: VISUALIZAÇÃO ---
      tabItem(
        tabName = "tab_visualizacao",
        h2("Visualização dos Dados Processados"),
        uiOutput("data_tables_ui")
      ),

      # --- TAB 4: ESTATÍSTICAS ---
      tabItem(
        tabName = "tab_estatisticas",
        h2("Estatísticas e Resumo"),
        box(
          title = "Contagem de Amostras por Boletim",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          uiOutput("stats_ui")
        )
      ),

      # --- TAB 5: DOWNLOADS ---
      tabItem(
        tabName = "tab_downloads",
        h2("Baixar Resultados"),
        box(
          title = "Arquivos Processados",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          p(
            "Baixe todos os arquivos de saída (dados, meta-dados, etc.) em um único arquivo ZIP. Os arquivos CSV usam o formato Latin-1."
          ),
          uiOutput("downloads_ui")
        )
      ),

      # --- TAB 6: SOBRE O APP (CONTEÚDO ATUALIZADO) ---
      tabItem(
        tabName = "tab_sobre",
        img(
          src = "icons/logo.jpg",
          width = 100,
          units = "%",
          alt = "Sua Foto",
          class = "logo-image"
        ),

        h2("Informações sobre o Aplicativo"),
        fluidRow(
          box(
            title = "Objetivo",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p(
              "Este aplicativo Shiny tem como objetivo simplificar a leitura e o processamento de boletins de análise geoquímica (química) dos laboratórios ACME e GEOSOL, transformando os resultados brutos em dados tabulares prontos para análise e visualização."
            ),
            p(
              "O processamento é realizado pelas funções R `le_boletim_quimica_acme` e `le_boletim_quimica_geosol`."
            )
          ),
          box(
            title = "Desenvolvedor/Versão",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tags$ul(
              tags$li(strong("Data do arquivo app.R:"), app_date_info), # 💡 Inserção da data aqui
              tags$li(
                strong("Pacotes utilizados:"),
                " Shiny, shinydashboard, DT."
              ),
              tags$li(
                strong("Contato:"),
                "Viviane Carillo Ferrari/Serviço Geológico do Brasil (SGB-CPRM) (mailto:viviane.ferrari@sgb.gov.br"
              )
            )
          )
        )
      )
    )
  )
)


# --- Definição do Servidor (server) ---
server <- function(input, output, session) {
  
  
  result <- reactiveVal(NULL)
  status_msg <- reactiveVal("Aguardando execução...")
  selected_dir <- reactiveVal(NULL)
  last_meta <- reactiveVal(list(classe = NA, lab = NA))
  
  # CORREÇÃO 1: Usar reactiveVal para caminhos temporários da sessão
  session_temp_paths <- reactiveVal(NULL)

  # Garante que os arquivos temporários sejam removidos ao fechar a sessão (Backup)
  session$onSessionEnded(function() {
    td_list <- isolate(session_temp_paths())
    if (!is.null(td_list)) {
      for (td in td_list) {
        if (!is.na(td) && dir.exists(td)) {
          try(unlink(td, recursive = TRUE, force = TRUE), silent = TRUE)
        }
      }
    }
  })

  resolve_dir <- function(path) {
    path <- trimws(path)
    if (identical(path, "") || is.na(path)) {
      return(NA_character_)
    }

    # remove surrounding quotes
    path <- gsub('^"|"$|^\'|\'$', "", path)

    # strip common URL/file prefixes
    path <- sub('^file:///', '', path)
    path <- sub('^file://', '', path)
    path <- sub('^file:', '', path)

    # decode percent-encoding (e.g. %20 -> space)
    path <- utils::URLdecode(path)

    # unify backslashes to forward slashes
    path <- gsub('\\\\', '/', path)

    # try direct normalization
    cand <- tryCatch(
      normalizePath(path, winslash = "/", mustWork = FALSE),
      error = function(e) NA_character_
    )
    if (!is.na(cand)) {
      if (dir.exists(cand)) {
        return(cand)
      }
      if (file.exists(cand)) return(dirname(cand))
    }

    # try relative to app_root
    cand2 <- tryCatch(
      normalizePath(
        file.path(app_root, path),
        winslash = "/",
        mustWork = FALSE
      ),
      error = function(e) NA_character_
    )
    if (!is.na(cand2)) {
      if (dir.exists(cand2)) {
        return(cand2)
      }
      if (file.exists(cand2)) return(dirname(cand2))
    }

    # try trimming trailing slashes and try again
    path_trim <- sub('[/\\\\]+$', '', path)
    cand3 <- tryCatch(
      normalizePath(
        file.path(app_root, path_trim),
        winslash = "/",
        mustWork = FALSE
      ),
      error = function(e) NA_character_
    )
    if (!is.na(cand3)) {
      if (dir.exists(cand3)) {
        return(cand3)
      }
      if (file.exists(cand3)) return(dirname(cand3))
    }

    return(NA_character_)
  }

  observeEvent(input$tipo_analise, {
  if (input$tipo_analise == "Mineralógica") {
    # --- Lógica para Mineralógica ---
    updateRadioButtons(session, "lab",
      choices = "GEOSOL",
      selected = "GEOSOL"
    )
    
    updateSelectInput(session, "classe_am",
      choices = "Concentrado de bateia",
      selected = "Concentrado de bateia"
    )
    
  } else {
    # --- Lógica para Química ---
    # 1. Restaura os laboratórios
    updateRadioButtons(session, "lab",
      choices = c("GEOSOL","ACME" ),
      selected = "GEOSOL"
    )
    
    # 2. Filtra o vetor classes: remove "Concentrado de bateia"
    # O comando abaixo pega todas as classes EXCETO a de bateia
    classes_quimica <- classes[classes != "Concentrado de bateia"]
    
    updateSelectInput(session, "classe_am",
      choices = classes_quimica,
      selected = classes_quimica[1] # Seleciona a primeira opção válida
    )
  }
})
  
  observeEvent(input$run, {
    if (is.null(input$upload) || is.null(input$upload_os)) {
      status_msg("Erro: Você deve enviar tanto o arquivo dos Boletins quanto o das OS.")
      result(NULL)
      return()
    }

    # CORREÇÃO 2: Pasta mestre ÚNICA para esta execução específica
    # tempfile() cria um nome aleatório impossível de colidir com outra sessão
    main_out <- tempfile(pattern = "proc_") 
    dir_bol_out <- file.path(main_out, "boletins")
    dir_os_out  <- file.path(main_out, "os")
    dir_geochem_out  <- file.path(main_out, "geochem")
    
    dir.create(dir_bol_out, recursive = TRUE)
    dir.create(dir_os_out, recursive = TRUE)
    dir.create(dir_geochem_out, recursive = TRUE)

    # Pastas para descompactação dos uploads
    td_up <- tempfile("up_bol_")
    td_os <- tempfile("up_os_")
    dir.create(td_up, recursive = TRUE)
    dir.create(td_os, recursive = TRUE)

    utils::unzip(input$upload$datapath, exdir = td_up)
    utils::unzip(input$upload_os$datapath, exdir = td_os)

    # Atualiza a lista de caminhos para limpeza automática
    session_temp_paths(c(td_up, td_os, main_out))

    # Define qual função de laboratório chamar
fun_name <- if (input$lab == "ACME") "le_boletim_acme" else "le_boletim_geosol"
classe_use <- input$classe_am
dir_bol <- td_up # Onde os boletins foram descompactados

    # --- EXECUÇÃO (Simplificado para foco no problema de arquivos) ---
res <- tryCatch({
      # 1. Extrair dados das OS
      if (!exists("extrai_dados_os")) stop("Função 'extrai_dados_os' não carregada.")
      
      dados_os_processados <- extrai_dados_os(
        dir_os = td_os,
        dir_out = dir_os_out,
        tipo = input$tipo_analise
      )

      # 2. Rodar a função principal dos boletins
      dados_boletins <- do.call(
        get(fun_name),
        list(
          classe_am = classe_use,
          dir_bol   = dir_bol,
          dir_ucc   = "inputs/ucc/",
          ref_ucc   = "ucc.csv",
          dir_out   = dir_bol_out,
          tipo = input$tipo_analise
        )
      )

      # 3. Processar Geochem (Nova Função)
      if (!exists("prepara_dados_geochem")) stop("Função 'prepara_dados_geochem' não carregada.")
      
      dados_geochem_processados <- prepara_dados_geochem(
        dir_out     = dir_geochem_out, 
        info_os     = dados_os_processados, 
        ca          = dados_boletins[['condições de análise']], 
        dados_pivo  = dados_boletins[['dados transformados pivotados']],
        classe_am = classe_use
      )

      # 4. Montar a lista final para o result()
      # IMPORTANTE: Mantemos o nome 'dados' para a tabela principal para não quebrar as Estatísticas
      
      # --- 3. Montar a lista final para o result() ---

# Inicializamos a lista que será enviada para o result()
lista_final <- list()

# Parte A: Lidar com os Boletins
if (is.data.frame(dados_boletins)) {
  # Se for um dataframe único, chamamos de 'dados' para manter compatibilidade com a aba Estatística
  lista_final$dados <- dados_boletins
} else if (is.list(dados_boletins)) {
  # Se for uma lista (caso da Geosol/Acme que retornam várias tabelas)
  lista_final <- dados_boletins
  # Garante que o primeiro item se chame 'dados' para as estatísticas não quebrarem
  if (length(names(lista_final)) > 0 && names(lista_final)[1] != "dados") {
    names(lista_final)[1] <- "dados"
  }
}

# Parte B: Incorporar a Geochem (Desmembrando a lista 'out' em 4 abas)
# Usamos c() para concatenar as duas listas. Isso faz com que cada um dos 4 nomes:
# "amostras e resultados...", "estações...", etc., vire um item da lista_final.
if (is.list(dados_geochem_processados)) {
  lista_final <- c(lista_final, dados_geochem_processados)
} else {
  lista_final$dados_geochem <- dados_geochem_processados
}

# Parte C: Adicionar a Info OS
lista_final$info_os <- dados_os_processados

# Retorno do tryCatch
lista_final
    }, error = function(e) e)
# --- TRATAMENTO DE ERRO E RESULTADO ---
    if (inherits(res, "error")) {
      status_msg(paste0("Erro no processamento: ", res$message))
      result(NULL)
    } else {
      # Se a função retornar múltiplos objetos, garanta que res seja uma lista nomeada
      if (is.data.frame(res)) {
        res <- list(dados = res)
      }

      # Se quiser incluir a tabela de OS na visualização final:
      # res$info_os <- dados_os_processados

      result(res)
      last_meta(list(classe = classe_use, lab = input$lab))
      status_msg("Processamento concluído com sucesso!")
    }
  })
  # allow starting a new reading: clear previous outputs and reset inputs
  observeEvent(input$reset, {
    result(NULL)
    status_msg("Aguardando execução...")
    selected_dir(NULL)
    updateRadioButtons(session, "lab", selected = "ACME")
    updateSelectInput(session, "classe_am", selected = classes[2])

    # Voltar para a aba de upload
    updateTabItems(session, "tabs", selected = "tab_upload")
  })

  # Downloads UI
  output$downloads_ui <- renderUI({
    res <- result()
    if (is.null(res)) {
      return(p(
        "Processamento pendente. Os dados estarão disponíveis aqui após a execução."
      ))
    }
    tagList(
      downloadButton("zip_all", "Baixar todos (.zip)", class = "btn-info")
    )
  })

  output$status <- renderText({
    status_msg()
  })

  # 1. VISUALIZAÇÃO (Tabelas)
output$data_tables_ui <- renderUI({
    res <- result()
    if (is.null(res)) return(p("Execute o processamento para visualizar as tabelas."))

    # Filtramos apenas o que é data.frame
    nomes_tabelas <- names(res)[sapply(res, is.data.frame)]
    
    tabs <- lapply(nomes_tabelas, function(nm) {
      # Higieniza o nome para o ID (remove espaços e acentos)
      output_name <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", nm))
      
      # Renderiza a DataTable dinamicamente
      output[[output_name]] <- DT::renderDataTable({
        res[[nm]]
      }, options = list(pageLength = 10, scrollX = TRUE), server = TRUE)

      tabPanel(
        title = toupper(nm), # Título da aba em caixa alta para melhor leitura
        DT::dataTableOutput(output_name)
      )
    })

    do.call(tabsetPanel, c(tabs, id = "dynamic_tables"))
  })

  # 2. ESTATÍSTICAS (Contagem)
  output$stats_ui <- renderUI({
    res <- result()
    if (is.null(res)) {
      return(p("Execute o processamento para gerar estatísticas."))
    }

    # Assume que a principal tabela de dados é 'dados' ou a primeira
    main_data <- NULL
    if ("dados" %in% names(res)) {
      main_data <- res[["dados"]]
    } else if (length(res) > 0 && is.data.frame(res[[1]])) {
      main_data <- res[[1]]
    }

    if (is.null(main_data)) {
      return(p(
        "Dados processados não encontrados ou não estão no formato de tabela."
      ))
    }

    # Colunas de interesse: Boletim (para agrupar) e NUM_LAB (para identificar amostras únicas)

    # 1. Identificação da Coluna de Boletim
    bol_col <- intersect(c("Boletim", "BULLETIN"), names(main_data))

    if (length(bol_col) == 0) {
      return(p(
        "Coluna 'Boletim' ou 'BULLETIN' não encontrada para calcular estatísticas de contagem."
      ))
    }
    bol_col <- bol_col[1] # Seleciona a coluna de Boletim

    # 2. Identificação da Coluna de Identificador da Amostra (NUM_LAB)
    lab_col <- intersect(c("NUM_LAB"), names(main_data))

    if (length(lab_col) == 0) {
      return(p(
        "Coluna 'NUM_LAB' não encontrada para calcular o número de amostras únicas."
      ))
    }
    lab_col <- lab_col[1] # Seleciona a coluna NUM_LAB

    # 3. Contagem de Amostras Únicas (NUM_LAB distintos) por Boletim

    if (requireNamespace("dplyr", quietly = TRUE)) {
      # 💡 CORREÇÃO APLICADA: Usando dplyr::n_distinct()
      stats_data <- main_data %>%
        dplyr::group_by(!!rlang::sym(bol_col)) %>%
        # Conta o número de valores únicos na coluna NUM_LAB dentro de cada grupo (Boletim)
        dplyr::summarise(
          N_Amostras_Unicas = dplyr::n_distinct(!!rlang::sym(lab_col)),
          .groups = 'drop'
        )
    } else {
      # Fallback usando aggregate e tapply (mais complexo, mas funciona sem dplyr)
      count_unique <- tapply(
        X = main_data[[lab_col]],
        INDEX = main_data[[bol_col]],
        FUN = function(x) length(unique(x))
      )
      stats_data <- data.frame(
        Boletim = names(count_unique),
        N_Amostras_Unicas = as.numeric(count_unique),
        stringsAsFactors = FALSE
      )
      names(stats_data)[1] <- bol_col
    }

    # 4. Cálculo dos Totais

    # Número de Boletins: Número de linhas na tabela stats_data
    total_boletins <- nrow(stats_data)

    # Número Total de Amostras Únicas (sem repetições): Contagem de NUM_LAB distintos em todo o conjunto de dados
    total_amostras_unicas <- length(unique(main_data[[lab_col]]))

    output$count_table <- DT::renderDataTable(
      stats_data,
      options = list(pageLength = 10),
      rownames = FALSE
    )

    return(
      tagList(
        h4("Contagem Detalhada de Amostras Únicas por Boletim"),
        DT::dataTableOutput("count_table"),
        hr(),
        h4("Resumo Estatístico"),
        p(strong("Total de Boletins Processados:"), total_boletins),
        p(
          strong("Total de Amostras Únicas Processadas (NUM_LAB distintos):"),
          total_amostras_unicas
        )
      )
    )
  })
  # download handler for zip of all outputs (mantido inalterado)
  observeEvent(result(), {
    res <- result()
    if (is.null(res) || inherits(res, "error")) return()

    output$zip_all <- downloadHandler(
      filename = function() {
        # ... (sua lógica de nome de arquivo continua a mesma) ...
        paste0("processamento_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip")
      },
      content = function(zipfile) {
        # Busca o caminho exato gerado nesta execução
        all_paths <- isolate(session_temp_paths())
        main_temp_dir <- all_paths[3] # O main_out definido no input$run
        
        if (is.null(main_temp_dir) || !dir.exists(main_temp_dir)) {
          stop("Erro: Diretório de saída não encontrado.")
        }

        old_wd <- getwd()
        # on.exit garante que o WD volte ao normal E apague os temporários após o ZIP
        on.exit({
          setwd(old_wd)
          # Opcional: apagar após o download para não acumular no servidor
          # unlink(main_temp_dir, recursive = TRUE) 
        }, add = TRUE)
        
        setwd(main_temp_dir)
        
        # Pega apenas os arquivos desta sessão
        files_to_zip <- list.files(recursive = TRUE)
        
        if (length(files_to_zip) == 0) {
          writeLines("Sem dados processados", "vazio.txt")
          files_to_zip <- "vazio.txt"
        }

        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zip(zipfile, files = files_to_zip)
        } else {
          utils::zip(zipfile, files = files_to_zip)
        }
      }
    )
  })
}

shinyApp(ui, server)
