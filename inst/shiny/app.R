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

      # 1. Entrada de Dados
      menuItem(
        "Entrada de Dados",
        tabName = "tab_upload",
        icon = icon("file-upload")
      ),

      # 2. Processamento
      menuItem(
        "Processamento",
        tabName = "tab_processamento",
        icon = icon("cogs"),
        menuSubItem(
          "Configurações de Leitura",
          tabName = "sub_leitura",
          icon = icon("sliders")
        ),
        menuSubItem(
          "Ação e Status",
          tabName = "sub_status",
          icon = icon("play")
        )
      ),

      # 3. Visualização
      menuItem(
        "Visualização",
        tabName = "tab_visualizacao",
        icon = icon("table")
      ),

      # 4. Estatísticas
      menuItem(
        "Estatísticas",
        tabName = "tab_estatisticas",
        icon = icon("chart-bar")
      ),

      # 5. Downloads
      menuItem("Downloads", tabName = "tab_downloads", icon = icon("download")),

      # 6. Sobre o App
      menuItem("Sobre o App", tabName = "tab_sobre", icon = icon("info-circle"))
    )
  ),

  # 3. Corpo (Body)
  dashboardBody(
    tabItems(
      # --- TAB 1: ENTRADA DE DADOS (UPLOAD) ---
      tabItem(
        tabName = "tab_upload",
        h2("Upload do Arquivo ZIP"),
        box(
          title = "Selecione o Arquivo",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          fileInput(
            "upload",
            "Enviar um arquivo .zip com os boletins (apenas ZIP).",
            multiple = FALSE,
            accept = c(".zip")
          ),
          p(em(
            "O arquivo ZIP deve conter os boletins de análise no formato adequado."
          ))
        )
      ),

      # --- TAB 2: PROCESSAMENTO (SUB-ITENS) ---

      # Sub-item: Configurações de Leitura
      tabItem(
        tabName = "sub_leitura",
        h2("Configurações do Processamento"),
        fluidRow(
          box(
            title = "Opções do Laboratório e Amostra",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            radioButtons(
              "lab",
              "Laboratório",
              choices = c("ACME", "GEOSOL"),
              selected = "ACME"
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
        tabName = "sub_status",
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
  uploaded_temp_dir_path <- NULL
  last_meta <- reactiveVal(list(classe = NA, lab = NA))

  # ensure uploaded temp files are removed when session ends
  session$onSessionEnded(function() {
    td <- uploaded_temp_dir_path
    if (!is.null(td) && dir.exists(td)) {
      try(unlink(td, recursive = TRUE, force = TRUE), silent = TRUE)
    }
  })

  # root used to resolve relative paths (app working directory at launch)
  app_root <- normalizePath(".", winslash = "/", mustWork = FALSE)

  # (função resolve_dir omitida por brevidade, mas deve ser mantida)
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

  observeEvent(input$run, {
    # Expect a single ZIP upload; do not accept server-side paths
    if (is.null(input$upload) || nrow(input$upload) == 0) {
      status_msg(
        "Nenhum arquivo enviado. Envie um arquivo .zip com os boletins."
      )
      result(NULL)
      return()
    }
    up <- input$upload
    name <- up$name
    if (!grepl("\\.zip$", name, ignore.case = TRUE)) {
      status_msg(
        "Arquivo inválido. Envie um arquivo .zip contendo os boletins."
      )
      result(NULL)
      return()
    }

    # Mover para a aba de status após o clique
    updateTabItems(session, "tabs", selected = "sub_status")

    td_up <- tempfile("uploaded_boletins")
    dir.create(td_up, recursive = TRUE, showWarnings = FALSE)
    uploaded_temp_dir_path <<- td_up
    utils::unzip(up$datapath, exdir = td_up)
    dir_bol <- normalizePath(td_up, winslash = "/", mustWork = FALSE)
    # ensure directory path ends with a slash to avoid filename concatenation
    if (dir.exists(dir_bol)) {
      dir_bol <- normalizePath(dir_bol, winslash = "/", mustWork = FALSE)
      if (!grepl("/$", dir_bol)) dir_bol <- paste0(dir_bol, "/")
    }

    # choose function
    if (input$lab == "ACME") {
      fun_name <- "le_boletim_quimica_acme"
    } else {
      fun_name <- "le_boletim_quimica_geosol"
    }

    if (!exists(fun_name)) {
      status_msg(paste0("Função não encontrada no ambiente: ", fun_name))
      result(NULL)
      return()
    }

    # map selected class name to its index
    sel_name <- input$classe_am
    idx <- match(sel_name, classes)
    if (is.na(idx)) {
      idx <- 1 # fallback to first class
    }
    classe_use <- idx

    status_msg("Lendo boletins (isso pode demorar)...")

    # call function safely; write outputs to tempdir to avoid polluting repo
    res <- tryCatch(
      {
        do.call(
          get(fun_name),
          list(
            classe_am = classe_use,
            dir_bol = dir_bol,
            dir_ucc = "inputs/ucc/",
            ref_ucc = "ucc.csv",
            dir_out = tempdir()
          )
        )
      },
      error = function(e) {
        e
      }
    )

    if (inherits(res, "error")) {
      status_msg(paste0("Erro ao ler boletins: ", res$message))
      result(NULL)
      return()
    }

    # ensure the result is a named list so we can offer downloads
    if (
      is.data.frame(res) ||
        (requireNamespace("tibble", quietly = TRUE) && tibble::is_tibble(res))
    ) {
      res <- list(dados = res)
    }

    if (is.null(names(res)) || any(names(res) == "")) {
      names(res) <- paste0("saida_", seq_along(res))
    }

    result(res)
    # store metadata for naming (classe used and lab)
    last_meta(list(classe = classe_use, lab = input$lab))
    status_msg(
      "Leitura concluída. Os downloads, tabelas e estatísticas estão disponíveis nas abas laterais."
    )
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
    if (is.null(res)) {
      return(
        p(
          "Execute o processamento na aba 'Processamento' para visualizar as tabelas."
        )
      )
    }

    # Cria uma lista de outputs de DataTable
    tabs <- lapply(names(res), function(nm) {
      data <- res[[nm]]
      if (is.data.frame(data)) {
        # Criar o output de renderização da tabela
        output_name <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", nm))
        output[[output_name]] <- DT::renderDataTable(
          data,
          options = list(pageLength = 10, scrollX = TRUE),
          server = FALSE # Processamento no cliente
        )

        return(tabPanel(
          title = nm,
          DT::dataTableOutput(output_name)
        ))
      }
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

    # Tenta calcular estatísticas, assumindo que há uma coluna de ID de boletim
    if ("Boletim" %in% names(main_data) || "BULLETIN" %in% names(main_data)) {
      bol_col <- intersect(c("Boletim", "BULLETIN"), names(main_data))[1]

      # Tabela de contagem: Boletim e Número de Amostras
      if (requireNamespace("dplyr", quietly = TRUE)) {
        stats_data <- main_data %>%
          dplyr::group_by(!!rlang::sym(bol_col)) %>%
          dplyr::summarise(N_Amostras = dplyr::n(), .groups = 'drop')
      } else {
        stats_data <- aggregate(
          x = rep(1, nrow(main_data)),
          by = list(Boletim = main_data[[bol_col]]),
          FUN = length
        )
        names(stats_data) <- c("Boletim", "N_Amostras")
      }

      output$count_table <- DT::renderDataTable(
        stats_data,
        options = list(pageLength = 10),
        rownames = FALSE
      )

      return(
        tagList(
          DT::dataTableOutput("count_table"),
          hr(),
          p(paste("Total de boletins processados:", nrow(stats_data))),
          p(paste("Total de amostras processadas:", sum(stats_data$N_Amostras)))
        )
      )
    } else {
      return(p(
        "Coluna 'Boletim' não encontrada para calcular estatísticas de contagem."
      ))
    }
  })

  # download handler for zip of all outputs (mantido inalterado)
  observeEvent(
    result(),
    {
      res <- result()
      if (is.null(res)) {
        return()
      }
      # download handler for zip of all outputs
      output$zip_all <- downloadHandler(
        filename = function() {
          meta <- last_meta()
          # derive human-readable class label when possible
          classe_label <- "classeNA"
          if (!is.null(meta$classe) && !is.na(meta$classe)) {
            clv <- meta$classe
            if (
              !is.null(meta$lab) &&
                toupper(meta$lab) %in% c("GEOSOL", "ACME") &&
                clv >= 1 &&
                clv <= length(classes)
            ) {
              raw <- classes[as.integer(clv)]
              # remove accents and convert to ascii-friendly slug
              slug <- iconv(raw, from = "UTF-8", to = "ASCII//TRANSLIT")
              slug <- gsub("[^A-Za-z0-9]+", "_", slug)
              slug <- tolower(gsub("_+", "_", slug))
              slug <- sub("^_+|_+$", "", slug)
              classe_label <- slug
            } else {
              classe_label <- paste0("classe", clv)
            }
          }
          # include lab slug
          lab_slug <- "lab"
          if (!is.null(meta$lab) && !is.na(meta$lab)) {
            lab_raw <- tolower(as.character(meta$lab))
            lab_slug <- iconv(lab_raw, from = "UTF-8", to = "ASCII//TRANSLIT")
            lab_slug <- gsub("[^a-z0-9]+", "_", lab_slug)
            lab_slug <- sub("^_+|_+$", "", lab_slug)
          }
          date_tag <- format(Sys.time(), "%Y%m%d")
          paste0(
            "boletins_",
            classe_label,
            "_",
            lab_slug,
            "_",
            date_tag,
            ".zip"
          )
        },
        content = function(zipfile) {
          enc <- "latin1"
          td <- tempfile("zip_files")
          dir.create(td, showWarnings = FALSE)
          files <- character()
          for (ii in seq_along(res)) {
            nm <- names(res)[ii]
            x <- res[[ii]]
            if (
              is.data.frame(x) ||
                (requireNamespace("tibble", quietly = TRUE) &&
                  tibble::is_tibble(x))
            ) {
              # convert encoding if requested
              if (identical(enc, "latin1")) {
                x <- as.data.frame(
                  lapply(x, function(col) {
                    if (is.character(col)) {
                      converted <- iconv(
                        col,
                        from = "UTF-8",
                        to = "latin1",
                        sub = ""
                      )
                      na_idx <- is.na(converted) & !is.na(col)
                      if (any(na_idx)) {
                        converted[na_idx] <- iconv(
                          col[na_idx],
                          from = "UTF-8",
                          to = "latin1",
                          sub = "byte"
                        )
                      }
                      return(converted)
                    }
                    col
                  }),
                  stringsAsFactors = FALSE
                )
              }
              f <- file.path(td, paste0(gsub("[[:space:]]+", "_", nm), ".csv"))
              utils::write.csv2(x, f, row.names = FALSE, fileEncoding = enc)
              files <- c(files, f)
            } else {
              # include RDS for non-dataframes
              f <- file.path(td, paste0(gsub("[[:space:]]+", "_", nm), ".rds"))
              saveRDS(x, f)
              files <- c(files, f)
            }
          }

          # create zip
          if (requireNamespace("zip", quietly = TRUE)) {
            zip::zip(zipfile, files, mode = "cherry-pick")
          } else {
            # try utils::zip; use -j to junk paths when available
            owd <- getwd()
            on.exit(setwd(owd), add = TRUE)
            setwd(td)
            utils::zip(zipfile, files = basename(files), flags = "-j")
          }
          # cleanup temporary files used to assemble the zip
          try(unlink(td, recursive = TRUE, force = TRUE), silent = TRUE)
        }
      )

      # individual download handlers removed — only ZIP download is provided
    },
    ignoreNULL = TRUE
  )
}

shinyApp(ui, server)
