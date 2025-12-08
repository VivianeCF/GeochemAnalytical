## Ensure required packages are installed and loaded
required_pkgs <- c("shiny")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    try(install.packages(pkg, repos = "https://cran.rstudio.com"), silent = TRUE)
  }
}
suppressPackageStartupMessages({
  lapply(required_pkgs, require, character.only = TRUE)
})

# allow larger uploads (200 MB)
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

ui <- fluidPage(
  titlePanel("Leitor de boletins - ACME / GEOSOL"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "upload",
        "Enviar um arquivo .zip com os boletins (apenas ZIP).",
        multiple = FALSE,
        accept = c(".zip")
      ),
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
      ),
      actionButton("run", "Ler boletins e preparar downloads"),
      actionButton("reset", "Nova leitura", class = "btn-secondary"),
      width = 4
    ),
    mainPanel(
      uiOutput("downloads_ui"),
      hr(),
      verbatimTextOutput("status")
    )
  )
)

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
        status_msg("Nenhum arquivo enviado. Envie um arquivo .zip com os boletins.")
        result(NULL)
        return()
      }
      up <- input$upload
      name <- up$name
      if (!grepl("\\.zip$", name, ignore.case = TRUE)) {
        status_msg("Arquivo inválido. Envie um arquivo .zip contendo os boletins.")
        result(NULL)
        return()
      }
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
      idx <- 1  # fallback to first class
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
    status_msg("Leitura concluída. Use os botões abaixo para baixar os CSVs.")
  })

  # allow starting a new reading: clear previous outputs and reset inputs
  observeEvent(input$reset, {
    result(NULL)
    status_msg("Aguardando execução...")
    selected_dir(NULL)
    updateRadioButtons(session, "lab", selected = "ACME")
    updateSelectInput(session, "classe_am", selected = classes[2])
  })

  output$downloads_ui <- renderUI({
    res <- result()
    if (is.null(res)) {
      return(NULL)
    }
    tagList(
      downloadButton("zip_all", "Baixar todos (.zip)")
    )
  })

  output$status <- renderText({
    status_msg()
  })

  # create download handlers dynamically when result changes
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
