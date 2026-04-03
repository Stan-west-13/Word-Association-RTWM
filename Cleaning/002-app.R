library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)
library(shiny)
library(DT)
library(hunspell)

root_dir <- "C:/Users/westa/Desktop/Word-Association-RTWM/"

source(file.path(root_dir, "R/dbInsertHelpers.R"))
source(file.path(root_dir, "R/dbLockHelpers.R"))
source(file.path(root_dir, "R/dbSelectHelpers.R"))

db_path <- file.path(root_dir, "/Cleaning/databases/TTA2_WordAssociation-DB.db")

shinyApp(
  ui = fluidPage(
    shiny::titlePanel("Mapping responses to metadata"),
    shiny::fluidRow(
      shiny::column(4, shiny::tableOutput("current_response")),
      #shiny::column(2, shiny::numericInput("cue_response_id", label = "Cue-Response ID", value = NA, min = 1, max = 11195), offset = 1)
    ),
    shiny::textInput("search_pattern", "Search pattern"),
    DT::dataTableOutput("search_results"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectizeInput("kuperman_map", label = "Kuperman", choices = NULL)),
      shiny::column(4, shiny::selectizeInput("subtlex_map", label = "SUBTLEX", choices = NULL)),
      shiny::column(4, shiny::selectizeInput("cue_map", label = "Word Association Cue", choices = NULL))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::textInput("revision", "Revision")),
      shiny::column(4, shiny::selectizeInput(
        inputId = "researcher_id",
        label = "Researcher",
        choices = NULL
      )),
      shiny::column(4,
                    shiny::actionButton("commit", "Commit", class = "btn-primary"),
                    shiny::actionButton("reset", "Reset", class = "btn-warning"),
                    shiny::actionButton("quit", "Quit", class = "btn-danger")
      )
    ),
    shiny::wellPanel(
      h4("Spelling Suggestions"),
      textOutput("spelling_suggestions"),
      h4("Stem suggestions"),
      textOutput("stem_suggestions")
    )
  ),
  server = function(input, output, session) {
    
    rv <- reactiveValues(
      lock_hash = NA,
      meta = NA,
      resp = NA
    )
    
    {
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con))
      rv$resp <- dbSelectOneUnmappedCueResponse(con)
    }
    
    observeEvent(input$cue_response_id, {
     if (dbIsLockedResponse(con, input$cue_response_id)) {
       showNotification("That cue-response is LOCKED.")
     } else {
       tmp <- dbSelectOneCueResponse(con, input$cue_response_id)
       str(tmp)
       rv$resp <- tmp
     }
    })
    
    observe({
      updateTextInput(session, "revision", value = rv$resp$response)
      updateTextInput(session, "search_pattern", value = paste0(str_sub(rv$resp$response, end = -3), "%"))
    })
    
    updateSelectizeInput(
      session, "researcher_id",
      choices = dbSafeQuery(db_path, dbSelectResearcherIDs, selectize_placeholder = TRUE),
      server = TRUE
    )
    
    observe({
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con))
      meta <- dbSelectMetadata(con, pattern = input$search_pattern)
      placeholder <- c("..." = "")
      choices <- list(
        kuperman = meta$kuperman_id[!is.na(meta$kuperman_id)],
        subtlex = meta$subtlex_id[!is.na(meta$subtlex_id)],
        cue = meta$cue_id[!is.na(meta$cue_id)]
      )
      names(choices$kuperman) <- meta$word[!is.na(meta$kuperman_id)]
      names(choices$subtlex) <- meta$word[!is.na(meta$subtlex_id)]
      names(choices$cue) <- meta$word[!is.na(meta$cue_id)]
      choices <- lapply(choices, function(x) c(placeholder, x))
      
      updateSelectizeInput(
        session, "kuperman_map",
        choices = choices$kuperman,
        server = TRUE
      )
      updateSelectizeInput(
        session, "subtlex_map",
        choices = choices$subtlex,
        server = TRUE
      )
      updateSelectizeInput(
        session, "cue_map",
        choices = choices$cue,
        server = TRUE
      )
      rv$meta <- meta
    })
    
    observe({
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con))
      dbExecute(con, "PRAGMA foreign_keys = ON;")
      if (!is.na(isolate(rv$lock_hash))) {
        isolate(dbUnlockResponse(con, rv$lock_hash))
        cat(file = stderr(), "UNLOCK\n")
      }
      str(as.numeric(input$researcher_id))
      if (!is.na(as.numeric(input$researcher_id))) {
        str(rv$resp$cue_response_id)
        rv$lock_hash <- dbLockResponse(con, rv$resp$cue_response_id, input$researcher_id)
        cat(file = stderr(), "LOCK\n")
      }
    })
    
    output$search_results <- DT::renderDataTable({
      DT::datatable(rv$meta, options = list(dom = "tp"))
    })
    
    output$current_response <- shiny::renderTable({
      rv$resp
    })
    observe({
      output$stem_suggestions <- renderText(unlist(hunspell_stem(rv$resp$response)))
      output$spelling_suggestions <- renderText(unlist(hunspell_suggest(rv$resp$response)))
    })
    
    observeEvent(input$commit, {
      if (is.na(as.numeric(input$researcher_id))) {
        showNotification("Commit failed. You must identify yourself before committing.")
      } else {
        con <- dbConnect(RSQLite::SQLite(), db_path)
        on.exit(dbDisconnect(con))
        dbInsertMappings(con,
                         rv$resp$cue_response_id,
                         kuperman_id = as.numeric(input$kuperman_map),
                         subtlex_id = as.numeric(input$subtlex_map),
                         cue_id = as.numeric(input$cue_map),
                         revision = str_to_lower(str_trim(input$revision)),
                         researcher_id = input$researcher_id
        )
        rv$resp <- dbSelectOneUnmappedCueResponse(con)
      }
    })
    
    observeEvent(input$reset, {
      cat(file = stderr(), "RESET\n")
      updateSelectizeInput(
        session, "kuperman_map",
        selected = ""
      )
      updateSelectizeInput(
        session, "subtlex_map",
        selected = ""
      )
      updateSelectizeInput(
        session, "cue_map",
        selected = ""
      )
      updateTextInput(session, "revision", value = rv$resp$response)
    })
    
    observeEvent(input$quit, {
      cat(file = stderr(), "QUIT\n")
      stopApp()
    })
    
    onStop(function() {
      if (!is.na(isolate(rv$lock_hash))) {
        con <- dbConnect(RSQLite::SQLite(), db_path)
        on.exit(dbDisconnect(con))
        isolate(dbUnlockResponse(con, rv$lock_hash))
        cat(file = stderr(), "UNLOCK\n")
      }
    })
  }
)
