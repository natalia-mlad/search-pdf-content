# debug <- TRUE

# Load packages ----
library(tidyverse) #core
library(glue) #core
library(fs) #paths
library(rstudioapi) #paths
library(DT) #shiny
library(shiny) #shiny
library(shinyWidgets) #shiny
library(shinycssloaders) #shiny
library(stringdist) #additional
library(targets) #data

# Helper functions -----
source("R/app-helpers.R")

# LoadData: ----
# I made the path not .Rproj dependant so I can click the shortcut on my desktop to run it:
rootdir <- path_dir(getSourceEditorContext()$path)
my_targets_store <- paste0(rootdir, "/_targets")
usethis::ui_todo("Loading the Data...")
tar_load(all_pdf_contents2, store = my_targets_store)
# if (isTRUE(debug)) all_pdf_contents2 <- all_pdf_contents2[1:50, ]
# if (isTRUE(debug)) print(head(all_pdf_contents2))
df <- all_pdf_contents2 #backup
usethis::ui_done("Data Loaded")

search_history <- c()

# TODO: add a toggle for bigger than 5Mb files
# print(Sys.time()) #7min to search through 20.5k+ pdfs/16.3Gb?

# User interface ----
ui <- fluidPage(
  titlePanel("Search PDF Contents"),
  h5("Info: Uses the str_detect() function to search, so any regex will work (e.g., |, *, [.], ., ^, $)"),
  # h5("Duplicate files (based on file size in bytes) get removed from the results."),
  h5("If you search several times, it narrows the search within the existing results."),
  h5("To start a new search, reload the app."),
  h5("To open a PDF that was found, select the row in the results table and press 'Open File' above the table."),
  sidebarLayout(
    sidebarPanel(
      # h5(textOutput("score")),
      # actionButton("load_data", "Load the Data"),
      # textInput("path", label = "[TODO] Path of the PDF(s) (e.g.,'C:/Users/Natalia/OneDrive/'):"),
      # textInput("filename", label = "[TODO] Filename:"),
      textInput("var", label = "Text to search:"),
      actionBttn("go", "Go", style = "jelly"),
      br(),
      p(textOutput("status1"), style = "font-weight=500; color: #000000;"),
      br(),
      p("Search history:", style = "font-weight: bold;"),
      textOutput("status2"),
      # h5(textOutput("status3"), style = "font-weight=500; color: #FF0000;"),
      br(),
      br(),
      actionBttn("refresh", "New Search", size = "xs", style = "jelly")
    ),
    mainPanel(
      actionBttn("open", "Open File", size = "xs", style = "jelly"),
      # verbatimTextOutput('test123'),
      DTOutput("search_results") %>%
        withSpinner(type = 7, color = "#0dc5c1")
    )
  ))


# Server logic ----
server <- function(input, output) {
  ## Notes: ####
  # shinyspring_bak::load_tar_as_tibble(all_pdf_contents2)
  #
  # observeEvent(input$load_data, {
  #   usethis::ui_todo("Loading the Data...")
  #   rootdir <- path_home("OneDrive/pkgs/relevant.references")
  #   my_targets_store <- paste0(rootdir, "/_targets")
  #   all_pdf_contents2 <- tar_load(all_pdf_contents2, store = my_targets_store)
  #   usethis::ui_done("Data Loaded")
  # })
  #
  # withProgress(message = 'Making plot', value = 0, {})

  ## Search History: ####
  searchHistory <- eventReactive(input$go, {
    search_history <<- c(search_history, input$var)
    return(search_history)
  })
  output$status2 <- renderText({searchHistory()}, sep = "\n") # TODO: keeps on NOT putting them on new line...

  ## Search Text: ####
  # When press 'Go', search the text
  searchVar <- eventReactive(input$go, {
    all_pdf_contents2 <<- suppressWarnings(searchPDFcontent(input$var, all_pdf_contents2))
    results <<- all_pdf_contents2 %>% select(-txt) %>% .[!duplicated(.$size), ]
    # return(all_pdf_contents2)
    # print(Sys.time())
    return(results)
  })

  ## DataTable: ####
  # Present the concise DataTable
  output$search_results <- renderDT({
    searchVar() %>%
      # select(-txt) %>%
      # select(-c(txt, inode, blocks, access_time, change_time)) %>%
      # .[!duplicated(.$size), ] %>%
      # doesn't work #mutate(size = fs_bytes(size)) #to make it easier to read
      select(filename, size, modification_time, path)
  },
  options = list(lengthChange = FALSE, pageLength = 10),
  selection = 'single')

  output$status1 <- renderPrint({
    searchVar() %>%
      nrow() %>%
      glue(" results found")
    # usethis::ui_bullet(x, crayon::yellow(cli::symbol$info))
  })

  ## Open File: ####
  # output$test123 = renderPrint(input$search_results_rows_selected)
  observeEvent(input$open, {
    s = input$search_results_rows_selected
    if (length(s)) {
      my_path <- results[s, ] %>% pull(path)
      if (file_exists(my_path)) file_show(my_path)
    }
  })

  ## TODO: New Search: ####
  observeEvent(input$refresh, {
    all_pdf_contents2 <<- df
  })

}

# Run app ----
shinyApp(ui, server)
