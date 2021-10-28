library(shiny)
library(dplyr)
library(readxl)
source('Functions/select_to_print.R')
source('Functions/check_for_duplicates.R')
# Define UI ----
ui <- shinyUI(
  navbarPage("Select Tool: ", 
             
             tabPanel("Step 1: Print Barcodes",
                      titlePanel( div(img(src = "hop_logo.png", height = 100, 
                                          width = 100), 
                                      "Barcodes for HOP Kits")
                      ),
                      sidebarLayout(
                        sidebarPanel(position = "left",
                                     h4("Choose the number of barcodes to print:"), 
                                     numericInput("numBarcodes", label = "", value = 0)
                                     #h4("Select the Master file (Excel format) containing list of barcodes and used status."),
                                     #fileInput("masterFile", label = "")
                                     #submitButton("Submit")
                        ),
                        mainPanel(postion = "right",
                                  h3("Step 1: Use this tool to select unique and unused barcodes for 
                                     printing barcode labels for new HOP saliva sample collection kits."),
                                  textOutput("selectedNum"),
                                  #textOutput("selectedMaster"),
                                  #              dataTableOutput("duplicates")
                                  downloadButton('downloadData', 'Download Barcodes to Print'),
                                  tableOutput('print_table')
                                  )
                      )
             ),
             tabPanel("Step 2: Confirm Unique Barcodes",
                      titlePanel( div(img(src = "hop_logo.png", height = 100, 
                                          width = 100), 
                                      "Barcodes for HOP Kits")
                      ),
                      sidebarLayout(
                        sidebarPanel(position = "left",
                                     h4("Select the file (Excel format) containing scanned primary barcodes to check for duplicates."),
                                     fileInput("scannedFile", label = "", accept = c(".xlsx", '.xls')),
                                     br()
                                     #h4("Select the Master file (Excel format) containing list of barcodes and used status."),
                                     #fileInput("masterFile2", label = ""),
 #                                    submitButton("Submit")
                        ),
                        mainPanel(postion = "right",
                                  h3("Step 2: Use this tool to confirm that selected and printed barcodes are unique and 
                                     have not been previously used."),
                                  textOutput("selectedScan"),
                                  #textOutput("selectedMaster2"),
                                  tableOutput("check_table")
                                  )
                      )
             )
  )
             )

# Define server logic ----
server <- function(input, output) {
  #### STEP 1 #####
  selected_barcodes <- reactive({
    validate(need(input$numBarcodes > 0, "Please select a number"))
    select_to_print(input$numBarcodes)})
  output$selectedNum <- renderText({paste("Selecting ", input$numBarcodes, " barcodes to print.")})
  output$print_table <- renderTable({selected_barcodes()})

 ##  Select barcodes to print & provide file for downloading
  output$downloadData <- downloadHandler(filename = function() {
                              paste("print_barcodes_", Sys.Date(), ".csv", sep="")
                              },
                              content = function(file) {
                                write.csv(selected_barcodes(), file, row.names = FALSE)
                              } )
  #### STEP 2 #####
  #check_results <- reactive({check_for_duplicates(read_excel(input$scannedFile))})
  output$selectedScan <- renderText({
    validate(need(!is.null(input$scannedFile), "Please select an Excel file as input"))
    paste("Uploading and opening file: ", input$scannedFile)})
  output$check_table <- renderTable({
    validate(need(!is.null(input$scannedFile), "Please select an Excel file as input"))
    req(input$scannedFile)
    inFile <- input$scannedFile
    scanned <- read_excel(inFile, 1)
    check_for_duplicates(scanned)
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)