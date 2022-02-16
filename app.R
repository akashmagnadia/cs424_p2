

# Create the shiny application
ui <- dashboardPage(
  dashboardHeader(title = "CTA Subway Data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About"),
                     menuItem("Visualization", tabName = "viz", selected = T)
                   ),
                   shinyjs::useShinyjs()
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "viz",
              sidebarLayout(
                position = "left",
                sidebarPanel(
                  style = "margin-top:100%; padding: 15px;",
                  div(checkboxInput("single_date_check",
                                    label = "Single Date",
                                    value = TRUE
                                    )
                      ),
                  div(dateInput("single_date_input",
                                label = "Single Date",
                                value = "2021-08-23",
                                min = "2001-01-01",
                                max = "2021-11-30",
                                format = "m / dd / yyyy"
                                )
                      ),
                  div(checkboxInput("range_date_check",
                                    label = "Range of Date(s)",
                                    value = FALSE
                                    )
                      ),
                  div(dateInput("range_start_date_input",
                                label = "Start Date",
                                value = "2020-08-23",
                                min = "2001-01-01",
                                max = "2021-11-30",
                                format = "m / dd / yyyy"
                  )
                  ),
                  div(dateInput("range_end_date_input",
                                label = "End Date",
                                value = "2021-08-23",
                                min = "2001-01-01",
                                max = "2021-11-30",
                                format = "m / dd / yyyy"
                  )
                  ),
                  width = 2
                ),
                mainPanel(
                  h2("test"),
                  width = 10
                  )
                )
              )
    )
  )
)

server <- function(input, output, session) {
  
  #################################################################
  
  # TODO: Chaange color palatte for the lines
  
  getLowGradientCol <- function(x) {
    gradientCol <- "grey"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#fbb4ae"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#b3cde3"
    } else if (x == "Rosemont") {
      gradientCol <- "#ccebc5"
    } else {
      gradientCol <- "#decbe4"
    }
  }
  
  getHighGradientCol <- function(x) {
    gradientCol <- "black"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#e41a1c"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#377eb8"
    } else if (x == "Rosemont") {
      gradientCol <- "#4daf4a"
    } else {
      gradientCol <- "#984ea3"
    }
  }  
  
  #################################################################
  
  # create a data frame to show yearly data for the station(s) selected
  
  entries_year_sum_table_2 <- reactive({
    toReturn <- sum_of_year_df_2()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_year_sum_table_3 <- reactive({
    toReturn <- sum_of_year_df_3()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
  # create a data table to show yearly data for the 
  
  output$entries_year_sum_table_2 <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_year_sum_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_year_sum_table_3 <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_year_sum_table_3(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  observeEvent(input$single_date_check, {
    if (input$single_date_check) {
      updateCheckboxInput(session, "range_date_check", value = FALSE)
      shinyjs::enable("single_date_input")
      shinyjs::disable("range_start_date_input")
      shinyjs::disable("range_end_date_input")
    }
  })
  
  observeEvent(input$range_date_check, {
    if (input$range_date_check) {
      updateCheckboxInput(session, "single_date_check", value = FALSE)
      shinyjs::disable("single_date_input")
      shinyjs::enable("range_start_date_input")
      shinyjs::enable("range_end_date_input")
    }
  })
}

shinyApp(ui = ui, server = server)