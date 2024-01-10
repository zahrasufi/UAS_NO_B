#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
# Load Packages
library(shiny)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(DT)
library(rstatix)
library(dplyr)
library(plotly)

# Sample data
data.CTR <- data.frame(
  day = 1:10,
  left = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "CTR Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input_data"),
      menuItem("Dataset", tabName = "dataset"),  # New menu item for the dataset
      menuItem("Summary and Analysis Statistics", tabName = "data_summary"),
      menuItem("Decisions and Conclusions", tabName = "decisions_conclusions"),
      menuItem("Visualization", tabName = "visualization")
    ),
    tags$style(
      HTML("
        .main-sidebar {
          background-color: #0056b3;
        }
        .main-sidebar a {
          color: white;
        }
      ")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f4f6f9;
        }
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 10px;
          border: 2px solid #0056b3;
        }
        .box .box-header {
          background-color: #0056b3;
          color: white;
          border-radius: 8px 8px 0 0;
        }
        .box .box-body {
          background-color: #e0e8f0;
          border-radius: 0 0 8px 8px;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "input_data",
        h2("Input Data"),
        fluidRow(
          column(
            width = 6,
            textInput("left_input", "Enter data for left", ""),
            textInput("center_input", "Enter data for center", ""),
            textInput("right_input", "Enter data for right", ""),
            actionButton("submit_btn", "Add Data")
          ),
        ),
        fluidRow(
          column(
            width = 12,
            plotlyOutput("bar_plot")
          )
        )
      ),
      tabItem(
        tabName = "data_summary",
        h2("Data Summary and ANOVA Test"),
        verbatimTextOutput("summary_anova_output")
      ),
      tabItem(
        tabName = "decisions_conclusions",
        h2("Decisions and Conclusions"),
        box(
          title = "LEFT",
          solidHeader = TRUE,
          status = "primary",
          textOutput("box_left")
        ),
        box(
          title = "CENTER",
          solidHeader = TRUE,
          status = "warning",
          textOutput("box_center")
        ),
        box(
          title = "RIGHT",
          solidHeader = TRUE,
          status = "success",
          textOutput("box_right")
        )
      ),
      tabItem(
        tabName = "visualization",
        h2("Visualization"),
        plotlyOutput("bar_plot_visualization")
      ),
      tabItem(
        tabName = "dataset",  # New tab for the dataset
        h2("Dataset"),
        DTOutput("dataset_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize default data
  rv <- reactiveValues(data = data.CTR, selected_rows = NULL, editing_row = NULL)
  
  # Add new data to the table
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left = as.numeric(input$left_input),
      center = as.numeric(input$center_input),
      right = as.numeric(input$right_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Set selected rows
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })
  
  # Remove selected data
  observeEvent(input$hapus_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })
  
  # Edit selected data
  observeEvent(input$edit_data_btn, {
    if (!is.null(rv$selected_rows) && length(rv$selected_rows) == 1) {
      rv$editing_row <- rv$selected_rows
      updateTextInput(session, "left_input", value = as.character(rv$data$left[rv$editing_row]))
      updateTextInput(session, "center_input", value = as.character(rv$data$center[rv$editing_row]))
      updateTextInput(session, "right_input", value = as.character(rv$data$right[rv$editing_row]))
    }
  })
  
  # Save changes after editing
  observeEvent(input$submit_btn, {
    if (!is.null(rv$editing_row)) {
      rv$data$left[rv$editing_row] <- as.numeric(input$left_input)
      rv$data$center[rv$editing_row] <- as.numeric(input$center_input)
      rv$data$right[rv$editing_row] <- as.numeric(input$right_input)
      rv$editing_row <- NULL
    }
  })
  
  # Statistical analysis
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  
  # Display ANOVA results
  output$box_left <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(left ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Because p-value = ", p_value, " > 0.05, data fails rejects H0. That means there is no significant difference for the left sidebar group compared to other groups")
    } else {
      paste("Because p-value = ", p_value, " < 0.05, data rejects H0. That means there is a significant difference for the left sidebar group compared to other groups")
    }
  })
  
  output$box_center <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(center ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Because p-value = ", p_value, " > 0.05, data fails rejects H0. That means there is no significant difference for the center page group compared to other groups")
    } else {
      paste("Because p-value = ", p_value, " < 0.05, data rejects H0. That means there is a significant difference for the center page group compared to other groups")
    }
  })
  
  output$box_right <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(right ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("Because p-value = ", p_value, " > 0.05, data fails rejects H0. That means there is no significant difference for the right sidebar group compared to other groups")
    } else {
      paste("Because p-value = ", p_value, " < 0.05, data rejects H0. That means there is a significant difference for the right sidebar group compared to other groups")
    }
  })
  
  # Bar plot visualization
  output$bar_plot_visualization <- renderPlotly({
    if (is.null(rv$data)) return(NULL)
    
    # Data for the plot
    data_plot <- rv$data %>%
      pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")
    
    # Colors for each group
    colors <- c("#418FBD", "#F39C14", "#14A85D")
    
    # Plot
    p <- plot_ly(data_plot, x = ~Group, y = ~Value, type = "bar", color = ~Group, colors = colors) %>%
      layout(title = "Sum of Click Based on Location",
             xaxis = list(title = "Location"),
             yaxis = list(title = "Sum"),
             showlegend = FALSE)
    
    return(p)
  })
  
  # Data Summary and ANOVA Test
  output$summary_anova_output <- renderPrint({
    if (is.null(rv$data)) return(NULL)  # Avoid summary if data is empty
    
    cat("Data Summary:\n")
    print(summary(rv$data))
    
    cat("\nANOVA Test Results:\n")
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  
  # Download Data
  output$download_data_btn <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  
  # Display dataset table
  output$dataset_table <- renderDT({
    datatable(rv$data)
  })
  
}

# Run the application
shinyApp(ui, server)