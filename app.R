library(shiny)
library(googlesheets)
library(ggplot2)
library(gridExtra)

# save and load data in Google sheets
table <- "responsesBG"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}
#------

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
#------

# CSS for mandatory fields
appCSS <-
  ".mandatory_star { color: red; }
#error { color: red; }"
#------

# Define the fields we want to save from the form
# fields <- c("name", "used_shiny", "r_num_years")
fields <- c("name", "email", "town", "used_shiny", "r_num_years", "os_type", "experiance", "expectation")
fieldsMandatory <- c("name", "email")
# Shiny app with 2 mandatory fields that the user can submit data for

ui = fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel(h2("R-Ladies BG Form")),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  
                  div(
                    id = "form",
                    
                    textInput("name", labelMandatory("Name"), ""),
                    textInput("email", labelMandatory("Your email address")),
                    textInput("town", "Town"),
                    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
                    sliderInput("r_num_years", "Number of years using R", 0, 25, 0, ticks = FALSE),
                    selectInput("os_type", "Operating system used most frequently",
                                c("",  "Windows", "Mac", "Linux")),
                    selectInput("experiance", "Level of experiance with R",
                                c("",  "no", "beginner", "intermediate", "advanced")),
                    selectInput("expectation", "What do you expect from R-Ladies",
                                c("",  "mentoring", "tutorials", "coding club", "networking")),
                    actionButton("submit", "Submit", class = "btn-primary"),
                    
                    shinyjs::hidden(
                      span(id = "submit_msg", "Submitting..."),
                      div(id = "error",
                          div(br(), tags$b("Error: "), span(id = "error_msg"))
                      )
                    )
                  ),
                  
                  shinyjs::hidden(
                    div(
                      id = "thankyou_msg",
                      h4("Thanks, your response was submitted successfully!"),
                      actionLink("close", "Close the app")
                    )
                  )), 
                
                mainPanel(column(12,
                                 #DT::dataTableOutput("responses", width = 300), tags$hr(),
                                
                                 plotOutput("no_years"),
                                 
                                 plotOutput("pl_experiance"),
                                 
                                 plotOutput("pl_expectation")
                                 
                ))
  ))


server = function(input, output, session) {
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })  
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # action to take when submit button is pressed
  observeEvent(input$submit, 
  {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # action to take when clicking on the “Close the App” 
    # in the message window  
    observeEvent(input$close, {
      stopApp()  
      #shinyjs::show("form")
      #shinyjs::hide("thankyou_msg")
    }) 
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
    my.dts <- loadData()
    output$output_txt1 <- renderPrint(my.dts)

    output$no_years <- renderPlot({
      ggplot(my.dts, aes(x = r_num_years, width = 0.5)) +
        geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
        theme_bw() +
        scale_x_continuous("no of years", breaks = seq(0,25,1)) +
        scale_y_continuous("count") +
        geom_text(stat = "count", aes(label = ..count.., y = ..count..), size = 8) +
        ggtitle("R No of Years") +
        theme(plot.title = element_text(hjust = 0.5))    

    })
    
    output$pl_experiance <- renderPlot({
      ggplot(my.dts, aes(x = factor(experiance), width = 0.5)) +
        geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
        ggtitle("Experiance") + xlab("") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5), panel.grid.major.x = element_blank())
    })
    
    output$pl_expectation <- renderPlot({
      ggplot(my.dts, aes(x = factor(expectation), width = 0.5)) +
        geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
        ggtitle("Expectation") + xlab("") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5), panel.grid.major.x = element_blank())
    })
    
  })
  
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })     
  
  my.dt <- loadData()
  output$output_txt1 <- renderPrint(my.dt)

  output$no_years <- renderPlot({
    ggplot(my.dt, aes(x = r_num_years, width = 0.5)) +
    geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
      theme_bw() +
      scale_x_continuous("no of years", breaks = seq(0,25,1)) +
      scale_y_continuous("count") +
      geom_text(stat = "count", aes(label = ..count.., y = ..count..), size = 8) +
      ggtitle("R No of Years") +
      theme(plot.title = element_text(hjust = 0.5))
      #+coord_flip() 
      # + theme(legend.position = "top")
    })
  
  output$pl_experiance <- renderPlot({
    ggplot(my.dt, aes(x = factor(experiance), width = 0.5)) +
      geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
      ggtitle("Experiance") + xlab("") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major.x = element_blank())
    })
  
  output$pl_expectation <- renderPlot({
    ggplot(my.dt, aes(x = factor(expectation), width = 0.5)) +
      geom_bar(stat="count", fill = "#8B4789", colour = "gray47") +
      ggtitle("Expectation") + xlab("") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major.x = element_blank())
  })

}

# Run the application 
shinyApp(ui = ui, server = server)