library(shiny)
library(shinydashboard)
library(kableExtra)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Child's Ideal Body Weight and Lean Body Mass Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "calculator",
        fluidPage(
          titlePanel("Calculator"),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "age",
                label = "Age (years)",
                choices = c(5:18),
                selected = 10
              ),
              checkboxGroupInput(
                inputId = "gender",
                label = "Gender",
                choices = c("Male", "Female"),
              ),
              numericInput(
                inputId = "height",
                label = "Height (cm)",
                value = 120,
                min = 50,
                max = 200
              ),
              numericInput(
                inputId = "weight",
                label = "Weight (kg)",
                value = 0,
                min = 10,
                max = 100
              ),
              actionButton(
                inputId = "calculate",
                label = "Calculate",
                icon = icon("play")
              )
            ),
            mainPanel(
              verbatimTextOutput(outputId = "result")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$calculate, {
    # Constants for calculating ideal body weight
    BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
    BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
    
    # Calculate ideal body weight
    ideal_body_weight <- ifelse(input$gender == "Male",
                                BMI_50_boys * input$height^2,
                                BMI_50_girls * input$height^2)
    
    # Calculate lean body mass
    lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
    
    # create table for output
    
    table <- data.frame(title = c("measured weight (kg)", "Ideal Body Weight (kg)", "Lean Body Mass (kg)"), 
                       value = c(input$weight, ideal_body_weight, lean_body_mass))
    
    # Output the results
    output$result <- kbl(table, "html", escape = F) %>%
      kable_styling("bordered")
      
      
      
      # kbl(table %>% select(ebl, pred_hb, range_PI), "html", escape = F, 
      #     col.names = c("Estimated Blood Loss (mls)", "Predicted Haemoglobin (g/L)", "90% Prediction Interval (g/L)"),
      #     align = c("crr")
      # ) %>%
      #   kable_styling("bordered", full_width = F, position = "left") %>% 
      #   row_spec(0, bold = T, background = "#f0f0f0") %>% 
      #   row_spec(1:nrow(table), color = "white", background = bg) #%>% 
      # #add_header_above(c("","Haemoglobin (g/L)" = 3), bold = T, background = "#f2d8d8")
      
      
    })
  }

# Run the app
shinyApp(ui, server)
