library(shiny)
library(shinydashboard)
library(knitr)
library(kableExtra)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "IBW/LBM Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paediatrics", tabName = "paeds_calc", icon = icon("baby")),
      
      menuItem("Adult", tabName = "adult_calc", icon = icon("person")),
      
      p("Copyright (c) Andy Clark & Lawrence Li")
    )
  ),
  dashboardBody(
    
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"), 
    
    tabItems(
      tabItem(
        tabName = "paeds_calc",
        fluidPage(
          
          titlePanel("Ideal Body Weight and Lean Body Mass for Paediatrics"),
          
          sidebarLayout(
            
            sidebarPanel(
              width = 3, 
              
              selectInput(
                inputId = "age",
                label = "Age (years)",
                choices = c(5:18),
                selected = 10
              ),
              selectInput(
                inputId = "gender",
                label = "Gender",
                choices = c("Male", "Female"),
                selected = "Male"
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
                label = "Measured Weight (kg)",
                value = 30,
                min = 10,
                max = 100
              ),
              
              br(),
              
              fluidRow(
                p("All calculations are derived from the Guidelines produced by the Society for Obesity and Bariatric Anaesthesia"),
                
                HTML(paste(("Please refere to the"), 
                           a(href = "https://www.sobauk.co.uk/guidelines-1", "full guidelines here.")))
              ),
              # actionButton(
              #   inputId = "calculate",
              #   label = "Calculate",
              #   icon = icon("play")
              # )
            ),
            mainPanel(
              
              # Display tables
              
              fluidRow(
                column(
                  width = 3,
                  htmlOutput(outputId = "tbl_wt")
                )
              ),
              
              fluidRow(
                column(
                  width = 4,
                  htmlOutput(outputId = "tbl_tbw")
                ),
                
                column(
                  width = 4, 
                  htmlOutput(outputId = "tbl_ibw")
                ),
                
                column(
                  width = 4, 
                  htmlOutput(outputId = "tbl_adbw")
                )
              ),
              
              
              
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Colour Coding for Drugs -------------------------------------------------
  
  col_opioid <- "#5db1e4"
  
  col_relaxant <- "#f02e17"
  
  col_GA <- "#ffff00"
  
  col_uppers <- "#e5c1e5"
  
  col_antichol <- "#51d851"
  
  col_benzo <- "#FFA500"
  
  col_mod <- "#c0c0c0"
  
  col_ponv <- "#FFCDB0"
  
  calc <- reactive({
    
    BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
    BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
    
    ideal_body_weight <- ifelse(input$gender == "Male",
                                BMI_50_boys * (input$height/100)^2,
                                BMI_50_girls * (input$height/100)^2)
    
    lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
    
    adjustedbw <- ideal_body_weight + 0.35 * (input$weight - ideal_body_weight)
    
    table <- data.frame(title = c("Measured Weight (kg)", "Ideal Body Weight (kg)", "Lean Body Mass (kg)", "Adjusted Body Weight (kg)"), 
                        value = c(round(input$weight, 1), 
                                  round(ideal_body_weight, 1), 
                                  round(lean_body_mass, 1),
                                  round(adjustedbw, 1)
                        )
    )
    
    return(table)
    
  })
  
  output$tbl_wt <- renderText({
    
    kbl(calc(), "html", escape = F, col.names = c("", "")) %>%
      kable_styling("bordered", full_width = F) %>% 
      column_spec(1, bold = T)
    
  })
  
  output$tbl_tbw <- renderText({
    
    tbl_tbw <- data.frame(Drug = c("Atropine (0.02 mg/kg)", 
                                   "Glycopyrrolate (0.01 mg/kg)", 
                                   "Dexamethasone (0.15 mg/kg)", 
                                   "Ondansetron (0.1 mg/kg)",
                                   "Suxamethonium (1 - 2 mg /kg)", 
                                   "Sugammadex (2 - 16 mg/kg)", 
                                   "Neostigmine (0.05 mg/kg)",
                                   "Penicillins", 
                                   "Cephalosporins", 
                                   "Enoxaparin"),
                          Dose = c(paste(ifelse(0.02*input$weight >= 0.6, 0.6, 0.02*input$weight), "mg"),
                                   paste(ifelse(0.01*input$weight >= 0.2, 0.2, 0.01*input$weight), "mg"),
                                   paste(ifelse(0.15*input$weight >= 8, 8, 0.15*input$weight), "mg"),
                                   paste(ifelse(0.1*input$weight >= 4, 4, 0.1*input$weight), "mg"),
                                   paste(1*input$weight, "to", 2*input$weight, "mg"),
                                   paste(2*input$weight, "to", 16*input$weight, "mg"),
                                   paste(ifelse(0.05*input$weight >= 2.5, 2.5, 0.05*input$weight), "mg"),
                                   "Refer to BNFC or local guideline",
                                   "Refer to BNFC or local guideline",
                                   "Refer to BNFC or local guideline"
                          )
    )
    
    tbl_tbw$Drug <- cell_spec(tbl_tbw$Drug,
                              background = ifelse(tbl_tbw$Drug %in% c("Sugammadex (2 - 16 mg/kg)", "Neostigmine (0.05 mg/kg)"), 
                                                  "white", 
                                                  scales::alpha("white", 0))
                              )
    
    generate_background_stripes <- function(color1, color2, angle = 45) {
      css <- sprintf("background: repeating-linear-gradient(%sdeg, %s, %s 10px);", angle, color1, color2)
      return(css)
    }
    
    
    kbl(tbl_tbw, "html", escape = F, col.names = c("Drug", "Dose based on Actual Weight")) %>%
      kable_styling("bordered", full_width = T) %>% 
      column_spec(1, bold = T) %>% 
      column_spec(2, background = "white") %>% 
      row_spec(1:2, background = col_antichol) %>% 
      row_spec(3:4, background = col_ponv) %>% 
      row_spec(5, background = col_relaxant) %>% 
      row_spec(6:7, extra_css = generate_background_stripes(col_relaxant, "white"), color = "black") %>% 
      row_spec(8:10, background = "white")  
    
  })
  
  output$tbl_ibw <- reactive({
    
    BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
    BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
    
    ideal_body_weight <- ifelse(input$gender == "Male",
                                BMI_50_boys * (input$height/100)^2,
                                BMI_50_girls * (input$height/100)^2)
    
    tbl_ibw <- data.frame(Drug = c("Propofol bolus (2 - 5 mg/kg)",
                                   "Ketamine (1 - 2 mg/kg",
                                   "Morphine (0.1 mg/kg)", 
                                   "Atracurium (0.5 mg/kg)",
                                   "Rocuronium (0.5 - 1 mg/kg)",
                                   "Dexamethasone (0.15 mg/kg)",
                                   "Local Anaesthetics",
                                   "Adrenaline",
                                   "Phenyephrine"
                                   ),
                          Doses = c(paste(round(2*ideal_body_weight, 0), "to", round(5*ideal_body_weight, 0), "mg"),
                                    paste(round(1*ideal_body_weight, 0), "to", round(2*ideal_body_weight, 0), "mg"),
                                    paste(round(0.1*ideal_body_weight, 0), "mg"),
                                    paste(round(0.5*ideal_body_weight, 0), "mg"),
                                    paste(round(0.5*ideal_body_weight, 0), "to", round(1*ideal_body_weight, 0), "mg"),
                                    paste(round(0.15*ideal_body_weight, 0), "mg"),
                                    paste("Dose according to preparation using IBW"),
                                    paste("Dose according to indication using IBW"),
                                    paste("Dose according to indication using IBW")
                                    )
                          )
    
    kbl(tbl_ibw, "html", escape = F, col.names = c("Drug", "Dose based on Ideal Body Weight")) %>% 
      kable_styling("bordered", full_width = T) %>%
      column_spec(1, bold = T) %>% 
      column_spec(2, background = "white") %>% 
      row_spec(1:2, background = col_GA) %>% 
      row_spec(3, background = col_opioid) %>% 
      row_spec(4:5, background = col_relaxant) %>% 
      row_spec(6, background = col_ponv) %>% 
      row_spec(7, background = col_mod) %>% 
      row_spec(8:9, background = col_uppers)
    })
  
  output$adbw <- reactive({
    
    BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
    BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
    
    ideal_body_weight <- ifelse(input$gender == "Male",
                                BMI_50_boys * (input$height/100)^2,
                                BMI_50_girls * (input$height/100)^2)
    
    lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
    
    adjustedbw <- ideal_body_weight + 0.35 * (input$weight - ideal_body_weight)
    
  })
  
  # observeEvent(input$calculate, {
  #   # Constants for calculating ideal body weight
  #   BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
  #   BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
  #   
  #   # Calculate ideal body weight
  #   ideal_body_weight <- ifelse(input$gender == "Male",
  #                               BMI_50_boys * input$height^2,
  #                               BMI_50_girls * input$height^2)
  #   
  #   # Calculate lean body mass
  #   lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
  #   
  #   # create table for output
  #   
  #   table <- data.frame(title = c("measured weight (kg)", "Ideal Body Weight (kg)", "Lean Body Mass (kg)"), 
  #                      value = c(input$weight, ideal_body_weight, lean_body_mass))
  #   
  #   # Output the results
  #   output$result <- kbl(table, "html", escape = F) %>%
  #     kable_styling("bordered")
  
  
  
  # kbl(table %>% select(ebl, pred_hb, range_PI), "html", escape = F, 
  #     col.names = c("Estimated Blood Loss (mls)", "Predicted Haemoglobin (g/L)", "90% Prediction Interval (g/L)"),
  #     align = c("crr")
  # ) %>%
  #   kable_styling("bordered", full_width = F, position = "left") %>% 
  #   row_spec(0, bold = T, background = "#f0f0f0") %>% 
  #   row_spec(1:nrow(table), color = "white", background = bg) #%>% 
  # #add_header_above(c("","Haemoglobin (g/L)" = 3), bold = T, background = "#f2d8d8")
  
  
  
}

# Run the app
shinyApp(ui, server)
