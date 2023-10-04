library(shiny)
library(shinydashboard)
library(knitr)
library(kableExtra)
library(formattable)
library(fontawesome)
library(shinyWidgets)
library(shinyvalidate)
library(shinyalert)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "IBW/AdjBW Calculator"),
  dashboardSidebar(collapsed = T, 
    sidebarMenu(
      menuItem("Calculator", tabName = "Calculator", icon = icon("calculator")),
      
      menuItem("Info", tabName = "info_page", icon = icon("book")),
      
      p("Copyright (c) Andy Clark & Lawrence Li")
    )
  ),
  dashboardBody(
    
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"), 
    
    tabItems(
      tabItem(
        tabName = "Calculator",
        fluidPage(
          
          titlePanel("Ideal and Adjusted Body Weight for Anaesthesia"),
          
          sidebarLayout(
            
            sidebarPanel(
              width = 3, 
              
              numericInput(
                inputId = "age",
                label = "Age (years)",
                min = 2,
                max = 110,
                value = 10, 
                step= 1
              ),
              radioGroupButtons(
                inputId = "gender",
                label = "Sex",
                choices = c("Male", "Female"),
                selected = "Male", 
                justified = T
              ),
              numericInput(
                inputId = "height",
                label = "Height (cm)",
                value = 120,
                min = 50,
                max = 200,
                step = 1
              ),
              numericInput(
                inputId = "weight",
                label = "Measured Weight (kg)",
                value = 30,
                min = 10,
                max = 100,
                step = 1
              ),
              
              br(),
              
              fluidRow(
                p("All calculations are derived from the Guidelines produced by the Society for Obesity and Bariatric Anaesthesia"),
                p("Suitable for use in patients over two years of age"),
                
                HTML(paste(("Please refer to the"), 
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
                  width = 12,
                  htmlOutput(outputId = "tbl_wt")
                )
              ),
                
                column(
                  width = 12, 
                  htmlOutput(outputId = "tbl_ga")
                ),
              column(
                width = 12, 
                htmlOutput(outputId = "tbl_la")
              )
              
              ),
                
            
              
              
              
            )
          )
        )
      )
              
            ),
  tabItem(tabName = "info_page",
          fluidPage(
            titlePanel("Ideal and Adjusted Body Weight for Anaesthesia"),
            mainPanel("boo")),
    )
  )


# Server
server <- function(input, output, session) {
  
  shinyalert(
    title = "IBW/Adj35W Calculator for Anaesthesia",
    text = "The use of this application is for reference only and whilst every effort is made to make it accurate,  
    the accuracy cannot be guaranteed.
    Please use your own clinical judgement and follow your own hospital's guidelines.
    The authors do not take responsibility for any clinical decisions made using this calculator.
    Copyright (c) Dr Andy Clark & Dr Lawrence Li 2023",
size = "s", 
closeOnEsc = TRUE,
closeOnClickOutside = FALSE,
html = FALSE,
type = "warning",
showConfirmButton = TRUE,
showCancelButton = FALSE,
confirmButtonText = "OK",
confirmButtonCol = "#AEDEF4",
timer = 0,
imageUrl = "",
animation = TRUE
  )
  
  # Colour Coding for Drugs -------------------------------------------------
  
  col_opioid <- "#5db1e4"
  
  col_relaxant <- "#f02e17"
  
  col_GA <- "#ffff00"
  
  col_uppers <- "#e5c1e5"
  
  col_antichol <- "#51d851"
  
  col_benzo <- "#FFA500"
  
  col_mod <- "#c0c0c0"
  
  col_ponv <- "#FFCDB0"
  
  generate_background_stripes <- function(color1, color2, angle = 45) {
    css <- sprintf("background: repeating-linear-gradient(%sdeg, %s, %s 10px);", angle, color1, color2)
    return(css)
  }
  
  col_reversal <- generate_background_stripes(col_relaxant, "white")
  
  calc <- reactive({
    
    BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
    BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
    
    ideal_body_weight_child <- ifelse(input$gender == "Male",
                                BMI_50_boys * (input$height/100)^2,
                                BMI_50_girls * (input$height/100)^2)
  
    
    ideal_body_weight_adult <- ifelse(input$gender == "Male", 
                                      50 + (0.91 * (input$height - 152.4)), 
                                      45.5 + (0.91 * (input$height - 152.4)))
    
    ideal_body_weight <- ifelse(input$age > 18, ideal_body_weight_adult, ideal_body_weight_child)
    
    
    lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
    
    
    
    adjustedbw <- ideal_body_weight + 0.35 * (input$weight - ideal_body_weight)
    
    table <- data.frame(title = c("Measured (Total) Body Weight (TBW)", "Ideal Body Weight (IBW)", "Lean Body Mass (LBM)", "Adjusted Body Weight (Adj35BW)"), 
                        value = c(paste(round(input$weight, 1), "kg"), 
                                  paste(round(ideal_body_weight, 1), "kg"), 
                                  paste(round(lean_body_mass, 1), "kg"),
                                  paste(round(adjustedbw, 1), "kg")
                        )
    )
    
    return(table)
    
  })
  
  output$tbl_wt <- renderText({
    
    kbl(calc(), "html", escape = F, col.names = c("", "")) %>%
      kable_styling("bordered", full_width = T) %>% 
      column_spec(1:2, bold = T, background = "white")
    
  })
  
 
  
        output$tbl_ga <- reactive({
          
          BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
          BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
          
          ideal_body_weight <- ifelse(input$gender == "Male",
                                      BMI_50_boys * (input$height/100)^2,
                                      BMI_50_girls * (input$height/100)^2)
          
          lean_body_mass <- ideal_body_weight + 0.29 * (input$weight - ideal_body_weight)
          
          adjustedbw <- ideal_body_weight + 0.35 * (input$weight - ideal_body_weight)

        
        ga_tbl <- data.frame(
          colour = c(rep("", 22)),
          Drug = c(
            # GA DRUGS #
            "Propofol bolus (2 - 5 mg/kg)",
            "Propofol TCI",
            "Ketamine (1 - 2 mg/kg)", 
            # OPIATES #
            "Morphine (0.1mg/kg)", 
            "Alfentanil (10mcg/kg)", 
            "Fentanyl (1mcg/kg)", 
            "Remifentanil (mcg/kg/min & Minto Model)",
            # RELAXANTS #
            "Rocuronium (0.5 - 1mg/kg)",
            # REVERSAL #
            "Neostigmine (50 mcg/kg)",
            "Sugammadex (2mg/kg)",
            # ANTI_CHOL
            "Atropine (20mcg/kg)",
            "Glycopyrronmium (4-8 mcg/kg)",
            # ANALG
            "Paracetamol (15mg/kg)",
            "Ibuprofen (10mg/kg)",
            # ANTIEMETICS
            "Ondansetron (0.15mg/kg)",
            "Dexamethasome (0.15mg/kg)",
            # ANTIBIOTICS
            "Cephalosporins",
            "Penicillins",
            # PREMEDS
            "Midazolam - oral (0.5mg/kg)",
            "Clonidine - oral (4mcg/kg)",
            "Ketamine - oral (5-10mg/kg)",
            "Ketamine - IM (5mg/kg)"
          ),
          Dose = c(paste(round(2*ideal_body_weight, 0), "to", round(5*ideal_body_weight, 0), "mg"), # PROP BOLUS
                   paste("Use", round(adjustedbw, 0), "kg"),                                      # POP INFUSION
                   paste(round(1*ideal_body_weight, 0), "to", round(2*ideal_body_weight, 0), "mg"), # KETAMINE
                   paste(round(0.1*ideal_body_weight, 1), "mg"), # MORPHINE
                   paste(round(10*adjustedbw, 0.1), "mcg"), # ALF
                   paste(round(1*adjustedbw, 0), "mcg"), # FENT
                   paste(paste("Use", round(adjustedbw, 0), "kg")), # REMI
                   paste(round(0.5*ideal_body_weight, 0), "to", round(1*ideal_body_weight, 0), "mg"), # ROC
                   paste(ifelse(round(0.05*input$weight,1) >= 2.5, 2.5, round(0.05*input$weight,1)), "mg"),# NEOSTIGMINE
                   paste(2*input$weight, "mg"), # SUGAMMADEX
                   paste(ifelse(0.02*input$weight >= 0.6, 600, 20*input$weight), "mcg"), # ATROPINE
                   paste(ifelse(0.008*input$weight >= 0.2, 200, paste(4*input$weight, "to", 8*input$weight)), "mcg"), # GLYC
                   paste(ifelse(15 * adjustedbw >= 1000, 1000, round(15 * adjustedbw, 0)), "mg"), # PARA
                   paste(ifelse(10 * adjustedbw >= 400, 400, round(10 * adjustedbw, 0)), "mg"), # IBUP
                   paste(ifelse(0.15 * input$weight >= 8, 8, round(0.15 * input$weight, 1)), "mg"), # ONDANSETRON
                   paste(ifelse(0.15 * input$weight >= 8, 8, round(0.15 * input$weight, 1)), "mg"), # DEX
                   paste("Use", input$weight, "kg"), # CEPH
                   paste("Use", input$weight, "kg"), # PENICILLIN
                   paste(ifelse(0.5 * input$weight >= 20, 20, round(0.5 * input$weight, 0)), "mg"), # MIDAZ- ORAL
                   paste(round(4*adjustedbw, 0), "mcg"), # CLONIDINE - ORAL
                   paste(round(5*ideal_body_weight, 0), "to", round(10*ideal_body_weight, 0), "mg"), # KETAMINE - ORAL
                   paste(round(5*ideal_body_weight, 0), "mg") # KETAMINE - IM
          ),
          Parameter = c("IBW",
                        "Adj35BW", 
                        "IBW", 
                        "IBW", 
                        "Adj35BW", 
                        "Adj35BW", 
                        "Adj35BW", 
                        "IBW", 
                        "TBW", 
                        "TBW", 
                        "TBW",
                        "TBW",
                        "Adj35BW",
                        "Adj35BW",
                        "TBW",
                        "TBW",
                        "TBW",
                        "TBW",
                        "TBW",
                        "Adj35BW",
                        "IBW", 
                        "IBW")
          
        )
        
    kable(ga_tbl, "html", escape = F, col.names = c("", "Drug", "Dose", "Parameter")) %>% 
      kable_styling("hover", full_width = T, font_size = 12) %>%
      column_spec(1, background = c(col_GA, col_GA, col_GA, 
                                    col_opioid, col_opioid, col_opioid, col_opioid,
                                    col_relaxant,
                                    extra_css = generate_background_stripes(col_relaxant, "white"), extra_css = generate_background_stripes(col_relaxant, "white"),
                                    col_antichol, col_antichol,
                                    "white", "white",
                                    col_ponv, col_ponv,
                                    "white", "white",
                                    "orange", "white", col_GA, col_GA),
                  width = "2em") %>%
      column_spec(2, bold = T) %>% 
      pack_rows("Anaesthesia", 1, 2, label_row_css = "background-color: #ffff00; color: #000000;") %>% 
      pack_rows("Opiods", 4,5, label_row_css = "background-color: #5db1e4; color: #fff;") %>%
      pack_rows("Relaxants", 8,8, label_row_css = "background-color: #f02e17; color: #fff;") %>%
      pack_rows("Reversal", 9,10, label_row_css = generate_background_stripes(col_relaxant, "white"), color = "black") %>%
      pack_rows("Anticholinergics", 11, 12, label_row_css = "background-color: #51d851; color: #fff;") %>%
      pack_rows("Analgesia", 13,14, label_row_css = "background-color: #FFFFFF; color: #000000;") %>%
      pack_rows("Antiemetics", 15,16, label_row_css = "background-color: #FFCDB0; color: #000000;") %>%
      pack_rows("Antibiotics", 17,18, label_row_css = "background-color: #FFFFFF; color: #000000;") %>%
      pack_rows("Pre-medication", 19,22, label_row_css = "background-color: #FFFFFF; color: #000000;") %>% 
      column_spec(4, color = "grey") %>% 
      column_spec(2:4, background = "white")

    
  })

        output$tbl_la <- reactive({
          
          BMI_50_boys <- 24.27 - (8.91/(1+(as.numeric(input$age)/15.78)^4.4))
          BMI_50_girls <- 22.82 - (7.51/(1+(as.numeric(input$age)/13.46)^4.44))
          
          
          ideal_body_weight_child <- ifelse(input$gender == "Male",
                                            BMI_50_boys * (input$height/100)^2,
                                            BMI_50_girls * (input$height/100)^2)
          
          
          ideal_body_weight_adult <- ifelse(input$gender == "Male", 
                                            50 + (0.91 * (input$height - 152.4)), 
                                            45.5 + (0.91 * (input$height - 152.4)))
          
          ideal_body_weight <- ifelse(input$age > 18, ideal_body_weight_adult, ideal_body_weight_child)
          
  
  BW_for_LA <- ifelse(ideal_body_weight > input$weight, input$weight, ideal_body_weight)
  
  BW_for_LA <- ifelse(BW_for_LA > 75, 75, BW_for_LA)
  
  
  LA_tbl <- data.frame(
    Agent = c("Dose", "1%", "2%", "2% Dental Cartidge (2.2ml)", "Dose", "0.5%", "0.25%", "0.1%"), 
    Plain = c(paste(floor(3*BW_for_LA), "mg"), # LIDO
              paste(floor(3*BW_for_LA / 10), "ml"), # 1%
              paste(floor(3*BW_for_LA / 20), "ml"), # 2%
              paste(""),
              paste(floor(2*BW_for_LA), "mg"), # BUP
              paste(floor(2*BW_for_LA / 5), "ml"), # 0.5%
              paste(floor(2*BW_for_LA / 2.5), "ml"), # 0.25%
              paste(floor(2*BW_for_LA / 1), "ml")  # 0.01%
    ), 
    With_Adrenaline = c(paste(floor(7*BW_for_LA), "mg"), # LIDO
                        paste(floor(7*BW_for_LA / 10), "ml"), # 1%
                        paste(floor(7*BW_for_LA / 20), "ml"), # 2%
                        paste(floor((7*BW_for_LA / 20)/2.2), "cartridges"), #DENTAL
                        paste(floor(2.5*BW_for_LA), "mg"), # BUP
                        paste(floor(2.5*BW_for_LA / 5), "ml"), # 0.5%
                        paste(floor(2.5*BW_for_LA / 2.5), "ml"), # 0.25%
                        paste(floor(2.5*BW_for_LA / 1), "ml")  # 0.01%
    ))
  
  
  LA_tbl %>%
    kbl("html", escape = T, col.names = c("", "Plain", "With adrenaline"), align = "c") %>%
    pack_rows("LIDocaine", 1,1, label_row_css = "background-color: #D3D3D3; color: #000000;") %>%
    pack_rows("- Volume -", 2,3, label_row_css = "background-color: #FFFFFF; color: #000000;", bold = F) %>%
    pack_rows("BUPivicaine", 5,8, label_row_css = "background-color: #D3D3D3; color: #000000;") %>%
    pack_rows("- Volume -", 6,8, label_row_css = "background-color: #FFFFFF; color: #000000;", bold = F) %>%
    column_spec(3, background = "#e5c1e5", width = "4cm") %>%
    column_spec(2, width = "4cm") %>%
    column_spec(1) %>%
    footnote(general = "These are based on Ideal Body Weight (IBW). If measured body weight is less than IBW, this is used instead. Calculations max-out at 75kg.")
  
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
