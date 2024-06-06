#############################################
#                                           #
#             Back-end of BPapp             #
#                                           #
#############################################

# Developed by: Marie-Christine Rufener
# Last update: 5 June 2024
# For queries, feedback, and bugs, contact: < macrufener@gmail.com >


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>><><>

# Data




server <- function(input, output) {
  
  
  ############
  ## Inputs ##
  ############
  
  
  #~~~~~~~~~~~~~~~~~~
  # 1) Upload  data
  #~~~~~~~~~~~~~~~~~~
  
  
  data <- reactive({
    
    req(input$filedata) #Id in ui.R
    read.csv(input$filedata$datapath)
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~
  # 2) Reshape data
  #~~~~~~~~~~~~~~~~~~
  
  df <- reactive({
    
    df <- data()
    
    
    # 2.1) Match colnames
    #~~~~~~~~~~~~~~~~~~~~~
    
    # Default colnames
    COLNAMES <- c("Valor_BRL",
                  "Categoria",
                  "Data",
                  "Ano",
                  "Mes",
                  "Forma_de_pagamento")
    
    
    # Match colnames
    if(length(intersect(colnames(df), COLNAMES)) != length(COLNAMES))
      stop("O nome de uma ou mais colunas não coincide com as colunas padrão. Certifique-se de que as colunas da sua tabela sigam o modelo padrão do aplicativo")
    
    
    # 2.2) Set appropriate data type to columns
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Factor
    df[, c("Categoria",
           "Ano", 
           "Mes", 
           "Forma_de_pagamento")] <- lapply(df[, c("Categoria",
                                                   "Ano", 
                                                   "Mes", 
                                                   "Forma_de_pagamento")], factor)
    
    
    # Date
    df$Data <- as.Date(df$Data, "%d.%m.%Y")
    
    
    # 2.3) Reorder data by Date & refactor month levels
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Reorder by date
    df <- df[order(df$Data),]
    
    
    # Rename factor levels of Month
    df$Mes <- dplyr::recode_factor(df$Mes,
                                   Janeiro = "Jan",
                                   Fevereiro = "Feb",
                                   Marco = "Mar",
                                   Abril = "Apr",
                                   Maio = "May",
                                   Junho = "Jun",
                                   Julho = "Jul",
                                   Agosto = "Aug",
                                   Setembro = "Sep",
                                   Outubro  = "Oct",
                                   Novembro = "Nov",
                                   Dezembro = "Dec")
    
    return(df)
    
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~
  # 3) Summarize data
  #~~~~~~~~~~~~~~~~~~~
  
  annual_summary <- reactive({
    
    THIS_YEAR <- year(Sys.Date())
    THIS_MONTH <- month(Sys.Date(), label = T)
    
    
    df() %>%
      group_by(Ano, Mes) %>%
      #filter(Mes %in% MONTH) %>%
      summarise(TAI = unique(Renda),
                TAE = sum(Valor_BRL),
                TAS = TAI - TAE) %>%
      summarise(TAI = round(sum(TAI),2),
                TAE = round(sum(TAE),2),
                TAS = round(sum(TAS)),2) %>%
      filter(Ano %in% THIS_YEAR) 
    
  })
  
  
  #############
  ## Outputs ##
  #############
  
  
  
  # Dashboard Tab
  #~~~~~~~~~~~~~~~
  output$TAI <- renderValueBox({
    
    valueBox(
      value = paste("R$", format(annual_summary()$TAI, big.mark=","), sep= " "), 
      subtitle = "Total annual income", 
      icon = icon("list"),
      color = "navy"
    )
  })
  
  
  output$TAE <- renderValueBox({
    valueBox(
      value = paste("R$", format(annual_summary()$TAE, big.mark=","), sep= " "), 
      subtitle = "Total annual expsenses", 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
    
  })
  
  output$TAS <- renderValueBox({
    valueBox(
      value = paste("R$", format(annual_summary()$TAS, big.mark=","), sep= " "), 
      subtitle = "Total annual savings", 
      icon = icon("list"),
      color = "blue"
    )
  })
  
  
  
  # Table Tab
  #~~~~~~~~~~~~
  #output$mydata <- renderDataTable(data())
  output$mydata <- renderDataTable(datatable(df(),
                                             filter = 'top',
                                             options = list(
                                               columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                               searching = TRUE)))
  
  
  
}

# # Define server logic required to draw a histogram
# function(input, output, session) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
# 
#     })
# 
# }
