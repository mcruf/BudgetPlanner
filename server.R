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
      stop("Please verify your column names. They should match to the same column names of the dataset in the example.")
    
    
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
  
  
  
  #~~~~~~~~~~~~~~~~~~~
  # 4) Default inputs
  #~~~~~~~~~~~~~~~~~~~
  
  # 4.1) Default color palette
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  MYCOLORS <- c("#203a44",
                #"#264653",
                "#2a9d8f",
                "#8ab17d",
                "#e9c46a",
                "#EFD595",
                "#efb366",
                "#f4a261",
                "#e76f51",
                "#e97c61",
                "#bc4749")
  
  
  ## Standardize colors based on expense category
  THIS_YEAR <- year(Sys.Date())
  THIS_MONTH <- month(Sys.Date(), label = T)
  
  
  dfcolors <- df() %>% 
              filter(Mes %in% THIS_MONTH & Ano %in% THIS_YEAR) %>%
              group_by(Categoria) %>%
              summarise(Total_cost = sum(Valor_BRL)) %>%
              mutate(Total_perc = Total_cost / sum(Total_cost) * 100) %>%
              select("Categoria") 
  
  dfcolors <- data.frame(Categoria = dfcolors, 
                         Color = MYCOLORS[1:nrow(dfcolors)])
  
  
  #scales::show_col(dfcolors$Color) # Visualize the colors
  
  
  #############
  ## Outputs ##
  #############
  
  
  
  # Dashboard Tab
  #~~~~~~~~~~~~~~~
  
  ## Total annual income ------------------------------------------
  output$TAI <- renderInfoBox({
    infoBox(
      value = paste("R$", format(annual_summary()$TAI, big.mark=","), sep= " "), 
      title = "Total annual income", 
      icon = icon("coins"),
      color = "navy",
      fill = TRUE
    )
  })
  
  
  ## Total annual expenses ------------------------------------------
  output$TAE <- renderInfoBox({
    infoBox(
      value = paste("R$", format(annual_summary()$TAE, big.mark=","), sep= " "), 
      title = "Total annual expsenses", 
      #icon = icon("thumbs-up", lib = "glyphicon"),
      icon = icon("wallet"),
      color = "light-blue",
      fill = TRUE
    )
    
  })
  
  # Total annual savings ------------------------------------------
  output$TAS <- renderInfoBox({
    infoBox(
      value = paste("R$", format(annual_summary()$TAS, big.mark=","), sep= " "), 
      title = "Total annual savings", 
      icon = icon("sack-dollar"),
      color = "blue",
      fill = TRUE
    )
  })
  
  
  
  ## Donut chart: Income vs. Expense (%) --------------------------
  output$IncomeExpense <- renderPlot({
    
    THIS_YEAR <- year(Sys.Date())
    THIS_MONTH <- month(Sys.Date(), label = T)

    
    ### Calculate incomve vs. expense in percentage
    tmp <- df() %>%
           filter(Mes %in% THIS_MONTH & Ano %in% THIS_YEAR) %>%
           summarise(expenses = sum(Valor_BRL),
                     income = unique(Renda),
                     x = 1,
                     budget = x - (expenses/income),
                     expense = x-budget) %>%
            select(3:5) %>%
            tidyr::pivot_longer(cols = -x)
    
    
    ### Define text label to appear in the center of the plot
    text_label <- percent(tmp$value, accuracy = 1)
    
    ### Go for the plot
    ggplot(tmp, aes(x = x, y = value, fill = name)) +
      
      # Add a bar, but don't add the legend
      geom_col(show.legend = T,
               color = 'white',
               size=1) +
      
      coord_polar(theta = "y",
                  direction = 1) +
      
      xlim(c(-2, 2)) +  # Set the limits, which is important for adding the hole
      scale_fill_manual(values = c("grey90", "#f2a262"),
                        labels = c("Remaining budget", "Expenses")) +
      
      
      # Set theme_void() to remove grid lines and everything else from the plot
      theme_void() +
      
      
      # Add the big number in the center of the hole
      annotate("text",
               label = text_label[2],
               #family = font_family,
               fontface = "bold",
               color = "#f2a262",
               size = 12,
               x = -2,
               y = 0) +
      
      labs(title = "Current expenses (R$)") +
      
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 11, face='bold', color = 'gray30'),
            plot.title = element_text(hjust = 0.5, face = 'bold', color = 'gray15'),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"),
            panel.background = element_rect(fill = "transparent",
                                            colour = NA_character_) # necessary to avoid drawing panel outline
            )
    
    
      
  })

  
  
  ## Bar chart: Monthly income vs. expense (%) --------------------------
  output$MonthlyExpenses <- renderPlot({
    
    THIS_YEAR <- year(Sys.Date())
    
    ## Prepare the data
    tmp <- df() %>%
          group_by(Ano, Mes) %>%
          filter(Ano %in% THIS_YEAR) %>%
          summarise(expenses = sum(Valor_BRL),
                      income = unique(Renda)) %>%
          mutate(diff = income-expenses,
                   diff_p = 100-100*((income-expenses) / income),
                   Budget = ifelse(diff >= 0, 'Below', 'Above'))
          
    ## Set factor
    tmp$Budget <- as.factor(tmp$Budget)
    
    
    ## Go for the plot
    ggplot(tmp, aes(x=Mes, y=expenses, col = Budget, group = 1)) +
      geom_bar(stat = "identity",
               col = '#ea8c4c',
               fill = '#f2a262',
               size = 0.8,
               alpha = 0.8) +
      geom_point(size=NA) +
      # geom_line(aes(x = Mes, y = income), 
      #           stat="identity", 
      #           color="#238d7b",
      #           size=1,
      #           linetype = "dashed") +
      geom_area( aes(x=Mes, y=income, col = Budget, group = 1),
                 size = 0.9, 
                 alpha = 0.3, 
                 linetype = "dashed",
                 color="#238d7b",
                 fill="#238d7b") +
      
      geom_text(aes(label = paste(round(diff_p, 0), "%", sep=' '),  
                    group = Mes, 
                    col = Budget),
                position = position_dodge2(width = 1, preserve = "single"),
                vjust=-0.5,
                size = 4,
                fontface = 'bold',
                show.legend=F) +
      
      
      scale_y_continuous(sec.axis = sec_axis(~.,name="Income (R$)")) +
      
      
      scale_color_manual(name = "Budget", values = c("darkred", "#274653")) +
      guides(colour=guide_legend(override.aes=list(size=2))) +
      
      # geom_text(aes(label=round(diff_p, 0), y = 1000), 
      #           position = position_dodge(0.90)) +
      
      #geom_hline(yintercept = tmp$income, linetype='dotted') +
      ylab('Expenses (R$)') +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 12, face='bold', color = 'gray20'),
            legend.text = element_text(size = 10, face='bold', color = 'gray30'),
            axis.text = element_text(size = 12, face = 'bold'),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12, face = 'bold'),
            #panel.border = element_blank(),
            #panel.grid=element_blank(),
            #axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold"),
            axis.text.y.right = element_text(color = "#238d7b"), 
            axis.title.y.right = element_text(color = "#238d7b")
      )
    
    
    
  })
  
  
  ## Treemap chart: Expenses per category (%) --------------------------
  output$CategoryExpenses <- renderPlot({
    
    # Select current month
    TODAY <- Sys.Date()
    THIS_MONTH <- month(TODAY, label = T)
    THIS_YEAR <- year(TODAY)
    
    # Subset & summarize expenses by category
    tmp <- df() %>% 
      filter(Mes %in% THIS_MONTH & Ano %in% THIS_YEAR) %>%
      group_by(Categoria) %>%
      summarise(Total_cost = sum(Valor_BRL)) %>%
      mutate(Total_perc = Total_cost / sum(Total_cost) *100)
    
    
    ## Go for the plot
    ggplot(tmp, aes(area = Total_perc,
                    fill = Categoria,
                    
                    # TODO: Adjust labels such that each information corresponds to one row
                    label = paste(paste(Categoria, "\n"), paste( paste(round(Total_cost,1), sep = "\n"), paste( "(", round(Total_perc,1), "%", ")", sep = ""))))) +
      #label= paste(Categoria, paste(round(Total_perc,1), "%"), sep = "\n\n"))) + 
      geom_treemap(layout="squarified",
                   colour = "white", size = 4) + 
      geom_treemap_text(place = "centre",
                        size = 12,
                        colour = "white",
                        reflow = T,
                        fontface = 'bold') + 
      #scale_fill_brewer(palette = "BrBG") +
      # scale_fill_viridis_d(direction = -1) +
      #scale_fill_manual(values = lacroix_palette("PeachPear",type = "continuous")) +
      scale_fill_manual(values = COLORS)  +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 13, face = 'bold', color = 'gray15'),
            legend.text = element_text(size = 11,  color = 'gray30'))
    
    
    
    ## Lolipop chart: Top expenses ---------------------------------------------------
    output$TopExpenses <- renderPlot({
      
      ## Define time window
      TODAY <- Sys.Date()
      THIS_MONTH <- month(TODAY, label = T)
      THIS_YEAR <- year(TODAY)
      
      
      ## Filter & prepare data
      tmp <- df() %>% 
             filter(Mes %in% THIS_MONTH & Ano %in% THIS_YEAR) %>%
             arrange(desc(Valor_BRL)) %>% 
             slice(1:10) %>%
             mutate(ID = as.factor(1:nrow(.))) %>%
             merge(., dfcolors, by = "Categoria")
      
      
      ## Go for the plot
      ggplot(tmp, aes(x = ID, y = Valor_BRL)) +
        geom_segment(aes(x = ID, xend = ID, y = 0, yend = Valor_BRL), 
                     color = tmp$Color, 
                     lwd = 4) +
        geom_point(size = 8, pch = 21, bg = tmp$Color, col = "white", stroke = 3) +
        coord_flip() +
        #scale_fill_discrete(limits=rev)
        #expand_limits(x = 0, y = 0) +
        scale_y_continuous(expand = c(0, 0), limits=c(0, (100 + max(tmp$Valor_BRL)))) +
        scale_x_discrete(labels = c("1" = paste(tmp[1,"Categoria"]),
                                    "2" = paste(tmp[2,"Categoria"]),
                                    "3" = paste(tmp[3,"Categoria"]),
                                    "4" = paste(tmp[4,"Categoria"]),
                                    "5" = paste(tmp[5,"Categoria"]),
                                    "6" = paste(tmp[6,"Categoria"]),
                                    "7" = paste(tmp[7,"Categoria"]),
                                    "8" = paste(tmp[8,"Categoria"]),
                                    "9" = paste(tmp[9,"Categoria"]),
                                    "10" = paste(tmp[10,"Categoria"])
        ),
        limits = rev) +
        labs(y = "Expense (R$)") +
        theme_pubr() +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.title.x = element_text(size = 15, face = 'bold'))
      
    })
    
    
    
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

