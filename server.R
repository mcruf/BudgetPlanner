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
    COLNAMES <- c("Cost",
                  "Category",
                  "Date",
                  "Year",
                  "Month",
                  "Payment",
                  "Obs")
    
    
    # Match colnames
    if(length(intersect(colnames(df), COLNAMES)) != length(COLNAMES))
      stop("Please verify your column names. They should match to the same column names of the dataset in the example.")
    
    
    # 2.2) Set appropriate data type to columns
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Factor
    df[, c("Category",
           "Year", 
           "Month", 
           "Payment")] <- lapply(df[, c("Category",
                                        "Year", 
                                        "Month", 
                                        "Payment")], factor)
    
    
    # Date
    df$Date <- as.Date(df$Date, "%d.%m.%Y")
    
    
    # 2.3) Reorder data by Date & refactor month levels
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Reorder by date
    df <- df[order(df$Date),]
    
    
    # Rename factor levels of Month
    df$Month <- dplyr::recode_factor(df$Month,
                                     January = "Jan",
                                     February = "Feb",
                                     March = "Mar",
                                     April = "Apr",
                                     May = "May",
                                     June = "Jun",
                                     July = "Jul",
                                     August = "Aug",
                                     September = "Sep",
                                     October  = "Oct",
                                     November = "Nov",
                                     December = "Dec")
    
    return(df)
    
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~
  # 3) Summarize data
  #~~~~~~~~~~~~~~~~~~~
  
  annual_summary <- reactive({
    
    THIS_YEAR <- year(Sys.Date())
    THIS_MONTH <- month(Sys.Date(), label = T)
    
    
    df() %>%
      group_by(Year, Month) %>%
      summarise(TAI = unique(Income),
                TAE = sum(Cost),
                TAS = TAI - TAE) %>%
      summarise(TAI = round(sum(TAI),2),
                TAE = round(sum(TAE),2),
                TAS = round(sum(TAS)),2) 

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
  dfcolors <- reactive({
    
              THIS_YEAR <- year(Sys.Date())
              THIS_MONTH <- month(Sys.Date(), label = T)
              
              datcol <- df() %>% 
                          filter(Month %in% THIS_MONTH & Year %in% THIS_YEAR) %>%
                          droplevels() %>%
                          group_by(Category) %>%
                          summarise(Total_cost = sum(Cost)) %>%
                          mutate(Total_perc = Total_cost / sum(Total_cost) * 100) %>%
                          select("Category")
              
              return(datcol)
              
              
              })
  
  
  
  #scales::show_col(dfcolors$Color) # Visualize the colors
  
  # 4.2) User inputs
  #~~~~~~~~~~~~~~~~~
  observeEvent(df(),{
    ## Define user-inputs
    CATEGORIES <- unique(df()$Category)
    
    ## Define expense categories to be selected by the user based on its dataset
    updatePickerInput(inputId = "Category", choices = CATEGORIES)
  })
  
  
  
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
      fill = T
    )
  })
  
  ## Total annual expenses ------------------------------------------
  output$TAE <- renderInfoBox({
    infoBox(
      value = paste("R$", format(annual_summary()$TAE, big.mark=","), sep= " "), 
      title = "Total annual expsenses", 
      icon = icon("wallet"),
      color = "light-blue",
      fill = T
    )
    
  })
  
  # Total annual savings ------------------------------------------
  output$TAS <- renderInfoBox({
    infoBox(
      value = paste("R$", format(annual_summary()$TAS, big.mark=","), sep= " "),
      title = "Total annual savings", 
      icon = icon("sack-dollar"),
      color = "blue",
      fill = T
    )
  })
  
  
  
  ## Donut chart: Income vs. Expense (%) --------------------------
  output$IncomeExpense <- renderPlot({
    
    THIS_YEAR <- year(Sys.Date())
    THIS_MONTH <- month(Sys.Date(), label = T)

    
    ### Calculate incomve vs. expense in percentage
    tmp <- df() %>%
           filter(Month %in% THIS_MONTH & Year %in% THIS_YEAR) %>%
           summarise(expenses = sum(Cost),
                     income = unique(Income),
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
      scale_fill_manual(values = c("grey90", "#e76f51"),
                        labels = c("Remaining \n budget", "Expenses\n")) +
      
      
      # Set theme_void() to remove grid lines and everything else from the plot
      theme_void() +
      
      
      # Add the big number in the center of the hole
      annotate("text",
               label = text_label[2],
               #family = font_family,
               fontface = "bold",
               color = "#e76f51",
               size = 12,
               x = -2,
               y = 0) +
      
      #labs(title = "Current expenses (R$)") +
      
      theme(legend.position = "left",
            legend.title = element_blank(),
            legend.text = element_text(size = 11, face='bold', color = 'gray30'),
            plot.title = element_text(hjust = 0.5, face = 'bold', color = 'gray15'),
            plot.margin = margin(0,0,0,0, "cm"),
            #plot.margin = unit(c(0, 0, 0, 0), "null"),
            #panel.margin = unit(c(0, 0, 0, 0), "null"),
            panel.background = element_rect(fill = "transparent",
                                            colour = NA_character_) # necessary to avoid drawing panel outline
            )
    
    
      
  })

  
  ## Bar chart: Monthly income vs. expense (%) --------------------------
  output$MonthlyExpenses <- renderPlotly({
    
    THIS_YEAR <- year(Sys.Date())
    
    ## Prepare the data
    tmp <- df() %>%
          group_by(Year, Month) %>%
          filter(Year %in% THIS_YEAR) %>%
          summarise(expenses = sum(Cost),
                      income = unique(Income)) %>%
          mutate(diff = income-expenses,
                   diff_p = 100-100*((income-expenses) / income),
                   Budget = ifelse(diff >= 0, 'Below', 'Above'))
          
    ## Set factor
    tmp$Budget <- as.factor(tmp$Budget)
    
    
    ## Go for the plot
    
    ### Prepare data for the plot
    tmp$expenses <- (tmp$expenses*-1) #Convert expenses to negative values
    tmp2 <- tidyr::gather(tmp, Type, Value, expenses:income, factor_key=TRUE) #Convert from wide to long
    
    
    ### Pretty breaks
    upper <- round(min(tmp2$Value), digits = -4)
    lower <- round(max(tmp2$Value), digits = -4)
    
    
    ggplotly(tooltip = "text", #Define text when hovering over plot
             
        ggplot(tmp2, aes(x=Month, y=Value)) + 
          geom_bar(aes(fill=str_to_title(Type), col = str_to_title(Type), text = ifelse(tmp2$Type == "income", 
                                                            paste("Income: R$", round(Value,2)), 
                                                            paste("Expense: R$", round(Value,2)))),
                   stat="identity", 
                   position="identity", alpha = 0.7,
                   size = 0.7) +
          geom_hline(yintercept=0, color = "gray40", size=1) +
          geom_line(aes(y = diff, group =1), col = "#e76f51", size = 1.5) +
          geom_point(aes(y = diff, group =1, text = paste("Savings: R$", round(diff, 2))), 
                     col = "#e76f51", size =2.5) +
          scale_fill_manual(values = c("#efd595", "#238d7b")) +
          scale_color_manual(values = c("#efd595", "#238d7b")) +
          theme_minimal() +
          #ylab('Income/Expense (R$)') +
          ylab('') +
          guides(colour=guide_legend(title=""),
                 fill = guide_legend(title="")) +     
          scale_y_continuous(breaks = seq(upper, lower, 5000)) +
    
          theme(legend.position = "top",
                title.hjust =0.5,
                #legend.title = element_text(size = 13, face='bold', color = 'gray20'),
                legend.text = element_text(size = 11, face='bold', color = 'gray30'),

                axis.text = element_text(size = 12, face = 'bold', color = "gray50"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 15, 
                                            face = 'bold', 
                                            color = "gray30"),
                
                axis.ticks = element_line(colour = NULL), 
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(colour = "gray30"), 
                axis.line = element_line(colour = "gray30"), 
                axis.line.y = element_blank(),
                #axis.text.y.right = element_text(color = "#238d7b"), 
                #axis.title.y.right = element_text(color = "#238d7b"),
                
                #panel.border = element_blank(),
                panel.grid=element_blank(),
                
                plot.margin = unit(c(t=0,b=0,r=0,l=0), "cm")
          )
    ) %>%
      layout(legend=list(#x=0, 
                         #xanchor='left',
                         hoverinfo = 'y',
                         x = 1, 
                         y = 0.5,
                         #yanchor='top',
                         #xanchor='center',
                         orientation='v'))  
    
    ##### Older alternative
    # ggplot(tmp, aes(x=Month, y=expenses, col = Budget, group = 1)) +
    #   geom_bar(stat = "identity",
    #            col = '#edcd7e',
    #            fill = '#efd595',
    #            size = 0.8,
    #            alpha = 0.8) +
    #   geom_point(size=NA) +
    #   # geom_line(aes(x = Mes, y = income), 
    #   #           stat="identity", 
    #   #           color="#238d7b",
    #   #           size=1,
    #   #           linetype = "dashed") +
    #   geom_area( aes(x=Month, y=income, col = Budget, group = 1),
    #              size = 0.9, 
    #              alpha = 0.3, 
    #              linetype = "dashed",
    #              color="#238d7b",
    #              fill="#238d7b") +
    #   
    #   geom_text(aes(label = paste(round(diff_p, 0), "%", sep=' '),  
    #                 group = Month, 
    #                 col = Budget),
    #             position = position_dodge2(width = 1, preserve = "single"),
    #             vjust=-0.5,
    #             size = 4,
    #             fontface = 'bold',
    #             show.legend=F) +
    #   
    #   
    #   scale_y_continuous(sec.axis = sec_axis(~.,name="Income (R$)")) +
    #   
    #   
    #   scale_color_manual(name = "Budget", values = c("darkred", "#274653")) +
    #   guides(colour=guide_legend(override.aes=list(size=2))) +
    #   
    #   # geom_text(aes(label=round(diff_p, 0), y = 1000), 
    #   #           position = position_dodge(0.90)) +
    #   
    #   #geom_hline(yintercept = tmp$income, linetype='dotted') +
    #   ylab('Expenses (R$)') +
    #   theme_minimal() +
    #   theme(legend.position = "bottom",
    #         legend.title = element_text(size = 15, face='bold', color = 'gray20'),
    #         legend.text = element_text(size = 14, face='bold', color = 'gray30'),
    #         axis.text = element_text(size = 14, face = 'bold'),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_text(size = 16, face = 'bold'),
    #         #panel.border = element_blank(),
    #         panel.grid=element_blank(),
    #         #axis.ticks = element_blank(),
    #         plot.title=element_text(size=14, face="bold"),
    #         axis.text.y.right = element_text(color = "#238d7b"), 
    #         axis.title.y.right = element_text(color = "#238d7b"),
    #         plot.margin = unit(c(t=0,b=0,r=0,l=0), "cm")
    #   )
    # 
  
  })
  
  
  ## Treemap chart: Expenses per category (%) --------------------------
  output$CategoryExpenses <- renderPlot({
    
    # Select current month
    TODAY <- Sys.Date()
    THIS_MONTH <- month(TODAY, label = T)
    THIS_YEAR <- year(TODAY)
    
  
    
    # Subset & summarize expenses by category
    tmp <- df() %>% 
      filter(Month %in% THIS_MONTH & Year %in% THIS_YEAR) %>%
      droplevels() %>%
      group_by(Category) %>%
      summarise(Total_cost = sum(Cost)) %>%
      mutate(Total_perc = Total_cost / sum(Total_cost) *100)
    
    
    ## Go for the plot
    ggplot(tmp, aes(area = Total_perc,
                    fill = Category, 
                    label = paste(paste(Category, "\n"), paste( paste("R$",round(Total_cost,1)), paste( "(", round(Total_perc,1), "%", ")", sep = ""))))) +
      #label= paste(Categoria, paste(round(Total_perc,1), "%"), sep = "\n\n"))) + 
      #labs(title="Customized Tree Plot using ggplot and treemapify in R") +
      geom_treemap(layout="squarified",
                   colour = "white", size = 4) + 
      geom_treemap_text(place = "centre",
                        size = 14,
                        colour = "white",
                        reflow = T,
                        fontface = 'bold') + 
      #scale_fill_brewer(palette = "BrBG") +
      # scale_fill_viridis_d(direction = -1) +
      #scale_fill_manual(values = lacroix_palette("PeachPear",type = "continuous")) +
      #scale_fill_manual(values = MYCOLORS)  +
      scale_fill_my_color_d("main") +
      theme(legend.position = "none",
            legend.title = element_text(size = 13, face = 'bold', color = 'gray15'),
            legend.text = element_text(size = 11,  color = 'gray30'))
    
  })
    
  ## Lolipop chart: Top expenses ---------------------------------------------------
  output$TopExpenses <- renderPlotly({

      ## Define time window
      TODAY <- Sys.Date()
      THIS_MONTH <- month(TODAY, label = T)
      THIS_YEAR <- year(TODAY)

      
      ## Set default colors
      dfcolors <- dfcolors()
      dfcolors <- data.frame(Category = dfcolors, 
                             Color = my_palettes(name = "main", 
                                                 type = 'continuous')[1:nrow(dfcolors)])
      # dfcolors <- data.frame(Category = dfcolors,
      #                        Color = MYCOLORS[1:nrow(dfcolors)])

      ## Filter & prepare data
      tmp <- df() %>%
             filter(Month %in% THIS_MONTH & Year %in% THIS_YEAR) %>%
             droplevels() %>%
             arrange(desc(Cost)) %>%
             slice(1:10) %>%
             mutate(ID = as.factor(1:nrow(.))) %>%
             merge(., dfcolors, by = "Category")


    #   ## Go for the plot
      ggplotly(tooltip = "text",
               ggplot(tmp, aes(x = ID, y = Cost,  col = Category,fill = Category, label = Cost)) +
                 geom_segment(aes(x = ID, xend = ID, y = 0, yend = Cost, color = Category),
                              lwd = 4) +
                 geom_point(aes(text = paste("Item: ", Obs), col = Category), size = 5,  fill = 'white',pch = 21, stroke = 2) +
                 #geom_point(aes(text = paste("Item: ", Obs)), size = 8, pch = 21,  col = "white", stroke = 3) +
                 
                 coord_flip() +
                 scale_colour_my_color_d("main") +

                 # scale_color_manual(
                 #   "Category", 
                 #   values = c("Alimentação" = "#203a44", 
                 #              "Casa" = "#2a9d8f",
                 #              "Comunicação" = "#8ab17d",
                 #              "Lazer" = "#e9c46a",
                 #              "Outros" = "#EFD595",
                 #              "Pessoal" = "#efb366",
                 #              "Saude" = "#f4a261",
                 #              "Transporte" = "#e76f51"), 
                 # ) +
                 guides(color = "none") +
                 scale_y_continuous(expand = c(0, 0), limits=c(0, (100 + max(tmp$Cost)))) +
                 scale_x_discrete(limits = rev) +
                 labs(y = "Expense (R$)") +
                 geom_text(aes(label = paste("R$",Cost)), 
                           position = position_dodge(width = 0), 
                           #hjust = 25, 
                           fontface = "bold") +
                 theme_pubr() +
                 theme(
                   legend.position=c(0.80, 0.25),
                   #legend.title = element_text(size = 16, face = "bold"),
                   #legend.text = element_text(size = 15),
                   
                   axis.text = element_text(size = 12, face = 'bold', color = "gray50"),
                   axis.text.y = element_blank(),
                   axis.title.y = element_blank(),
                   axis.title.x = element_text(size = 13, 
                                               face = 'bold', 
                                               color = "gray35"),

                   axis.ticks = element_line(colour = NULL), 
                   axis.ticks.y = element_blank(),
                   axis.ticks.x = element_line(colour = "gray30"), 
                   axis.line = element_line(colour = "gray30"), 
                   axis.line.y = element_blank(),
                   
                   
                   #axis.text.x = element_text(size = 14),
                   

                   panel.grid=element_blank())
    
     ) %>%
       layout(legend=list(
         tracegroupgap = 2,
         x = 0.7,
         y = 0.1,
         orientation='v',
         #textfont = list(size = 2, color = 'gray30'))
         font = list(size = 13, color = "gray30"))
         ) %>%
        style(texttemplate = '%{x: 28} R$', 
              textposition = "outside") # Force text's position for ggplotly
      
    })


  
  ## Line chart: Expense breakdown by category ---------------------------------------------------
  output$CategoryBreakdown <- renderPlot({
    
    
    ## Define time window
    TODAY <- Sys.Date()
    THIS_YEAR <- year(TODAY)
    
   
   
    ## Set default colors
    dfcolors <- dfcolors()
    dfcolors <- data.frame(Category = dfcolors, 
                           Color_main = my_palettes(name = "main", 
                                               type = 'continuous')[1:nrow(dfcolors)],
                           Color_dark = my_palettes(name = "dark", 
                                                    type = 'continuous')[1:nrow(dfcolors)])
  
    COLOR <- dfcolors %>%
      filter(Category == input$Category) %>%
      select(Color_main, Color_dark)
    
    
    ## Filter & prepare data
    tmp <- df() %>%
      filter(Year %in% THIS_YEAR & Category == input$Category) %>%
      droplevels() %>%
      group_by(Month) %>%
      summarize(Total = sum(Cost)) 
    

    
    
    ## Go for the plot
    ggplot(tmp, aes(x = Month, y = Total, group = 1)) +
      ggpattern::geom_area_pattern( 
        pattern = "gradient", 
        #fill = "#E69F00", 
        alpha = 0,
        pattern_alpha = 0,
        pattern_fill  = "white",
        pattern_fill2 = paste(COLOR[1])) +
      geom_line(size = 1, col = paste(COLOR[2])) +
      geom_point(shape = 21, size = 5, stroke=3, fill=paste(COLOR[2]), col = "white") +
      geom_text(aes(label = paste("R$",round(Total,2))), 
                position = position_dodge(width = 0), 
                vjust = -1.5, 
                size = 6,
                fontface = "bold",
                col = paste(COLOR[2])) +
      labs(y = "Total expense (R$)") +
      scale_x_discrete(expand = c(0.05,0.05)) +
      theme_pubr() +
      theme(#legend.position = 'bottom',
        legend.position=c(0.80, 0.25),
        
        
        axis.text = element_text(size = 18, face = 'bold', color = "gray50"),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        axis.title = element_text(size = 20, 
                                    face = 'bold', 
                                    color = "gray35"),
        
        axis.ticks = element_line(colour = NULL), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "gray30"), 
        axis.line = element_line(colour = "gray30"), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(t=1,b=0,r=0,l=0), "cm"))
    
    
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

