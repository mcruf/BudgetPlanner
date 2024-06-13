#############################################
#                                           #
#             Front-end of BPapp            #
#                                           #
#############################################

# Developed by: Marie-Christine Rufener
# Last update: 5 June 2024
# For queries, feedback, and bugs, contact: < macrufener@gmail.com >


#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(shiny)
library(shinydashboard)
library(summaryBox)
library(DT)
library(stringr)
library(plotly)




#~~~~~~~~~~~~~~~~~~~~~~~~
# Set dashboard layout
#~~~~~~~~~~~~~~~~~~~~~~~~


# 1) Header
#~~~~~~~~~~~~
header <- dashboardHeader(title = "Budget Planner")


# 2) Sidebar
#~~~~~~~~~~~~
sidebar <-  dashboardSidebar(
  
  ## File input widget
  # fileInput(inputId = "filedata",
  #           label ="Upload CSV file",
  #           multiple=FALSE, accept=(".csv")),
  
  
  sidebarMenu(
    
    # Interactive Inputs: Integrated into the sidebar for user interaction
    fileInput(inputId = "filedata",
              label ="Upload CSV file",
              multiple=FALSE, 
              accept=(".csv")),
    
    
    # Navigation Tabs: Each tab links to different content in the main body
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # menuItem("Dashboard", fileInput("upload", "Upload csv file"),
    #          tabName = "dashboard", icon = icon("dashboard")),
    # 
    menuItem("Data", tabName = "datatable", icon = icon("database")),
    
    # External Link: Opens in a new tab or window
    menuItem("GitHub", href = "https://github.com/mcruf", icon = icon("github"))
    
    
  )
)



# 3) Body
#~~~~~~~~

## Personalize further the dashboard layout
library(fresh)

my_theme = create_theme(
  adminlte_color(
    yellow = "#2a9d8f",
    navy = "#264653",
    light_blue = "#8AB17D",
    blue = "#E9C46A",

  ),
  adminlte_global(
    content_bg = "#D8DEE9",
    #box_bg = "#D8DEE9", 
    #info_box_bg = "FFFFF"
  )
)



body <- dashboardBody(
 use_theme(my_theme),
  
  tabItems(
    
    ## Dashboard page
    tabItem(
      tabName = "dashboard",
      #h2("hello there"),
      
      
      fluidRow(
        box( title = tags$p("Annual Overview",style = 'font-size:20px;'), 
             status = "warning", 
             #background = "yellow",
             solidHeader = F,
             width = 12,
            # hr(),
      # box(width = 12, 
      #     title = "General Summary", 
      #     status = "warning",
      #     solidHeader = TRUE,
      # Dynamic valueBoxes
      infoBoxOutput("TAI"), #Total annual income
      infoBoxOutput("TAE"), #Total annual expense
      infoBoxOutput("TAS"), #Total annual savings
)
      ),
      
     # br(), # Add extra row
      
      fluidRow(
        column(
          width = 4,
          box( title = tags$p("Current expenses",style = 'font-size:20px;'), 
               status = "warning",
               collapsible = T,
               solidHeader = F,
               width = NULL,
               #hr(),
          plotOutput("IncomeExpense", height = 250)
        )),
        column(
          width = 8,
          box( title = tags$p("Monthly income & expenses",style = 'font-size:20px;'), 
               status = "warning", 
               collapsible = T,
               solidHeader = F,
               width = NULL,
               #hr(),
               plotlyOutput("MonthlyExpenses", height = 250)
          )
        )
      ),
  
      
      fluidRow(), # Add extra row
     
      
      fluidRow(
        column(
          width = 6,
          #style='padding-top:15px;',
          #br(),
          box( title = tags$p("Expense by Category", style = 'font-size:20px;'), 
               status = "warning", 
               solidHeader = F,
               collapsible = T,
               width = NULL,
               #hr(),
          plotOutput("CategoryExpenses")
          )
        ),
        column(
          width = 6,
          #style='padding-top:15px;',
          #br(),
          box( title = tags$p("Top 10 expenses",style = 'font-size:20px;'), 
               status = "warning", 
               solidHeader = F,
               collapsible = T,
               width = NULL,
               #hr(),
          plotlyOutput("TopExpenses")
        )
        )
        )
      
    ),
    
    ## Data table page
    tabItem(
      tabName = "datatable",
      dataTableOutput("mydata")
      
    )
    
  )
  
  
  
  
  # Data table
  #tabItem(tabName = "table", dataTableOutput("data"))
  
  
) 


# 4) Put all together
#~~~~~~~~~~~~~~~~~~~
ui <- dashboardPage(header,
                    sidebar,
                    body,
                    skin ='yellow',
                    
                    ## customize horizontal ruler
                    tags$head(
                      tags$style(HTML("hr {border-top: solid #f39c12;
                                           border-width: 3px;
                                           margin-top: -0.5em;}"))
                    ))


