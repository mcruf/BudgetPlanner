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
library(DT)



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
body <- dashboardBody(
  
  tabItems(
    
    ## Dashboard page
    tabItem(
      tabName = "dashboard",
      #h2("hello there"),
      
      
      fluidRow(
      # box(width = 12, 
      #     title = "General Summary", 
      #     status = "warning",
      #     solidHeader = TRUE,
      # Dynamic valueBoxes
      infoBoxOutput("TAI"), #Total annual income
      infoBoxOutput("TAE"), #Total annual expense
      infoBoxOutput("TAS"), #Total annual savings 
      ),
      
      fluidRow(), # Add extra row
      
      fluidRow(
        column(
          width = 4,
          plotOutput("IncomeExpense")
        ),
        column(
          width = 8,
          plotOutput("MonthlyExpenses")
        )
      ),
  
      
      
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
                    skin ='yellow')


