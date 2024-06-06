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
library(shinydashboard)


#~~~~~~~~~~~~~~~~~~~~~~~~
# Set dashboard layout
#~~~~~~~~~~~~~~~~~~~~~~~~


# 1) Header
#~~~~~~~~~~~~
header <- dashboardHeader(title = "Budget Planner")


# 2) Sidebar
#~~~~~~~~~~~~
sidebar <-  dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Table", tabName = "table", icon = icon("th")),
    menuItem("GitHub", href = "https://github.com/mcruf", icon = icon("github"))
    
  )
)



# 3) Body
#~~~~~~~~
body <- dashboardBody(
  
  # Dynamic valueBoxes
  valueBoxOutput("TAI"), #Total annual income
  valueBoxOutput("TAE"), #Total annual expense
  valueBoxOutput("TAS") #Total annual savings
                      
  ) 


# 4) Put all together
#~~~~~~~~~~~~~~~~~~~
ui <- dashboardPage(header,
                    sidebar,
                    body,
                    skin ='yellow')



# # Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )
