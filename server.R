#############################################
#                                           #
#             Back-end of BPapp             #
#                                           #
#############################################

# Developed by: Marie-Christine Rufener
# Last update: 5 June 2024
# For queries, feedback, and bugs, contact: < macrufener@gmail.com >


server <- function(input, output) {
  

  output$TAI <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Total annual income", icon = icon("list"),
      color = "navy"
    )
  })

  
  output$TAE <- renderValueBox({
    valueBox(
      "80%", "Total annual expsenses", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
 
  })
  
  output$TAS <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Total annual savings", icon = icon("list"),
      color = "blue"
    )
  })
  
  
  
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
