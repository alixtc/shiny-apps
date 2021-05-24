library(shiny)
library(shinyWidgets)
# 
# generate_fields <- function(sub_index, global_index, grid){ 
#     
#     sub_index <- (global_index - 1) * 9 + sub_index
#     # numericInput(
#     #     paste0("idx", sub_index), 
#     #     paste0("idx", sub_index), 
#     #     value = 1,  min = 1,max =  9, step = 1
#     #     )
#     if (is.na(grid[sub_index])) {
#         
#         # textInput(
#         #     paste0("idx", sub_index),
#         #     label = NULL,
#         #     placeholder = " "
#         # )
#         # 
#         # numericInput(
#         #     paste0("idx", sub_index),
#         #     label = NULL,
#         #     label = paste0("idx", sub_index),
#         #     value = 1,  min = 1,max =  9, step = 1
#         # )
#         # 
#         pickerInput(
#             paste0("idx", sub_index),
#             label = NULL,
#             choices = 1:9,
#             selected = NULL,
#             width = 'fit'
#         )
#     } else{
#         h4(paste0(grid[sub_index], '<\n. '))
#        
#     }
# 
# }
# generate_columns <- function(global_index, grid){ 
#     column(map( 1:9, generate_fields, global_index, grid), width = 1)
# }
# 

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Sudoku Solver"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("difficulty",
                        "Choose difficulty level",
                        choices = list(
                            easy = 1,
                            medium = 2,
                            hard = 3,
                            insane = 4),
                        selected = 2),
            actionButton('getGrid', 'Get a new sudoku grid'),
            actionButton('solve_grid', 'Solve the Grid')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("initial_grid"),
            # uiOutput('solving_puzzle'),
            # fluidRow(
            #     map(1:9, generate_columns)
            # )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Initialize the structure containing the sudoku grid
    grid <- reactiveValues(
        grid = NA
    )
    
    # Get the grid from the registered website
    observeEvent(input$getGrid, {
        grid$grid <- grid_fetch(input$difficulty)
    })
    
    # Solve the grid automatically through recursive search
    observeEvent(input$solve_grid, {
        req(grid$grid)
        solution <- successive_search(grid$grid)
        grid$grid <- solution$grid
    })
    
    # Print a 9x9 grid with text inputs below the actual sudoku
    # observeEvent(input$getGrid,{ 
    #     output$solving_puzzle <- renderUI({
    #         map(1:9, generate_columns, grid$grid)
    #     })
    # })
    
    output$initial_grid <- renderTable({
        req(grid$grid)
        grid$grid
    }, width = "400px", colnames = FALSE, align = 'c')
}

# Run the application 
shinyApp(ui = ui, server = server)
