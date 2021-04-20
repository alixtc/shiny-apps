library(shinyWidgets)
library(plotly)
library(sf)
library(raster)
library(viridis)
library(RcppRoll)
library(tidyverse)




CSS_CODE <-  c(".shiny-notification {
             position:fixed;
             top: calc(25%);
             left: calc(50%);
             }
             ")

# Define UI for application that draws a histogram
myui <- shinyUI({fluidPage(
    theme = bslib::bs_theme(version = 4,
                            bootswatch = "simplex", 
                            secondary = "#D9230F"),
    
    navbarPage( inverse = TRUE, #Needed to fill the Navbar with the main color
                
                title = "Purchase of pesticides products in France",
                
                # First tab
                tabPanel(
                    title ="France", 
                    
                    titlePanel("Mainland Data"),
                    
                    
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Map", 
                                     h6("Legend: Graphical representation of the amount of phytosanitary products 
               purchased each year in France for each departement."),
               shinycssloaders::withSpinner(plotOutput("france_map", inline = TRUE #,  width = "140%"#, height = "auto"
               ))),
               tabPanel("Bar Chart",
                        h6("Note: Use this interactive graph to display the desired information about each compound."),
                        plotlyOutput("bar_graph", width = "140%", height = 800))
                        )
                    )
                ),
               
               # 2nd General panel
               tabPanel(
                   title ="Product Comparison", 
                   
                   titlePanel("Global evolution of purchased quantity per substance"),
                   
                   # Sidebar with a slider inputs for substance and year
                   
                   
                   fluidRow(h3("Select chemical substances:")),
                   fluidRow(
                       column(4, radioButtons(
                           "sort_names", 
                           label = "Sort substance names by:", 
                           choices = list("Quantity used (decreasing)" = 1, 
                                          "Alphabetic order" = 2),
                           selected = 1)),
                       column(4,pickerInput(
                           "compound",
                           choices = choice_subs_import_order ,
                           options = list(`actions-box` = TRUE),
                           multiple = TRUE
                       )), 
                       column(4, actionButton(
                           "draw",
                           label =  "Draw the comparison plot"
                       ))
                   ),
                   
                   # Show a plot of the generated distribution
                   fluidRow(
                       plotOutput("compound_comp", height = 800)
                       
                   )
               ),
               
               # 3rd Tab
               tabPanel(
                   
                   # ------ Appropirate Notification placement -----
                   tags$head(tags$style(HTML(CSS_CODE))),
                   # ------ 
                   
                   title ="Department Analysis", 
                   
                   titlePanel("Evolution of purchased quantity per departement"),
                   
                   # Sidebar with a slider inputs for departement and substance
                   sidebarLayout(
                       sidebarPanel(
                           selectInput(
                               "da_regions",
                               label = "Select regions",
                               choices = c("",sort(unique(dep_regions$nom_reg))) ,
                               selected = NULL
                               
                           ),
                           selectInput(
                               "da_departement",
                               "Select departements:", 
                               choices = c("",dep_regions$departement),
                               selected = NULL
                           ),
                           pickerInput(
                               "da_type_of_subs",
                               label = "Select substance mode of action",
                               choices = na.omit(unique(main_substances$effect)) ,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                           ),
                           pickerInput(
                               "da_subst",
                               label = "Select substances",
                               choices = choices_substances ,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                           ),
                           sliderTextInput(
                               "da_years",
                               label = "Select years to analyze",
                               choices = choices_year,
                               selected = c(2008,2019)
                           ),
                           (actionButton("da_plot", "Plot")),
                           (actionButton("da_reset", "Reset"))
                       ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(plotOutput("da_plot", height = 700))
                   )
               ),
               tabPanel(
                   title = "Table data",
                   titlePanel("Total quantity of substance per year, departement"),
                   dataTableOutput("whole_df")
                   
               )
    ),
    # Footnote,
    hr(),
    p("The data were collected on data.gouv.fr, in April 2021.
          Additional information on substance effects were collected from the
          available pages on Wikipedia and merge with main dataset based on CAS
          number."),
    p("The group \"Other\" correponds to all the substances who each represented less that 0.5% of total.")
)})

