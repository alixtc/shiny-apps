library(bslib)
library(shiny)
library(shinyWidgets)
library(sf)
library(raster)
library(viridis)
library(RcppRoll)
library(plotly)
library(tidyverse)


source("server.R")
source("helper.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(bootswatch = "simplex", secondary = "#D9230F" ),
    navbarPage(
        title = "Purchase of pesticides products in France",
        
        # First tab
        tabPanel(
            title ="France", 
            
            titlePanel("Mainland Data"),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Map", plotOutput("france_map", width = "150%", height = 1000)),
                    tabPanel("Bar Chart", plotlyOutput("bar_graph", width = "150%", height = 800))
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
                plotOutput("compound_comp")
                
            )
        ),
        
        # 3rd Tab
        tabPanel(
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
                mainPanel(plotOutput("da_plot"))
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
    print("The data were collected on data.gouv.fr, in April 2021. 
          Additional information on substance effects were collected from the 
          available pages on Wikipedia and merge with main dataset based on CAS
          number.
          The group \"Other\" correponds to all the substances who each represented less that 0.5% of total.")
)

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1600))