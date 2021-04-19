library(shiny)
library(tidyverse)
library(tidymodels)



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "spacelab"),
    # Application title
    titlePanel("Example of clustering analysis on randomly generated points"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("nclusters",
                        "Number of clusters to create:",
                        min = 2,
                        max = 7,
                        value = 3),
            
            h5("Determine how data points are spread and identified with an arbitraty selection of K clusters."),
            
            sliderInput("k_to_test",
                        "Value of K:",
                        min = 2,
                        max = 10,
                        value = 3)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Real clustering of data points"),
            plotOutput("real"),
            h3("Clustering based on inputed K value"),
            plotOutput("manual"),
            h3("Additional variance explained by each value of K"),
            plotOutput("variance"),
            # h3("Analysis for all possible K values"),
            # plotOutput("systematic")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    thematic::thematic_shiny()
    centers <- reactive({
        n_cent <- input$nclusters
        tibble(
            cluster = factor(1:n_cent),
            
            # number points in each cluster determined at pseudo-random
            num_points = sample(
                c(100, 125, 150, 75), 
                size = n_cent, 
                replace = TRUE
            ) ,  
            x1 = sample(seq(-6, 6, by = 2), n_cent),  # x1 coordinate of cluster center
            x2 = sample(seq(-6, 6, by = 2), n_cent)  # x2 coordinate of cluster center
        )
    })   
    
    # Calculate random points for each center
    df <- reactive({
        req(centers)
        centers() %>% 
            mutate(
                x1 = map2(num_points, x1, rnorm),
                x2 = map2(num_points, x2, rnorm)
            ) %>% 
            unnest(c(x1,x2)) %>% 
            select(-num_points)
    })
    
    manual_clust <- eventReactive(list(df, input$k_to_test),{
        points <- df()[,2:3]
        example <- kmeans(points, input$k_to_test)$cluster
        return(factor(LETTERS[example]))
    })
    
    
    
    detailed_analysis <- eventReactive(df(),{ 
        points <- df()[,2:3]
        tibble(k = 1:10) %>%
            mutate(
                kclust = map(k, function(x){ kmeans(points, x)}),
                tidied = map(kclust, tidy),
                augmented = map(kclust, augment, points),
                glanced = map(kclust, glance)
            )
    })
    
    
    output$real <- renderPlot({
        df() %>% 
            ggplot(aes(x = x1, y= x2, color = cluster)) + geom_point(alpha = 0.75)+
            labs(color = "Cluster")
    })
    
    output$manual <- renderPlot({
        df() %>% 
            bind_cols(man_clust = manual_clust()) %>% 
            ggplot(aes(x = x1, y= x2, color = man_clust)) + geom_point(alpha = 0.75) +
            labs(color = "Manual Cluster")
    })
    
    output$variance <- renderPlot({
        detailed_analysis() %>% unnest(glanced) %>%
            ggplot(aes(factor(k), tot.withinss, group = 1)) +
            geom_line() +
            geom_point() +
            xlab(label = "Value of K") +
            ylab("Additional variance explained") 
        
    })
    
    
    # output$systematic <- renderPlot({
    #     detailed_analysis() %>%
    #         unnest(augmented) %>%
    #         ggplot(aes(x1, x2)) +geom_point( aes(color = .cluster), alpha = 0.75) + facet_wrap(~k)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)




