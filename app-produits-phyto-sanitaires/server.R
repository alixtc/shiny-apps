

# Define server logic required to draw a histogram
myserver <- function(session, input, output) {
    

    
    # Dynamic UI for Product Comparison page
    observeEvent(input$sort_names,{
        if (input$sort_names == 1) {
            #Sort names by importance
            updatePickerInput( 
                session,
                inputId = "compound",
                choices = choice_subs_import_order
            )
        }else if(input$sort_names == 2){
            # Sort names alphabetically
            updatePickerInput(
                session,
                inputId =  "compound",
                choices = choice_subs_import_order %>% unlist() %>% sort()
            )
        }
    })
    
    # Product Comparison page: Select data and calculate values
    comp_data <-  eventReactive(input$draw, {
        
        req(input$compound)
        condensed_df %>% 
            filter(substance %in% input$compound) %>% 
            group_by(annee, substance) %>% 
            summarise(quantite = sum(quantite), .groups = "drop") 
    })
   

 
    
    # DA Page: Dynamic UI
    
    # Display only departements belonging to a selected region
    observeEvent(input$da_regions, {
        req(input$da_regions)
        
        possible_dept <- dep_regions %>% 
            filter(nom_reg %in% input$da_regions) %>% 
            pull(departement)
        
        updateSelectInput(session,
                          inputId = "da_departement",
                          choices =  as.list(possible_dept)
        )
    })
    
    # Display only substances corrsponding to selected effect
    observeEvent(input$da_type_of_subs, {
        req(input$da_type_of_subs)
        
        choices_subs <- main_substances %>%
            filter(effect %in% input$da_type_of_subs) %>%
            pull(substance)
        
        updatePickerInput(session,
                          inputId = "da_subst", 
                          choices = choices_subs)
    })
    
    # DA Page: Variable selection
    da_data_selec <- eventReactive(input$da_plot, {
        prog_notif <- showNotification("Drawing map. Please wait.",  duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(prog_notif), add = TRUE)
        
        req(input$da_departement, input$da_subst,input$da_years)
        
        condensed_df %>% 
            filter(annee %in% input$da_years[1]:input$da_years[2]) %>% 
            filter(substance %in% input$da_subst) %>% 
            filter(departement %in% input$da_departement) 
         
        
    })
    
    # ------ DA RESET------
    
    observeEvent(input$da_reset, {
        updateSelectInput(session,
                          inputId = "da_regions",
                          choices = c("",sort(unique(dep_regions$nom_reg))),
                          selected = NULL        
        )
        updateSelectInput(session,
                          inputId = "da_departement",
                          choices = c("",dep_regions$departement),
                          selected = NULL
        )
        updatePickerInput(session,
                          inputId = "da_type_of_subs",
                          choices = na.omit(unique(main_substances$effect)) 
        )
        updatePickerInput(session,
                          inputId = "da_subst",
                          choices = choices_substances
        )
    })

    
    # ----- OUTPUTS ------
    
    output$france_map <- renderPlot({ 
        
        # Calculate total quantities per year and department
        data_for_map <- condensed_df %>% 
            group_by(annee, departement) %>% 
            summarise(quantite = sum(quantite))
        
        par(mar = rep(0, 4))
        carte %>% 
            left_join(data_for_map, by= "departement") %>%
            ggplot( aes(fill = quantite)) +
            facet_wrap(~annee) +
            scale_fill_viridis(name = "Quantity (in tons)", 
                               labels = function(x) format(x / 1000)) +
            geom_sf(show.legend =TRUE,
                    lwd = 0.5) +  # Set thickness of border region (line width)
            coord_sf(crs = 4326)
        # +
        #     theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        #           plot.background=element_rect(fill="grey", colour=NA))
        # 
            
        
    }, width = 1100, height = 800)
    
    # Plot with the quantity of each substance per year
    output$bar_graph <- renderPlotly({
        ggplotly({
            
            # Smooth the quantity of subtance using a 3 year sliding window
            # First year = average year 1  + year 2
            # Last year = average year(n) + year(n-1)
            condensed_df %>% 
                group_by(annee, substance) %>% 
                summarise(quantite = sum(quantite)) %>% 
                
                # Data smoothing Y-1, Y+1
                # group_by(substance) %>% 
                # group_modify(
                #     ~mutate(.x,  quantite = RcppRoll::roll_mean(
                #         quantite, 
                #         n = 3, 
                #         fill = c( (quantite[1] + quantite[2])/2, 
                #                   0, 
                #                   (quantite[length(quantite)] + quantite[length(quantite)])/2 ), 
                #         align = "center"))) %>% 
            
                ggplot(aes(factor(annee), quantite, fill = (substance))) + 
                geom_col(position = "stack") +
                theme(plot.margin = unit(c(0.25,0.1,0,0.25), "inches"))   +
                xlab("Year") + ylab("Quantity (tons)") +
                scale_y_continuous(labels = function(x) format(x / 1000))
        })
    })
    
    # Product Comparison page: Plot the data
    output$compound_comp <- renderPlot({
        comp_data() %>% 
            ggplot(aes(factor(annee, ordered = TRUE), quantite,  group = substance, color = substance)) + 
            geom_line() + 
            geom_point() +
            facet_wrap(~substance, scales = "free_y") + 
            guides(color = FALSE)  +
            xlab("Year") + ylab("Quantity (tons)") +
            scale_y_continuous(labels = function(x) format(x / 1000))
    })
    
    
    # For a given departement plot time course for selected substances
    output$da_plot <- renderPlot({
        
        da_data_selec() %>% 
            ggplot(aes(factor(annee, ordered = TRUE), 
                       y = quantite, 
                       group = substance,
                       color = substance)) +
            geom_point() +
            geom_line() +
            facet_wrap(~substance, ncol = 3) +
            xlab("Year") + ylab("Quantity (tons)") +
            scale_y_continuous(labels = function(x) format(x / 1000))
    })
    
    # Print the whole data frame
    output$whole_df <- renderDataTable(condensed_df)
    
}

