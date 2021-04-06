library(shiny)
library(lme4)
library(lmerTest)
library(rstatix)
library(tidyverse)

# ---- Preprocessing file information ----


file_list <- grep("csv", list.files(), value = TRUE)
file_list <- as.list(file_list)
names(file_list) <- c("cocaine inhibition",
                      "cocaine stimulation",
                      "food inhibition",
                      "food stimulation")

# Stores in a list: list$new_name <- "old_names"
var_names <- list(injection = "inje",
                  pellets = "pellets",
                  perseveratives = "pers",
                  ratio = "ratio",
                  errors = "err",
                  food_magazine = "mang" )



interest_vars <- c("rats", "groupe", 
                   "cond", "bloc", "trend", 
                   "inje", "pellets", "pers", "ratio", "err", "mang" )
# ---- UI Definition ----
ui <- fluidPage(
  titlePanel(h1("Behavioral analysis", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("file", 
                  h3("Select a type of experiment"), 
                  choices = file_list,
                  selected = "",),
      
      varSelectInput("variable",
                     data = "",
                     selected = "",
                     label = h3("Select variable to analyse")),
      
      radioButtons("normalize", 
                   label = "Choose method of normalization", 
                   choices = list("None" = 1, 
                                  "On baseline" = 2,
                                  "Overall timeline" = 3),
                   selected = 1)
      
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Properties",
                           fluidRow(
                             h3("Summary"),
                             verbatimTextOutput("debug"),
                             h3("Anova"),
                             tableOutput("stats")
                           )
                  ),
                  tabPanel("Analysis",
                           h3("Wilcoxon test"),
                           dataTableOutput("analysis"))
      )
    )
  )
)


server <- function(session, input, output){
  
  # ---- Reactive functions ----
  
  df <- reactive({
    
    # Preprocessing remove unnecessary columns
    df <- read_csv(input$file) 
    names_to_remove <- setdiff(names(df),
                               interest_vars)
    df <- df %>% 
      select(-all_of(names_to_remove)) %>% 
      mutate(trend = factor(trend))
    
    # Rename columns according to string stored list : var_names
    labs <- names(df)
    labs <- map_chr(labs, function(x){
      if(x %in% var_names){
        x <- names(which(var_names == x))
      } else{
        x
      }
    })
    names(df) <- labs
    
    
    # To update possible variable choice in UI
    update_choice <- df %>% 
      select(-c("rats", "groupe", "cond", "bloc", "trend"))
    
    
    updateVarSelectInput(session, 
                         inputId = "variable", 
                         data = update_choice,
                         label = "Select your variable of interest",
                         selected = 1)
    return(df)
  })
  
  # Normalization reactive block
  data <- reactive({
    state <- input$normalize
    normdata <- df()
    normvariable <- input$variable
    
    if ( state == 2) {
      
      # Calculate average interest value for baseline
      baselineval <- normdata %>%
        group_by(rats) %>%
        filter(cond == "baseline") %>%
        summarise(bval = mean(!!normvariable))
      
      # Normalizes variable of interest with those baselines
      normdata <- normdata %>%
        left_join(baselineval) %>%
        mutate( {{ normvariable }} := !!normvariable  / bval) %>%
        select(-bval)
      
      
    }else if (state == 3) {
      normdata <-  normdata %>%
        group_by(rats) %>%
        mutate( {{ normvariable }} := scale(!!normvariable))
    } else{
      return(normdata)
    }
  })
  
  # Statistical anova analysis on data
  stat_anova <- reactive({
    form_anova <- as.formula(paste(input$variable, "~ groupe*trend + (1|rats)"))
    anova_df <- data()
    results <- lmer(form_anova, data = anova_df)
  })
  
  
  #  Statiscal analysis multiple wilcoxon test with mult.comp
  analysis <-  reactive({
    formu <- as.formula(paste(input$variable, "~ groupe"))
    data() %>% 
      group_by(trend) %>% 
      wilcox_test(formu) %>% 
      adjust_pvalue(method = "fdr") %>% 
      add_significance()
  })
  
  
  # ----- Outputs ----
  
  output$plot <- renderPlot({
    data() %>% 
      ggplot(aes(x = trend, !!input$variable , fill = groupe)) +
      geom_boxplot() + 
      ylab( names(var_names[var_names == paste(input$variable)]) ) +
      xlab("Days")
    
  })
  
  output$debug <- renderPrint({ 
    input$variable
  })
  
  output$stats <- renderTable({
    anova(stat_anova()) %>% 
      rownames_to_column() %>% 
      rename("Anova Factor" = rowname,
             p = "Pr(>F)") %>% 
      add_significance()
    
  })
  
  output$analysis <- renderDataTable({
    analysis()
  })
  
}

shinyApp(ui, server)