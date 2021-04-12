library(bslib)
library(shiny)
library(lme4)
library(lmerTest)
library(rstatix)
library(tidyverse)

# ---- Preprocessing file information ----


file_list <- grep("csv", list.files(), value = TRUE)
file_list <- as.list(file_list)
names(file_list) <- c("Drug test inhibition",
                      "Drug test stimulation",
                      "Food test inhibition",
                      "Food test stimulation")

# Stores in a list: list$new_name <- "old_names"
var_names <- list(Injections = "inje",
                  Pellets = "pellets",
                  Perseveratives = "pers",
                  # ratio = "ratio",
                  Errors = "err",
                  `Food magazine responses` = "mang" )



interest_vars <- c("rats", "groupe", 
                   "cond", "bloc", "trend", 
                   "inje", "pellets", "pers", 
                   # "ratio", 
                   "err", "mang" )
# ---- UI Definition ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "simplex"),
  titlePanel(h1("Behavioral analysis", align = "center")),
  p("Automated statistical analysis based on the selection of an 
    experiment and one of the reacorded variables.", align = "center"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file", 
                  h3("Select a type of experiment"), 
                  choices = file_list,
                  selected = file_list[1],),
      
      varSelectInput("variable",
                     label = h3("Select variable to analyse"),
                     data = "",
                     selected = ""),
      
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
                  tabPanel("Statisical Test",
                           fluidRow(
                             h3("Summary Anova"),
                             p("The statistical test on this page and the detailed analysis are 
                               updated based on the choices on the left side bar"),
                             tableOutput("stats")
                           )
                  ),
                  tabPanel("Detailed Analysis",
                           h3("Wilcoxon test"),
                           dataTableOutput("analysis"))
      )
    )
  )
)


server <- function(session, input, output){
  thematic::thematic_shiny()
  # ---- Reactive functions ----
  
  df <- reactive({
    
    # Preprocessing remove unnecessary columns
    df <- read_csv(input$file) 
    names_to_remove <- setdiff(names(df),
                               interest_vars)
    
    # Format data
    df <- df %>% 
      select(-all_of(names_to_remove)) %>% 
      rename("days" = "trend",
             "subjects" = "rats") %>% 
      mutate(days = factor(days)) %>% 
      mutate(groupe = case_when(groupe == "eyfp"~ "control",
                                  groupe == "stim"~ "stimulation",
                                  groupe == "inhi"~ "inhibition")) %>% 
      mutate(cond = case_when(cond == "laser"~ "test",
                               TRUE~ cond)) %>%
      mutate(cond = factor(cond, 
                           levels = c("baseline", "test", "off", "challenge"),
                           labels = c("baseline", "test", "off", "challenge")))
    
    
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
      select(-c("subjects", "groupe", "cond", "bloc", "days")) 
    
    
    updateVarSelectInput(session, 
                         inputId = "variable", 
                         data = update_choice,
                         label = "Select your variable of interest",
                         selected = names(update_choice)[[1]])
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
        group_by(subjects) %>%
        filter(cond == "baseline") %>%
        summarise(bval = mean(!!normvariable))
      
      # Normalizes variable of interest with those baselines
      normdata <- normdata %>%
        left_join(baselineval) %>%
        mutate( {{ normvariable }} := !!normvariable  / bval) %>%
        select(-bval)
      
      
    }else if (state == 3) {
      normdata <-  normdata %>%
        group_by(subjects) %>%
        mutate( {{ normvariable }} := scale(!!normvariable))
    } else{
      return(normdata)
    }
  })
  
  # Statistical anova analysis on data
  stat_anova <- reactive({
    form_anova <- as.formula(paste(input$variable, "~ groupe*days + (1|subjects)"))
    anova_df <- data()
    results <- lmer(form_anova, data = anova_df)
  })
  
  
  #  Statiscal analysis multiple wilcoxon test with mult.comp
  analysis <-  reactive({
    formu <- as.formula(paste(input$variable, "~ groupe"))
    data() %>% 
      group_by(days) %>% 
      wilcox_test(formu) %>% 
      adjust_pvalue(method = "fdr") %>% 
      add_significance()
  })
  
  
  # ----- Outputs ----
  
  output$plot <- renderPlot({
    data() %>% 
      ggplot(aes(x = days, !!input$variable , fill = groupe)) +
      geom_boxplot() + 
      scale_fill_manual(values = c("grey", "red")) +
      facet_grid(~cond, space="free_x", scales="free_x", switch="x") +
      ylab(input$variable ) +
      xlab("Days") +
      theme(panel.spacing = unit(0.1, "cm"))
    
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