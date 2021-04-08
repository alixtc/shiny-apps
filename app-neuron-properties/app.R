library(shiny)
library(rstatix)
library(tidyverse)
library(plotly)

# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer

df <- readxl::read_xlsx("Perforated Q175 AP waveform analysis.xlsx")


# Calculate 1st order derivative centered on point with paddind on both sides
derivative <- function(x){
  Deri <- diff(x, lag  = 2, differences = 1) / 2
  Deri <- c(NA, Deri, NA) / 0.04
}


# function to calculte curvature from 1st, 2nd and 3rd order derivatives
curvature <- function(x){
  D1 <- derivative(x)
  
  D2 <- derivative(D1)
  
  D3 <- derivative(D2)
  
  res <- ( D3 * D1 - D2 ^ 2 )/ ( D1 ^ 3 )
  
}

# Select neurons to analyse
dff <- df %>% 
  select(contains("Events")) %>% 
  slice(2:nrow(df)) %>% 
  mutate_if(is.character, as.numeric)


dff[,ncol(dff)+1] <- 1:nrow(dff)
names(dff)[ncol(dff)] <- "time"

# Calculates curvature and Peak time for each neuron
dff <- dff %>% 
  pivot_longer( cols = !time,
                names_to = "neuron", 
                values_to = "voltage") %>% 
  arrange(neuron) %>% 
  filter(!is.na(voltage)) %>%
  group_by(neuron) %>%
  mutate( curve = curvature(voltage),
          curve = curve * (curve >= 0),
          time_maxV = time[voltage == max(voltage)])

#  Keep curvatures data for the 25  points before peak and  extract threshold from it
thresholds <- dff %>% 
  mutate(curve = curve * (time > time_maxV - 25 & time <= time_maxV)) %>%
  group_by(neuron) %>% 
  filter(!is.na(curve)) %>% 
  summarise(threshold = voltage[curve == max(curve)])

dff <- dff %>% 
  left_join(thresholds) %>% 
  group_by(neuron) %>%
  mutate(timing = ifelse(voltage == max(threshold), time, NA ))

# Put all the names of the neurons in a list to select them individually in the app
vnames <- unique(dff$neuron)
vnames <- `names<-`(as.list(vnames), vnames)

# Define UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"), # Set theme for app
  titlePanel(h1("Neuronal property analysis", align = "center")),
  
  sidebarLayout(
    sidebarPanel(selectInput("file", 
                             h3("Select a neuron"), 
                             choices = vnames,
                             selected = 1)
    ),
    
    mainPanel(
      navbarPage(" ",
                 tabPanel("Plot",
                          h4("Waveform"),
                          plotOutput("plot"),
                          h4("Phase plot"),
                          plotlyOutput("phaseplot", height = 600)
                 ),
                 tabPanel("Properties", 
                          h3("Summary"),
                          tableOutput("properties"))
      )
    )
  )
)


# Retreat some data for output


# Define server logic ----
server <- function(input, output) {
  
  thematic::thematic_shiny() # adapt plot theme to current theme
  
  newdata <- reactive({
    data <- dff %>% filter(neuron == input$file) 
    
    t_peak <- data %>%  
      distinct(time_maxV) %>% 
      pull(time_maxV)
    
    window <- c(t_peak - 30, t_peak + 30)
    
    newdata <- data %>% 
      slice(window[1] : window[2]) %>% 
      summarise(Peak = max(voltage),
                Antipeak = min(voltage),
                Amplitude = Peak - Antipeak,
                Distance_P_AP_in_ms = time[which(voltage == min(voltage))] - t_peak)
  })
  
  output$properties = renderTable({
    newdata()
  }) 
  
  
  output$plot = renderPlot({
    
    dff %>% 
      filter(neuron == input$file) %>% 
      ggplot(aes(x = time, y = voltage)) +
      geom_line() +
      geom_point(data = . %>% filter(!is.na(timing)), # Removes NA values from plot
                 aes(timing, threshold), # Plot the threshold value
                 size = 3,
                 alpha = 0.3,
                 color = "red") 
  })
  
  output$phaseplot = renderPlotly({
    
    dff %>% 
      filter(neuron == input$file) %>% 
      mutate(dV = derivative(voltage)) %>% 
      ggplot(aes(x = voltage, y = dV)) +
      geom_path() + geom_point(alpha = 0.3) +
      geom_point(data = . %>% filter(!is.na(timing)), 
                 aes(x = threshold, y = dV, color = "red")) +
      theme(legend.position = "none")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)