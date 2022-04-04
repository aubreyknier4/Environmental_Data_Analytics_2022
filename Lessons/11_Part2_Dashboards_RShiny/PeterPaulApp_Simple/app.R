#### Load packages ----
library(shiny)
library(tidyverse)

#### Load data ----
nutrient_data <- read_csv("Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
ui <- fluidPage(
  titlePanel("Nutrients in Peter Lake and Paul Lake"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "dropdown_input", 
                  label = "Nutrient element",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tn_ug"),
  
      ),

    # Output
    mainPanel(
      plotOutput("scatterplotty")
    )))

#### Define server  ----
server <- function(input, output) {
     
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplotty <- renderPlot({
        ggplot(nutrient_data, 
               aes_string(x = "sampledate", y = input$dropdown_input, 
                          fill = "depth_id", shape = "lakename")) +
          geom_point(alpha = 0.6, size = 6) +
          theme_classic(base_size = 16) +
          scale_shape_manual(values = c(22, 24)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          scale_fill_distiller(palette = "BuPu", guide = "colorbar", direction = 1)
          #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

#### Questions for coding challenge ----
#1. Play with changing the options on the sidebar. 
# Choose a shinytheme that you like. The default here is "yeti"
# How do you change the default settings? 
# How does each type of widget differ in its code and how it references the dataframe?
#2. How is the mainPanel component of the UI structured? 
# How does the output appear based on this code?
#3. Explore the reactive formatting within the server.
# Which variables need to have reactive formatting? 
# How does this relate to selecting rows vs. columns from the original data frame?
#4. Analyze the similarities and differences between ggplot code for a rendered vs. static plot.
# Why are the aesthetics for x, y, fill, and shape formatted the way they are?
# Note: the data frame has a "()" after it. This is necessary for reactive formatting.
# Adjust the aesthetics, playing with different shapes, colors, fills, sizes, transparencies, etc.
#5. Analyze the code used for the renderTable function. 
# Notice where each bit of code comes from in the UI and server. 
# Note: renderTable doesn't work well with dates. "sampledate" appears as # of days since 1970.



