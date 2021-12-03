library(shiny)
# If you need to install my gardenR library
# library(devtools)
# devtools::install_github("llendway/gardenR")
library(gardenR) # yay, my garden data again!
library(tidyverse)
library(DT) # for table output
library(ggthemes)
library(bslib)

veggies <- garden_harvest %>% 
  distinct(vegetable) %>% 
  arrange(vegetable) %>% 
  pull(vegetable)

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  # Application title
  titlePanel("Root Veggies Harvested From Lisa's Garden"),
  
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      #this is for box/bar/point plot
       dateInput(inputId = "date", 
                 label = "Date of Harvest", 
                 value = "2020-06-06"
                 ),
    ),
    
    # Show a plot of cumulative weight for chosen vegetable
    # Show a table beneath
    mainPanel(
      plotOutput(outputId = "sum_veg"),
      #dataTableOutput(outputId = "sum_veg_tbl")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  

  # Enclose in reactive() - makes a function
  veg_smry <- reactive(garden_harvest %>%
                       filter(vegetable %in% c("beets",
                                                "carrots",
                                                "potatoes", 
                                                "radish", 
                                                 "onions", 
                                                 "rutabaga")) %>%
                         mutate(vegetable = str_to_title(vegetable)) %>%
                         group_by(date, vegetable) %>%
                         summarize(sum_veg = sum(weight)) %>%
                         mutate(cum_wt = cumsum(sum_veg))
  )
  
  
## THIS IS FOR BOXPLOT?BAR?POINTPLOT
     # Now use that function, with no arguments.
     output$sum_veg <- renderPlot({
       veg_smry() %>%
         filter(date == input$date) %>%
         ggplot(aes(x = vegetable, y = sum_veg, fill = vegetable)) +
         geom_point(size = 6) +
         #scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) +
         labs(title = paste("Sum weight (gram) of root vegetables on ", input$date),
              x = "",
          y = "") +
         theme_economist() + 
         guides(fill = guide_legend(title = "VEGETABLE(S)", size = 10))
     })
  
  
   }
  
  


# Run the application 
shinyApp(ui = ui, server = server)
