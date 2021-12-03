library(shiny)
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
    
    mainPanel(
      plotOutput(outputId = "sum_veg"),
    )
  )
)

# Define server logic 
server <- function(input, output) {
  

  # Reactive
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
  
  
## Rendering Plot 
  
     output$sum_veg <- renderPlot({
       veg_smry() %>%
         filter(date == input$date) %>%
         ggplot(aes(x = vegetable, y = sum_veg, fill = vegetable)) +
         geom_point(size = 6) +
         #scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) +
         labs(title = paste("Sum weight (grams) of root vegetables on ", input$date),
              x = "",
          y = "") +
         theme_economist() + 
         guides(fill = guide_legend(title = "VEGETABLE(S)", size = 10))
     })
  
  
   }
  
  


# Run the application 
shinyApp(ui = ui, server = server)
