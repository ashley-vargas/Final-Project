library(shiny)
library(tidyverse)
library(rsconnect)




mn_contrib <- read_csv("~/Desktop/Stat112/indivs_Minnesota18.csv")
zip_codes <- read_csv("~/Desktop/Stat112/zip_code_database.csv")


states <- map_data("state")
mn_df <- subset(states, region == "minnesota")
counties <- map_data("county")
mn_county <- filter(counties, region == "minnesota") %>%
  select(-region) %>%
  mutate(region = subregion)


main <- mn_contrib %>%
  mutate(Zip = as.numeric(Zip)) %>%
  left_join(zip_codes,
            by = "Zip") %>%
  mutate(county1 = str_to_lower(county),
         county2 = str_remove(county1, pattern = " county"),
         county2 = str_remove(county2, pattern = "\\.")) %>%
  select(-acceptable_cities, 
         -unacceptable_cities, 
         -state, 
         -decommissioned, 
         -country, 
         -world_region, 
         -area_codes, 
         -timezone, 
         -type, 
         -Microfilm, 
         -OtherID, 
         -Type, 
         -Realcode, 
         -Street, 
         -Ultorg, 
         -Contribid, 
         -Cycle, 
         -Recipcode, 
         -Source, 
         -primary_city, 
         -county, 
         -county1) %>%
  mutate(county = county2) %>%
  select(-county2) 



ui <- fluidPage(selectInput(inputId = "userchoice1", 
                            label = "Input Gender Here", 
                            choices = c("F", "M", "U", "N"),
                            multiple = FALSE), 
                numericInput(inputId = "userchoice2", 
                            label = "Input Zip Code Here", 
                            value = 55105), 
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot"))


server <- function(input, output){
  output$timeplot <- renderPlot({
    mn_contrib %>% 
      filter(Amount > 0) %>% 
      filter(Gender == input$userchoice1, Zip == input$userchoice2) %>% 
      ggplot(aes(Amount)) +
      geom_density(binwidth = 10) +
      theme_minimal()})
    }



shinyApp(ui = ui, server = server)

