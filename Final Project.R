library(shiny)
library(tidyverse)
library(rsconnect)


mn_contrib <- read.csv("indivs_Minnesota18.csv")
zip_code_database <- read_csv("zip_code_database.csv")


data(county.regions, 
     package = "choroplethrMaps")

region <- county.regions %>%
  filter(state.name == "minnesota") %>%
  mutate(county = county.name)

try <- mn_contrib %>%
  mutate(Zip = as.numeric(Zip)) %>%
  left_join(zip_code_database,
            by = "Zip") %>%
  mutate(county1 = str_to_lower(county),
         county2 = str_remove(county1, pattern = " county")) %>%
  select(-acceptable_cities, -unacceptable_cities, -state, -decommissioned, -country, -world_region, -area_codes, -timezone, -type, -Microfilm, -OtherID, -Type, -Realcode, -Street, -Ultorg, -Contribid, -Cycle, -Fectransid, -Recipcode, -Source, -primary_city, -county, -county1)


region

merged <- try %>%
  left_join(region, by = c("county2" = "county"))


ui <- fluidPage(selectInput("County", 
                            "County", 
                            choices = Gender, Occupation, Contrib, Zip, 
                            multiple = TRUE, 
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot"))


server <- function(input, output) 
{output$timeplot <- renderPlot({
  if(length(input$state) > 0) {
     try %>% 
      ggplot(aes(x = , y = )) +
      geom_histogram() +
      labs(title = " ",
           y= "",  
           x = "") +
      theme_minimal()}})}



shinyApp(ui = ui, server = server)

