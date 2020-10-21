library(shiny)
library(tidyverse)
library(rsconnect)
library(shinythemes)
library(maps)
library(plotly)


mn_contrib <- read_csv("indivs_Minnesota18.csv")
zip_codes <- read_csv("zip_code_database.csv")
committees <- read_csv("fecinfo.csv")
candidates <- read.csv("candidates.csv")



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
  select(-county2) %>%
  left_join(committees, by = "CmteId") %>%
  left_join(candidates, by =  c("cm_cand_id" = "cand_id")) %>%
  select(-Fectransid, 
         -Recipid, 
         -CmteId, 
         -cm_treasurer_name, 
         -cm_address_line_1, 
         -cm_address_line_2, 
         -cm_city, 
         -cm_state, 
         -cm_zip, 
         -cm_desig, 
         -cm_type, 
         -cm_freq, 
         -cm_interest, 
         -cm_cand_id, 
         -cand_election_year, 
         -cand_status, 
         -cand_cmte, 
         -cand_st1, 
         -cand_st2, 
         -cand_zip,
         -cand_state2) 

counties <- main  %>%
  select(county) %>%
  distinct(county)

ui<-fluidPage(theme = shinytheme("cerulean"),
  titlePanel("2018 Minnesota Political Donations"),
  sidebarLayout(position = "left",
                sidebarPanel("",
                             selectInput(inputId = "userchoice2", 
                                         "Input County Here", 
                                         choices = counties,
                                         selected=list("ramsey","hennepin"),
                                         multiple = TRUE), 
                             submitButton(text = "Create my plot!")),
                mainPanel("MN Political Donations by Sex",
                          verticalLayout(plotlyOutput("mapping",width="870px",height="400px"),
                                         plotOutput("timeplot")))))





server <- function(input, output){
  output$mapping <- renderPlotly({
    print(
      ggplotly(
        main %>%
        filter(Gender == c("M", "F")) %>%
        group_by(county, Gender) %>%
        mutate(total_contribs = n()) %>%
        filter(total_contribs > 25) %>%
        summarize(mean_amt = mean(Amount)) %>%
        ggplot() + 
        geom_map(map = mn_county, aes(map_id = county, fill = mean_amt)) +
        expand_limits(x = mn_county$long, y = mn_county$lat) + 
        scale_fill_gradient(low = "navyblue", high = "red") + 
        theme(
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank()
        ) + 
        facet_wrap(~Gender)
        ))})
  output$timeplot <- renderPlot({
    main %>% 
      filter(Amount > 0) %>%
      filter(county == input$userchoice2) %>% 
      filter(Gender == "M" | Gender == "F") %>%
      group_by(county, Gender) %>%
      mutate(avg = mean(Amount)) %>%
      ungroup() %>% 
      ggplot(aes(x = Amount, fill = county)) +
      geom_histogram(color = "white") +
      facet_grid(county ~ Gender, scales="free_y") +
      geom_vline(aes(xintercept = avg),
                 color="black", linetype="dashed", size=1) +
      geom_text(aes(avg, 0,label = round(avg), hjust = -1)) + 
      scale_x_log10(labels = scales::comma) +
      scale_fill_brewer(palette="Dark2") +
      labs(title = "Minnesota Political Donations by County and Sex",
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
            strip.text = element_text(size=15), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())})}



shinyApp(ui = ui, server = server)





