library(shiny)
library(tidyverse)
library(rsconnect)
library(shinythemes)



mn_contrib <- read_csv("indivs_Minnesota18.csv")
zip_codes <- read_csv("zip_code_database.csv")



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


ui <- fluidPage(theme = shinytheme("cerulean"),
                selectInput(inputId = "userchoice2", 
                            "Input County Here", 
                            choices = list("ramsey","hennepin","houston","anoka","winona","renville","st louis",
                                           "sherburne","brown","itasca","scott","dakota","washington","olmsted","wright",
                                           "rice","goodhue","kandiyohi","le sueur","mcleod","carlton","becker","blue earth",
                                           "benton","carver","mille lacs","clay","cook","otter tail","big stone",
                                           "chisago","stearns","mower","pine","hubbard","todd","crow wing","meeker",
                                           "polk","nicollet","aitkin","wadena","faribault","pierce","isanti","fillmore",
                                           "lake","beltrami","cass","st croix","martin","douglas","stevens","morrison",
                                           "watonwan","cottonwood","swift","clearwater","lyon","sibley","steele","wabasha",
                                           "freeborn","murray","wilkin","traverse","marshall","norman","koochiching",
                                           "chippewa","lac qui parle","grant","yellow medicine","roseau","pennington",
                                           "red lake","dodge","pope","redwood","kanabec","pipestone","erie","lake of the woods",
                                           "lincoln","nobles","jackson","santa fe","st louis city","waseca","madison",
                                           "dupage","washtenaw","rock","marin","kittson","worcester","portage","tippecanoe",
                                           "clark","milwaukee","harris","fayette","osage","macomb","taos","wayne","carbon",
                                           "rankin","nassau","burnett","chaves","hamilton","bernalillo","isabella","ingham",
                                           "lancaster","marquette","denver","clare","panola","bay","richland","traill",
                                           "pembina","avoyelles parish","wake","johnson","district of columbia","montgomery",
                                           "san juan","missoula","woodbury","fairfax","cuyahoga","berrien","los alamos",
                                           "boone","fulton","prince george's","lewis and clark","santa barbara","king",
                                           "antrim","oktibbeha","santa clara","marathon","mahoning","anne arundel","ada",
                                           "lee","gallatin","burleigh","navajo","midland"),
                            selected=list("ramsey","hennepin"),
                            multiple = TRUE), 
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot"))


server <- function(input, output){
  output$timeplot <- renderPlot({
    main %>% 
      filter(Amount > 0) %>% 
      filter(Gender %in% c("M", "F"), county %in% input$userchoice2) %>%
      group_by(county, Gender) %>%
      mutate(avg = mean(Amount)) %>%
      ungroup() %>%
      ggplot(aes(x = Amount)) +
      geom_histogram(color = "black", data=subset(main, Gender  %in% "M" & county  %in% input$userchoice2),  aes(fill = "M"), alpha = 0.3) +
      geom_histogram(color = "black", data=subset(main, Gender  %in% "F" & county  %in% input$userchoice2),  aes(fill = "F"), alpha = 0.3) +
      facet_wrap(~county, scales="free_y") +
      geom_vline(aes(xintercept= avg),
                 color= "red", linetype="dashed", size=1) +
      scale_x_log10(breaks = scales::log_breaks(n=10), labels = scales::comma_format(accuracy = 1)) +
      scale_fill_manual(name="Sex",
                        values=c("M"="red", "F"="blue")) +
      labs(title = "Minnesota Political Donations by County and Sex",
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
            strip.text = element_text(size=15), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())})
}



shinyApp(ui = ui, server = server)














