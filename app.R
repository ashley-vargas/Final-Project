

library(shiny)
library(tidyverse)
library(rsconnect)
library(shinythemes)
library(maps)




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

ui<-fluidPage(
  titlePanel("Minnesota Political Donations"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             selectInput(inputId = "userchoice1", 
                                         label = "Input Gender Here", 
                                         choices = c(Female = "F", Male = "M"), 
                                         multiple = FALSE),
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
                             submitButton(text = "Create my plot!")),
                mainPanel("main panel",
                            verticalLayout(plotOutput("timeplot"), 
                                        plotOutput("mapping",width="400px",height="300px")))))





server <- function(input, output){
  output$timeplot <- renderPlot({
    main %>% 
      filter(Amount > 0) %>%
      filter(Gender %in%  input$userchoice1, county == input$userchoice2) %>% 
      group_by(county) %>%
      mutate(avg = mean(Amount)) %>%
      ungroup() %>% 
      ggplot(aes(x = Amount, fill=county)) +
      geom_histogram() +
      facet_wrap(~county, scales="free_y") +
      geom_vline(aes(xintercept=avg),
                 color="royalblue1", linetype="dashed", size=1) +
      scale_x_log10(labels = scales::comma) +
      scale_color_brewer(palette="Accent") +
      labs(title = "Minnesota Political Donations by County and Sex",
           x = "",
           y = "") +
      theme_minimal()})
  output$mapping <- renderPlot({
    main %>%
      group_by(county) %>%
      mutate(total_contribs = n()) %>%
      filter(total_contribs > 25) %>%
      summarize(mean_amt = mean(Amount)) %>%
      ggplot() + 
      geom_map(map = mn_county, aes(map_id = county, fill = mean_amt)) +
      labs(x="long",y="lat",title = "Mean amount of donations for each county")+
      expand_limits(x = mn_county$long, y = mn_county$lat)})
}




shinyApp(ui = ui, server = server)







