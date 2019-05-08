library(tidyverse)
library(leaflet)
library(sf)

hospitals <- read_csv("rankings.csv")
hospitals$US_News_and_World_Report_State_Ranking<- as.factor(gsub("0", "Not ranked", hospitals$US_News_and_World_Report_State_Ranking))                                                                                                                                            
zip_centroids <- read_csv("zip_centroids.csv")
ui <- fluidPage(titlePanel("Amulet Health"), 
                h2("To guide your healthcare journey"), 
                sidebarPanel(sliderInput("score", "Amulet score:", min = 0, max = 5, value = c(1,5)), 
                             textInput("zip_code", "Zip Code" , "60613"),
                             actionButton("update", "Generate map")),
                mainPanel(leafletOutput("myMap")))

server <- function(input,output){
    library(tidyverse)
    library(leaflet)
    
    pal <- colorNumeric("RdYlGn", c(0,5))
    
    target_zip <- eventReactive(input$update, {input$zip_code})
    
    output$myMap <- renderLeaflet({
        leaflet(hospitals %>% 
                    filter(between(Total_composite_score, input$score[1], input$score[2]))) %>% 
            setView(zip_centroids$LNG[zip_centroids$ZIP == target_zip()],
                    zip_centroids$LAT[zip_centroids$ZIP == target_zip()], 
                    zoom = 13) %>%
            addTiles() %>% 
            addCircleMarkers(lat = ~Lat, lng = ~Long,
                             popup = ~paste0("<b><a href = 'https://www.basketball-reference.com'>",Hospital_Name, "</a></b><br>Amulet score: ", Total_composite_score, "</b><br>Hospital Compare Star Rating: ", Hospital_Compare_Star_Rating, "</b><br>Leapfrog Safety Grade: ", Leapfrog_Safety_Grade, "</b><br>US News and World Report Illinois Ranking: ", US_News_and_World_Report_State_Ranking, "</b><br>Magent Nursing designation: ", Magnet_Designation),
                             color = ~pal(Total_composite_score),
                             opacity = 1) %>%
            addLegend("bottomright", pal = pal, values = ~Total_composite_score,
                      title = "Amulet Score",
                      opacity = 1)
    })
}
shinyApp(ui=ui, server=server)