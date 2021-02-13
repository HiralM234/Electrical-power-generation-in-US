library(shiny)
library(ggplot2)
library(rgeos)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)
library(sp)
library(stringr)
library(plyr)
library(usmaps)

readfile <- read.csv("annual_generation_state.csv")

#required changes: toupper, factor, string-int
readfile$STATE = toupper(readfile$STATE)
readfile$STATE <- as.factor(readfile$STATE)
readfile$TYPE.OF.PRODUCER <- as.factor(readfile$TYPE.OF.PRODUCER)
readfile$ENERGY.SOURCE<- as.factor(readfile$ENERGY.SOURCE)
readfile$GENERATION..Megawatthours. <- as.numeric(factor(readfile$GENERATION..Megawatthours.))

#missing identifiers
readfile <- subset(readfile, readfile$STATE == "  ")
readfile$STATE <- droplevels(readfile$STATE)

#removes lines with negative values for generation megawatthours and saves all positive values
readfile <- subset(readfile, readfile$GENERATION..Megawatthours. >=0)

#remove lines from energy sources
readfile <- subset(readfile, readfile$ENERGY.SOURCE != "Other" & readfile$ENERGY.SOURCE != "Other Gases" &
                     readfile$ENERGY.SOURCE != "Other Biomass" & readfile$ENERGY.SOURCE != "Pumped storage")
readfile$ENERGY.SOURCE <- droplevels(readfile$ENERGY.SOURCE)

#short names
levels(readfile$ENERGY.SOURCE)[levels(readfile$ENERGY.SOURCE) == "Hydroelectric Conventional"]<- "Hydro"
levels(readfile$ENERGY.SOURCE)[levels(readfile$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"
levels(readfile$ENERGY.SOURCE)[levels(readfile$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"

#select different states
listOfStates <- as.character(unique(unlist(readfile$STATE)))
listOfStates2 <- do.call("rbind.data.frame", listOfStates)  #convert to data frame
listOfStates2$state = rownames(listOfStates)
names(listOfStates2) <- c(IL, DC, US)

#select dufferent years
years <- c(1990:2019)

# e = readfile$YEAR
# e1 = readfile$ENERGY.SOURCE
# df = data.frame(e, e1)

usa <- map_data("state")


shinyUI( fluidPage(

  #Application heading
  titlePanel( "Electrical power generation in US"),

  sidebarLayout(
    #For stacked bar graph
    # sidebarPanel(
    #   selectInput("xaxis", "Select the value for xaxis", colnames(readfile), selected ="YEAR"),
    #   selectInput("yaxis", "Select the value for yaxis", colnames(readfile), selected ="ENERGY.SOURCE"),
    #   checkboxInput("sidebar", "Create")
    # ),
    sidebarPanel(
       #selectInput("bar","Energy source per year", aes(x= YEAR, y= GENERATION..Megawatthours.) )
       checkboxInput("bar", "Create")
    ),
    sidebarPanel(
      selectInput("map1", "Select the state:", colnames(listOfStates), selected ="IL"),
      selectInput("map1", "Select the state:", colnames(listOfStates), selected ="DC"),
      selectInput("map1", "Select the state:", colnames(listOfStates), selected ="Total US")
    ),
    sidebarPanel(
      checkboxGroupInput("")
    ),

    mainPanel(
      plotOutput("bar")   #stacked bar graph

    )
  )
)
)

shinyServer(function(input, output) {

  energy <- subset(readfile, readfile$ENERGY.SOURCE != "Total" )
  #summary(readfile)
  output$bar <- renderPlot({
    #energy <- table( readfile[,input$xaxis], readfile[,input$yaxis])
    #barplot(energy, main = "stacked bar chart_energy source per year", beside = input$sidebar)
    #e1 <-table(aes( x= readfile$YEAR, y= readfile$GENERATION..Megawatthours.))
    
    ggplot(readfile ,aes( x= YEAR, y= GENERATION..Megawatthours.,fill = readfile$ENERGY.SOURCE))+
    geom_bar(stat = "identity", position = "stack") + labs(title = "Energy source per year", 
                                                           x = "Year", y="Generation Megawatthours", beside = input$bar)
  })
  
})
shinyApp(ui=ui, server=server)






