
library(shiny)
library(markdown)
library(tidyverse)
library(leaflet)
library(corrplot)
library(RColorBrewer)

## Read in item dataframes
itemData <- read.csv("HospitalPricingAppData.csv")

# Define UI 
ui <- navbarPage("DSA-8001 Hospital Pricing Project",
              tabPanel("Pricing Comparison by Item",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("itemType", "Please Select Item for Comparison",
                                       levels(itemData$Item)
                          ) #end radioButtons
                        ), #end sidebarPanel
                        mainPanel(
                          h3("Hospitals with this item listed in their Chargemaster"),
                          h4("Click on Marker for Hospital Demographic Data"),
                          fluidRow(
                          leafletOutput("priceMap",height = 300)
                        ), #end fluidRow priceMap
                        h3("Item Price by Hospital"),
                        h4(textOutput("ItemMedian")),
                         fluidRow(
                           plotOutput("barPlot")
                         ), #end fluidRow barPlot
                        h3("Correlation plot of Item Price with Hospital Demographic Factors"),
                        h4("Positive correlations are displayed in blue and negative correlations in red color. 
                           Color intensity and the size of the circle are proportional to the correlation coefficients."),
                        fluidRow(
                          plotOutput("corrPlot")
                        ) #end fluidRow corrPlot
                        ) #end mainPanel
                      ) #end sidebarLayout
                      ), #end tabPanel Pricing
              tabPanel("About",
                       fluidRow(
                         column(12,
                                includeMarkdown("about.md")
                         ) #end column
                       ) #end fluidRow
              ) #end tabPanel About
  ) #end navbarPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- reactive({
    itemData %>%
    filter(Item == input$itemType)
  })
  
  output$priceMap <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'ion-medkit',
      iconColor = 'black',
      library = "ion",
      markerColor = "blue")
    
    map <- leaflet(df()) %>% 
      addTiles() %>%
      setView(lng=-91.8318, lat=38.5 , zoom=6) %>%
      addAwesomeMarkers(~long, 
                        ~lat,
                        #icon=icons,
                        popup = ~as.character(paste(HospitalName, "<br/>", 
                                                    "City:", City, "<br/>", 
                                                    "Staffed Beds:", StaffedBeds, "<br/>",
                                                    "Gross Patient Revenue:", GrossPatientRevenue, "<br/>",
                                                    "Median Income of City:", medianIncome, "<br/>",
                                                    "Percent Insured in City:", round(percentInsured)
                        )), 
                        label = ~as.character(HospitalName)
      )
    map
  }) #end renderLeaflet
  
  output$barPlot <- renderPlot({
    legend_ord <- levels(with(df(), reorder(HospitalName, -Price)))
    
    p <- ggplot(df(), aes(x=reorder(HospitalName, -Price), y=Price, fill=HospitalName)) +
      geom_bar(stat = "identity") +
      geom_hline(aes(yintercept = median(Price))) +
      labs(x="Hospital", y="Price (in US Dollars)") +
      theme(axis.text.x = element_blank()) + 
      scale_fill_discrete(breaks=legend_ord)
    
    p
  }) #end renderPlot barPlot
  
  output$corrPlot <- renderPlot({
    df_corr <-cor(df()[, c(2, 5:6, 10:13)])
    corrplot(df_corr, type="upper", order="hclust",
             col=brewer.pal(n=8, name="RdYlBu"))
  })
  
  output$ItemMedian <- renderText({
    itemMedian <- itemData %>%
      filter(Item == input$itemType) %>%
      summarise(`Median Price` = median(Price))
    
    paste0("The median price of ", input$itemType, " for the displayed hospitals is $", itemMedian)
  })
  
} #end server function

# Run the application 
shinyApp(ui = ui, server = server)

