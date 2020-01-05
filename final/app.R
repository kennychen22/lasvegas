library(shinydashboard)
library(leaflet)
library(wordcloud)
library(tm)
library(ggplot2)
library(dplyr)
library(ggwordcloud)
library(leafpop)
library(mdsr)
library(tidytext)
library(stringr)
library(DT)

set.seed(07132000)
bing <- get_sentiments("bing")
#reading file
matches_reviews_fixed <- readRDS("matches_reviews_fixed.Rds")
#getting latitude and longitude because once you collapse, it only has two columns (business id and text)
latLong <- matches_reviews_fixed %>%
  distinct(latitude.x, longitude.x, business_id, Inspection_Grade, stars.x,categories.x,name.y, address.y,Inspection_Date,postal_code.x,
           Inspection_Demerits) 
#collapsing texts grouped by restaurant
collapse <- matches_reviews_fixed %>%
    group_by(business_id) %>%
    summarize(text=paste(text, collapse =" // ")) %>%
    ungroup()
#joining to get back coordinates tp map
collapseNew <- collapse %>% left_join(latLong, by = "business_id")
#making names all uppercase
collapseNew<- collapseNew %>%
  mutate(name.y=toupper(name.y))

ui <- dashboardPage(
    dashboardHeader(title = "Las Vegas Restaurants"),
    dashboardSidebar(
      #choosing category of restaurant
        selectInput("categories1","Categories", choices = c("All", "American", "Mexican", "Japanese", "Chinese", "Desserts", 
                                                           "Vietnamese", "Hawaiian", "Italian","Korean", "Mediterranean",
                                                           "Latin American","Thai", "Indian", "Middle Eastern", "French")),
        
        selectInput("categories2","Categories", choices = c("All","Cafes", "Restaurants", "Bars")),
        
        checkboxInput("categories3", "Vegan?"),
        
        hr(),
        #choosing sanitation grade
        checkboxGroupInput("sanitationgrade",
                           h4("Show Restaurants with Sanitation Grades: "),
                           choices = list("A"="A",
                                          "B"="B",
                                          "C"="C"),
                           selected = list("A","B","C")
                           
                           ),
        #typing in zipcode
       textInput("zip","Zipcode",value="")
        
    ),
    dashboardBody(
  
       #maps, reviews, and inspection data
        fluidRow(
            box(leafletOutput("plot1", height = 350),width="100%")),
        
            fluidRow(
            box(plotOutput("plot2",height= 450, width = "100%")),
            box(plotOutput("plot3", height = 450, width = "100%"))
            ),
        fluidRow(
                 infoBoxOutput("starsBox",width=4),
                 infoBoxOutput("gradeBox",width=4),
                 infoBoxOutput("demeritsBox",width=4),

        
    ),
        fluidRow(
          box(DT::dataTableOutput(outputId = "reviewstable"),width=12)
        ),
)
)


server <- function(input, output, session) {
    # creating leaflet plot
    output$plot1 <- renderLeaflet({
        
        
        if (input$categories1 != "All"){
          collapseNew <- collapseNew%>%
            filter(str_detect(categories.x,input$categories1))
        }
        
       
          if (input$categories2 != "All"){
            collapseNew<- collapseNew %>%
            filter(str_detect(categories.x,input$categories2))
          }
        
        
         if(input$categories3){
           collapseNew <- collapseNew %>%
             filter(str_detect(categories.x,"Vegan"))
         }
      

          collapseNew <- collapseNew %>%
            filter(Inspection_Grade %in% input$sanitationgrade  )
          
          if(!input$zip==""){
          collapseNew <- collapseNew %>%
            filter(postal_code.x == input$zip)
          }
      
      #making popup
        content <- paste(sep = "<br/>",
                         "<b>",collapseNew$name.y,"</b>",
                         collapseNew$address.y, " ", paste0("Last Inspected: ",collapseNew$Inspection_Date))
                         
        
        leaflet()%>%
            addTiles()%>%
            addCircleMarkers(lng=~longitude.x,lat=~latitude.x, layerId = collapseNew$business_id, data=collapseNew,
                             popup= content, color=ifelse(collapseNew$Inspection_Grade=="A", "blue",
                                                          ifelse(collapseNew$Inspection_Grade=="B","green","red")))
  
    })
    
    #function to get top 3 pos and neg words
    goodBad <- function(text) {
    words <- tibble(text) %>%
        unnest_tokens(word, text)
    positive <- words %>%
        count(word, sort = T) %>%
        inner_join(bing) %>%
        filter(sentiment == "positive") %>%
        head(3)
    negative <- words %>%
        count(word, sort = T) %>%
        inner_join(bing) %>%
        filter(sentiment == "negative") %>%
        head(3)
    c <- rbind(positive, negative)
    return(as.data.frame(c))
    }
    # word cloud function
    wordCloud <- function(source){
        Corpus <- VCorpus(VectorSource(source$text))
        Corpus <- Corpus %>%
            tm_map(stripWhitespace) %>%
            tm_map(removeNumbers) %>%
            tm_map(removePunctuation) %>%
            tm_map(content_transformer(tolower)) %>%
            tm_map(removeWords, stopwords("english"))
        Corpus
        
    
        
    }
    
  
    
    #clicking observation
    observe({
        click <- input$plot1_marker_click
        if (is.null(click))
            return()
        
        
        proxy <- leafletProxy("plot1")
       
        ## gets the text for one business
        sentiment <- collapseNew %>%
            filter(business_id == click$id) 
        #tokennizes the reviews for one business
        sentimentwords <- sentiment %>%
            unnest_tokens(word, text) %>%
            group_by(word) %>%
            summarize(count = n()) %>%
            arrange(desc(count)) 
        
        # get the corpus of one businesses' reviews
        words <-  wordCloud(sentiment)
        #chooses 5 reviews from one restaurant
        reviewsforbox <- matches_reviews_fixed %>%
          filter(business_id == click$id)%>%
          select(text)%>%
          head(5)
        #top3plot
        output$plot2<-renderPlot({
            c <- goodBad(sentiment$text)
            ggplot(c ,aes(x= reorder(word,n), y = n, fill = sentiment)) + geom_bar(stat = "identity")+coord_flip()+
              ggtitle("Top 3 Negative and Positive Words")+xlab("Number of Words")+ylab("Words")
        },height=450,width=450)
            #wordcloud plot
            output$plot3 <- renderPlot({
                wordcloud(words, max.words = 30, scale = c(8, .25),
                          colors = c("cornflowerblue","darkorange"))
            },width=650,height=450)
            

        #inspectiongrade
    output$gradeBox <- renderInfoBox({
        infoBox(
            "Sanitation Grade", sentiment$Inspection_Grade, icon = icon("thumbs-up", lib = "glyphicon"),
            color = "blue"
        )
    })
    #ratings
    output$starsBox <- renderInfoBox({
        infoBox(
            "Stars", sentiment$stars.x, icon = icon("star", lib = "glyphicon"),
            color = "yellow"
        )
    })
    #inspection demerits
    output$demeritsBox <- renderInfoBox({
      infoBox(
        "Inspection Demerits", sentiment$Inspection_Demerits, icon = icon("warning-sign", lib = "glyphicon"),
        color = "red"
      )
    })
    #reviews table
    output$reviewstable <- renderDataTable({
      (data=reviewsforbox)

    })
    
    
    })
    
}

shinyApp(ui, server)