

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(kableExtra)
library(tidytext)
library(wordcloud)


google <- read_csv('non-repeat-apps.csv')
reviews <- read_csv('googleplaystore_user_reviews.csv')
categories <- readRDS("categories.rds")

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Google Apps"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Shiny App Description",tabName = "shiny",icon = icon("chalkboard-teacher")),
                        menuItem("Data Overview", icon = icon("google-play"),
                          menuSubItem("Categories",tabName = "cats",icon = icon("table")),
                          menuSubItem("Reviews/Ratings",tabName = "rr",icon = icon("star")),
                          menuSubItem("Games",tabName = "games",icon = icon("gamepad")),
                          menuSubItem("Reviews Distribution",tabName = "review",icon = icon("chart-line"))),
                        menuItem("Individual App Information", tabName = "apps", icon = icon("podcast"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        ## language bar chart panel
                        tabItem(
                          tabName = "shiny", 
                          fluidRow(
                            box(
                              title = "Overview",
                              "Use this app to explore data on about 10,000 Google Play Apps. The tabs on the sidebar will allow you to view different representations of the data as a whole. You can also get quick information on a specific app.",
                              width = 12
                            )
                          ),
                          fluidRow(
                            box(title = "Categories Tab",
                                "Explore which categories lead in various metrics."),
                            box(title = "Reviews/Ratings Tab",
                                "Select a category to see it's top ten reviewed apps.")
                          ),
                          fluidRow(
                            box(title = "Games Tab",
                                "View most popular game categories."),
                            box(title = "Individual Apps Tab",
                                "Search for an App to view basic statistics and a word cloud from word frequencies in review.",
                                "**Disclaimer: Word cloud will only be produced for apps that start with a letter from A-H because of data restrictions.")
                          )
                        ),
                        tabItem(
                          tabName = "cats",
                          fluidRow(
                            box(selectInput("plottype","Plot:",c("Apps","Installs","Reviews"),"Apps"))
                                ),
                          fluidRow(
                            plotOutput("plot2")
                                  )
                              ),
                        ## intro part
                        tabItem(
                          tabName = "apps",
                          fluidRow(
                            valueBoxOutput("vbox"),
                            valueBoxOutput("vbox2"),
                            valueBoxOutput("vbox3")),
                          fluidRow(
                            box(
                              title = "Search/Select App for Information",
                              selectInput("app", "App:", google$App, "Facebook")
                            )
                          ),
                          fluidRow(
                            plotOutput("plot1")
                          )
                        ),
                        ## language bar chart panel
                        tabItem(
                          tabName = "rr", 
                          fluidRow(
                            box(
                              title = "Select Category to See Most Reviewed Apps",
                              selectInput("cat","Category:",google$Category,"GAME")
                            )
                          ),
                          fluidRow(
                            tableOutput('table1')
                          )
                        ),
                        tabItem(
                          tabName = "games",
                          fluidRow(
                            plotOutput("plot3")
                          )
                        ),
                        tabItem(
                          tabName = "review",
                          fluidRow(
                            box(sliderInput("slider1","Min",0,10000000,0)),
                            box(sliderInput("slider2","Max",1,10000000,1000))
                          ),
                          fluidRow(plotOutput("plot4"))
                        )
                      )
                    )
                        
                        
)


server = function(input, output){
  
  output$vbox <- renderValueBox({
    individual <- google %>% filter(App == input$app)
    valueBox(
      "Reviews",
      individual$Reviews
      )
  })
  output$vbox2 <- renderValueBox({
    individual <- google %>% filter(App == input$app)
    valueBox(
      "Rating",
      individual$Rating
    )
  })
  output$vbox3 <- renderValueBox({
    individual <- google %>% filter(App == input$app)
    valueBox(
      "Installs",
      individual$Installs
    )
  })
  
  output$plot1 <- renderPlot({
    app_reviews <- reviews %>% filter(App == input$app)
    app_reviews <-  app_reviews %>% unnest_tokens(word, Translated_Review)
    app_reviews <- app_reviews %>% anti_join(stop_words) %>% filter(word != 'nan')
    d <- app_reviews %>% count(word, sort = TRUE)
    
    wordcloud(words = d$word, freq = d$n, min.freq = 1,
              max.words=100, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$plot2 <- renderPlot({
    colored2 <- ifelse(categories$Installs > 5000000000, "red", "black")
    colored <- ifelse(categories$Total > 200000000, "red", "black")
    if(input$plottype == 'Apps')
        ggplot(google)+
          geom_bar(aes(Category, fill = Type))+
          theme(axis.text.x = element_text(angle = 75, hjust = 1))+
          labs(title = "Number of Apps by Category", x = "Category", y = "Number of Apps")
    else if(input$plottype == 'Reviews')
        ggplot(categories)+
          geom_col(aes(Category,Total/1000000,fill=ifelse(Total > 200000000,"A", "B")))+
          scale_fill_manual(guide=FALSE, values=c("red", "black"))+
          theme(axis.text.x = element_text(color = colored, angle = 75, hjust = 1))+
          labs(title = "Total Reviews by Category", x = "App Category", y = "Total Reviews")
    else 
        ggplot(categories)+
          geom_col(aes(Category,Installs/1000000,fill=ifelse(Installs > 5000000000,"A", "B")))+
          scale_fill_manual(guide=FALSE, values=c("red", "black"))+
          theme(axis.text.x = element_text(color = colored2,angle = 75, hjust = 1))+
          labs(title = "Total Installs by Category", y = "Total Installs", x = "Category")
  })
  output$table1 <- renderTable({
    cat <- google %>% filter(Category == input$cat) %>% arrange(desc(Reviews)) 
    cat[1:10,c(1,3:6)]
  })
  
  output$plot3 <- renderPlot({
    games <- google %>% filter(Category == "GAME")
    ggplot(games)+geom_bar(aes(Genres),fill = 'dodgerblue')+theme(axis.text.x = element_text(angle = 75, hjust = 1))
  })
  
  output$plot4 <- renderPlot({
    ggplot(google,aes(Reviews))+geom_histogram(bins = 30, fill = "dodgerblue")+xlim(input$slider1,input$slider2)
  })
}

shinyApp(ui, server)




