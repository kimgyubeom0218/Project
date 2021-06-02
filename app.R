library(shiny)
library(tidyverse)
sports<-read.csv(file="sports.csv")
covid_sports<-read.csv(file="covid_sports.csv")
covid<-read.csv(file="covid.csv")

sports<-sports %>% mutate(ave_soccer=score_soccer/match_soccer,
                          ave_base=score_base/match_base,
                          ave_basket=score_basket/match_basket)
sports<- sports %>% select(date_soccer,score_soccer,match_soccer,date_base,score_base,match_base,
                           date_basket,score_basket,match_basket,ave_soccer, ave_base, ave_basket)
covid_sports<-covid_sports %>% mutate(ave_soccer=score_soccer/match_soccer,
                                      ave_base=score_base/match_base,
                                      ave_basket=score_basket/match_basket)
covid_sports<-covid_sports %>% select(date_soccer,score_soccer,match_soccer,date_base,score_base,match_base,
                                      date_basket,score_basket,match_basket,ave_soccer, ave_base, ave_basket)


sports$ave_soccer[is.na(sports$ave_soccer)]<-0
sports$ave_base[is.na(sports$ave_base)]<-0
sports$ave_basket[is.na(sports$ave_basket)]<-0

covid_sports$ave_soccer[is.na(covid_sports$ave_soccer)]<-0
covid_sports$ave_base[is.na(covid_sports$ave_base)]<-0
covid_sports$ave_basket[is.na(covid_sports$ave_basket)]<-0


sports$ave_soccer<-round(sports$ave_soccer,3)
sports$ave_base<-round(sports$ave_base,3)
sports$ave_basket<-round(sports$ave_basket,3)

covid_sports$ave_soccer<-round(covid_sports$ave_soccer,3)
covid_sports$ave_base<-round(covid_sports$ave_base,3)
covid_sports$ave_basket<-round(covid_sports$ave_basket,3)



ui <- fluidPage(
  
  
  # App title ----
  titlePanel("코로나-19가 스포츠 경기결과에 미치는 영향"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  h4("스포츠 종목"),
                  choices = list("Soccer" ,
                                 "Baseball" ,
                                 "Basketball" ,
                                 "Covid_soccer",
                                 "Covid_baseball" ,
                                 "Covid_basketball" ),
                  selected = "Soccer"),
      helpText("코로나 이전의 그래프는 상단에 나타나고, 코로나 때의 그래프는 하단에 나타납니다.")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      textOutput("selected_var"),
      plotOutput(outputId = "plot1"),
      plotOutput(outputId = "plot2")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_var <- renderText({
    paste("You have selected information about", input$var)
   
  })
  output$plot1 <- renderPlot({
    average_score1 <-switch(input$var,
                  "Soccer" = sports$ave_soccer,
                  "Baseball" = sports$ave_base,
                  "Basketball" = sports$ave_basket)
                  
    day1 <- switch(input$var,
                  "Soccer" = sports$date_soccer,
                  "Baseball" = sports$date_base,
                  "Basketball" = sports$date_basket)

    
      ggplot(data = sports) +
        geom_line(mapping = aes(x = day1, y = average_score1 , group = 1))
    
      })
  output$plot2 <- renderPlot({
    average_score2 <- switch(input$var,
                  "Covid_soccer" = covid_sports$ave_soccer,
                  "Covid_baseball" = covid_sports$ave_base,
                  "Covid_basketball" = covid_sports$ave_basket)
    day2 <- switch(input$var,
                  "Covid_soccer" = covid_sports$date_soccer,
                  "Covid_baseball" = covid_sports$date_base,
                  "Covid_basketball" = covid_sports$date_basket)
      ggplot(data = covid_sports) + 
      geom_line(mapping = aes(x = day2, y = average_score2, group = 1))
  })
} 
shinyApp(ui = ui, server = server)