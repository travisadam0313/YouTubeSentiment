#Remember to install.packages("example") if you do not already have them...
library(shiny)
library(youtubecaption)
library(lexicon)
library(ggplot2)

ui<-fluidPage(
  titlePanel("YouTube Video Sentiment"),
  sidebarPanel(
    uiOutput("linkInput"),
    actionButton("Search",label="Search")
  ),
  mainPanel(
    plotOutput("Chart")
  )
)

server<-function(input,output){
  output$linkInput <- renderUI({
    textInput("linkInput", "Enter YouTube URL", "")
  })
  #ActionButton Sequence
  observeEvent(input$Search,{
    url<-input$linkInput
    Captions <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(Captions)<-c('Text')
    
    tryCatch({
      url <- paste0(url)
      caption <- get_caption(url)
      for (i in 1:nrow(caption)){
        df <- data.frame(strsplit(caption$text[i]," "))
        colnames(df) <- c('Text')
        Captions <- rbind(df,Captions)
      }
      Captions<-merge(Captions, nrc_emotions, by.x="Text", by.y="term", sort = TRUE)
      Captions <- aggregate(. ~ Text, Captions, sum)
      
      CaptionsPlot<-Captions
      CaptionsPlot$Text<-"Total"
      CaptionsPlot <- aggregate(. ~ Text, CaptionsPlot, sum)
      CaptionsPlot <- t(CaptionsPlot)
      CaptionsPlot <- cbind(rownames(CaptionsPlot), data.frame(CaptionsPlot, row.names=NULL,stringsAsFactors = FALSE))
      colnames(CaptionsPlot) <- c('Sentiment','Score')
      CaptionsPlot <- CaptionsPlot [2:9,]
      CaptionsPlot$Score<-as.numeric(CaptionsPlot$Score)
      CaptionsPlot$Score <- (CaptionsPlot$Score/max(CaptionsPlot$Score))*100
      #Plot Data
      C <- ggplot(CaptionsPlot, aes(Sentiment, Score))
      Chart<-C + geom_col(fill="#8ebef5") + ggtitle(url)
      Chart
      output$Chart <- renderPlot({Chart})
    },
    error=function(e) { 
      showNotification(paste0("Error: Video probably doesn't have captions..."),type="error") #this will display a little red push notification at the bottom right of the screen
      return(NULL) })
  })
}

shinyApp(ui=ui,server=server)