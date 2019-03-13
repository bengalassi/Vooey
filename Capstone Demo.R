library(shiny)
library(shinythemes)
library(tm)
library(tidyverse)
library(udpipe)
library(tidytext)
library(sentimentr)
library(DT)


ui <- fluidPage(
  titlePanel("Vooey Wizard"),
  textInput("text_entry","Enter dialogue: ",width="500px"),
  h4("Word Frequency"),
  tableOutput("freq_table"),
  h4("Sentence Sentiment"),
  dataTableOutput("sent_table")
)

server <- function(input,output){
  
  calculate_frequency <- reactive({
    
    themes_source <- VectorSource(input$text_entry)
    
    themes_corpus <- VCorpus(themes_source)
    
    other_words_to_remove <- c()
    
    corpus <- tm_map(themes_corpus,removePunctuation)
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    
    corpus <- tm_map(corpus, removeWords, c(stopwords("en"),other_words_to_remove))
    
    cleaned_themes_corpus <- corpus
    
    themes_tdm <- TermDocumentMatrix(cleaned_themes_corpus)
    
    themes_m <- as.matrix(themes_tdm)

    term_frequency <- rowSums(themes_m)
    
    term_frequency <- sort(term_frequency,decreasing = TRUE)
    
    term <- names(term_frequency)
    
    freq_frame <- data.frame(term,term_frequency)
    
    return(freq_frame)
    
    })
  
  calculate_sentiment <- reactive({
    sentences <- get_sentences(input$text_entry)
    sentence_sentiment <- sentiment(sentences)
    sent_frame <- data.frame(sentences,sentence_sentiment$sentiment)
    names(sent_frame) <- c("sentence","sentiment")
    return(sent_frame)
  })
  
  output$freq_table <- renderTable(calculate_frequency())
  output$sent_table <- renderDataTable(calculate_sentiment())
}


shinyApp(ui=ui,server=server)