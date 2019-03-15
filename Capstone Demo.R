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
  #img(src="doNotOpen.png"),
  textInput("text_entry","Enter dialogue: ",width="500px"),
  h4("Word Frequency"),
  tableOutput("freq_table"),
  radioButtons("pos", "Filter by Part of Speech:",
               c("All" = "pos_all",
                 "Nouns" = "pos_nouns",
                 "Adjectives" = "pos_adjectives",
                 "Verbs" = "pos_verbs")),
  h4("Nouns"),
  verbatimTextOutput("pos_debug1"),
  h4("Verbs"),
  verbatimTextOutput("pos_debug2"),
  h4("Adjectives"),
  verbatimTextOutput("pos_debug3"),
  h4("Sentence Sentiment"),
  dataTableOutput("sent_table"),
  h4("Filtered by Keyword Table"),
  textInput("keyword","Enter keyword: ",width="500px"),
  tableOutput("filtered_by_keyword_table")
)

server <- function(input,output){
  
  udmodel_download <- udpipe_download_model(language = "english")
  udmodel <- udpipe_load_model(file = udmodel_download$file_model)
  
  tagged_pos_table <- reactive({
    x <- udpipe_annotate(udmodel,unlist(strsplit(input$text_entry, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)))
    x_as_frame <- as.data.frame(x)
    pos_tagged_words_table <- x_as_frame %>% 
      select(token,upos)
    return(pos_tagged_words_table)
  })
  
  filter_by_pos_nouns <- reactive({
      nouns_only <- tagged_pos_table() %>% 
        filter(upos=="NOUN") %>%
        select(token)
    return(nouns_only)
  })
  
  filter_by_pos_verbs <- reactive({
    verbs_only <- tagged_pos_table() %>% 
      filter(upos=="VERB") %>%
      select(token)
    return(verbs_only)
  })
  
  filter_by_pos_adjectives <- reactive({
    adjectives_only <- tagged_pos_table() %>% 
      filter(upos=="ADJ") %>%
      select(token)
    return(adjectives_only)
  })
  
  
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
    if(input$pos == "pos_all"){
      freq_frame <- data.frame(term,term_frequency)
    }
    
    if(input$pos == "pos_nouns"){
      freq_frame <- data.frame(term,term_frequency) %>%
        filter(term %in% filter_by_pos_nouns())
    }
    
    if(input$pos == "pos_verbs"){
      freq_frame <- data.frame(term,term_frequency) %>%
        filter(term %in% filter_by_pos_verbs())
    }
    
    if(input$pos == "pos_adjectives"){
      freq_frame <- data.frame(term,term_frequency) %>%
        filter(term %in% filter_by_pos_adjectives())
    }
    
    return(freq_frame)
    })
  
  calculate_sentiment <- reactive({
    sentences <- get_sentences(input$text_entry)
    sentence_sentiment <- sentiment(sentences)
    sent_frame <- data.frame(sentences,sentence_sentiment$sentiment)
    names(sent_frame) <- c("sentence","sentiment")
    return(sent_frame)
  })
  
  filter_by_keyword <- reactive({
    filtered_by_keyword_table <- calculate_sentiment() %>% 
      filter(grepl(input$keyword,sentence)) %>% 
      summarize(average_sentiment = mean(sentiment))
    return(filtered_by_keyword_table)
  })
  
  output$freq_table <- renderTable(calculate_frequency())
  output$sent_table <- renderDataTable(calculate_sentiment())
  output$filtered_by_keyword_table <- renderTable(filter_by_keyword())
  output$pos_debug1 <- renderPrint(filter_by_pos_nouns())
  output$pos_debug2 <- renderPrint(filter_by_pos_verbs())
  output$pos_debug3 <- renderPrint(filter_by_pos_adjectives())
}


shinyApp(ui=ui,server=server)