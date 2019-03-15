library(shiny)
library(shinythemes)
library(tm)
library(tidyverse)
library(udpipe)
library(tidytext)
library(sentimentr)
library(DT)
library(wordcloud)


ui <- fluidPage(
  theme="readable",
  sidebarLayout(
    sidebarPanel(
      img(src="wizard.png",width="50%"),
      titlePanel("Vooey Wizard"),
      textInput("vooeys_question","Enter Vooey's question:",width="100%"),
      textAreaInput("text_entry","Enter dialogue: ",width="100%",height="200px")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Word Frequency",
          h2("Word Frequency"),
          plotOutput("freq_cloud"),
          radioButtons("pos", "Filter by:",
                       c("All" = "pos_all",
                         "Nouns" = "pos_nouns",
                         "Adjectives" = "pos_adjectives",
                         "Verbs" = "pos_verbs")),
          dataTableOutput("freq_table")
        ),
        tabPanel(
          "Parts of Speech",
          h2("Parts of Speech"),
          h4("Nouns"),
          verbatimTextOutput("pos_debug1"),
          h4("Verbs"),
          verbatimTextOutput("pos_debug2"),
          h4("Adjectives"),
          verbatimTextOutput("pos_debug3")
        ),
        tabPanel(
          "Sentiment",
          h2("Sentence Sentiment"),
          dataTableOutput("sent_table"),
          h4("Filtered by Keyword Table"),
          textInput("keyword","Enter keyword: ",width="500px"),
          tableOutput("filtered_by_keyword_table")
        ),
        tabPanel(
          "N-Grams"
        )
      )
    )
  )
)

server <- function(input,output){
  
  udmodel_download <- udpipe_download_model(language = "english")
  udmodel <- udpipe_load_model(file = udmodel_download$file_model)
  
  tagged_pos_table <- reactive({
    text_entry_character_vector <- unlist(strsplit(input$text_entry, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    x <- udpipe_annotate(udmodel,text_entry_character_vector)
    x_as_frame <- as.data.frame(x)
    pos_tagged_words_table <- x_as_frame %>% 
      select(token,upos)
    return(pos_tagged_words_table)
  })
  
  filter_by_pos_nouns <- reactive({
    nouns_only <- tagged_pos_table() %>% 
      filter(upos=="NOUN") %>%
      select(token)
    return(unique(nouns_only$token))
  })
  
  filter_by_pos_verbs <- reactive({
    verbs_only <- tagged_pos_table() %>% 
      filter(upos=="VERB") %>%
      select(token)
    return(unique(verbs_only$token))
  })
  
  filter_by_pos_adjectives <- reactive({
    adjectives_only <- tagged_pos_table() %>% 
      filter(upos=="ADJ") %>%
      select(token)
    return(unique(adjectives_only$token))
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
    freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
    if(input$pos=="pos_nouns"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_nouns())
    }
    if(input$pos=="pos_verbs"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_verbs())
    }
    if(input$pos=="pos_adjectives"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_adjectives())
    }
    names(freq_frame) <- c("question","term","term_frequency")
    freq_frame <- datatable(freq_frame, options = list(searchHighlight = TRUE),rownames=FALSE)
    return(freq_frame)
  })
  
  create_wordcloud <- reactive({
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
    freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
    if(input$pos=="pos_nouns"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_nouns())
    }
    if(input$pos=="pos_verbs"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_verbs())
    }
    if(input$pos=="pos_adjectives"){
      freq_frame <- data.frame(input$vooeys_question,term,term_frequency)
      freq_frame <- freq_frame %>% 
        filter(term %in% filter_by_pos_adjectives())
    }
    cloud <- wordcloud(words = freq_frame$term, freq = freq_frame$term_frequency, min.freq = 1,
                       max.words=200, random.order=FALSE, rot.per=0.35)
    return(cloud)
  })
  
  calculate_sentiment <- reactive({
    sentences <- get_sentences(tolower(input$text_entry))
    sentence_sentiment <- sentiment(sentences)
    sent_frame <- data.frame(input$vooeys_question,sentences,sentence_sentiment$sentiment)
    names(sent_frame) <- c("question","sentence","sentiment")
    sent_frame <- datatable(sent_frame, options = list(searchHighlight = TRUE),rownames=FALSE)
    return(sent_frame)
  })
  
  calculate_sentiment_dataDotFrame <- reactive({
    sentences <- get_sentences(tolower(input$text_entry))
    sentence_sentiment <- sentiment(sentences)
    sent_frame <- data.frame(input$vooeys_question,sentences,sentence_sentiment$sentiment)
    names(sent_frame) <- c("question","sentence","sentiment")
    return(sent_frame)
  })
  
  filter_by_keyword <- reactive({
    filtered_by_keyword_table <-   calculate_sentiment_dataDotFrame() %>% 
      filter(grepl(input$keyword,sentence)) %>% 
      summarize(average_sentiment = mean(sentiment))
    return(filtered_by_keyword_table)
  })
  
  
  output$freq_table <- renderDataTable(calculate_frequency())
  output$freq_cloud <- renderPlot(create_wordcloud())
  output$sent_table <- renderDataTable(calculate_sentiment())
  output$filtered_by_keyword_table <- renderTable(filter_by_keyword())
  output$pos_debug1 <- renderPrint(filter_by_pos_nouns())
  output$pos_debug2 <- renderPrint(filter_by_pos_verbs())
  output$pos_debug3 <- renderPrint(filter_by_pos_adjectives())
}


shinyApp(ui=ui,server=server)