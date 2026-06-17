library(shiny)
library(bslib)
library(plotly)
library(visNetwork)

ui <- page_navbar(
  
  title = "Harry Potter Text Explorer",
  
  ########
  theme = bslib::bs_theme(
    version = 5,
    bg = "#F5E6C8",
    fg = "#1a1a1a",
    primary = "#7F0909",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Cinzel")
  ),
  
  nav_panel(
    
    "Home",
    
    div(
      style = "
      background: linear-gradient(135deg, #740001, #1a472a, #0e1a40, #d3a625);
      color: white;
      padding: 40px;
      border-radius: 15px;
      text-align: center;
    ",
      
      h1("Гарри Поттер: анализ текстов книг"),
      
      br(),
      
      h3("Digital Humanities проект на основе полного корпуса книг"),
      
      hr(),
      
      h4(" Команда проекта:"),
      
      
      div(
        "Лесниченко Михаил",
        br(),
        "Тарасов Александр",
        br(),
        "Широков Иван"
      ),
      
      
      
      br(),
      
      p("Проект исследует текстовую вселенную Гарри Поттера с помощью методов анализа текста, машинного обучения и сетевого анализа."),
      
      br(),
      
      tags$blockquote(
        style = "font-style: italic; opacity: 0.9;",
        "«Счастье можно найти даже в тёмные времена, если не забывать обращаться к свету» — Альбус Дамблдор"
      )
    )
  ),
  
  nav_panel(
    
    "Characters",
    
    selectInput(
      "book_char",
      "Выберите книгу:",
      choices = unique(df$book)
    ),
    
    plotlyOutput("char_plot")
  ),
  
  nav_panel(
    
    "Network",
    
    selectInput(
      "book_network",
      "Выберите книгу:",
      choices = unique(df$book)
    ),
    
    sliderInput(
      "min_connection",
      "Минимальная сила связи:",
      min = 1,
      max = 20,
      value = 2
    ),
    
    visNetworkOutput("network_plot")
  ),
  
  nav_panel(
    "Sentiment",
    
    selectInput("book_sent", "Выберите книгу:",
                choices = unique(df$book)),
    
    plotlyOutput("sent_plot")
  ),
  
  nav_panel(
    
    "TF-IDF",
    
    selectInput(
      "book_tfidf",
      "Выберите книгу:",
      choices = unique(df$book)
    ),
    
    plotlyOutput("tfidf_plot")
  ),
  
  nav_panel(
    
    "Classifier",
    
    textAreaInput(
      "input_text",
      "Введите текст из книги:",
      rows = 6
    ),
    
    actionButton(
      "predict_btn",
      "Predict book"
    ),
    
    br(),
    br(),
    
    verbatimTextOutput("prediction_result")
  )
)