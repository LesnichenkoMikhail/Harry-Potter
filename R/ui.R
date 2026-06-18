library(shiny)
library(bslib)
library(plotly)
library(readr)
library(visNetwork)

addResourcePath("data-assets", normalizePath("data", mustWork = TRUE))

book_choices <- if (exists("df", inherits = TRUE) && is.data.frame(get("df", inherits = TRUE))) {
  unique(get("df", inherits = TRUE)$book)
} else {
  unique(read_csv("data/harry_potter_books.csv", show_col_types = FALSE)$book)
}

ui <- page_navbar(
  title = "Гарри Поттер: анализ текстов",
  theme = bs_theme(
    version = 5,
    bg = "#f7f1e4",
    fg = "#1f1f1f",
    primary = "#7f0909",
    secondary = "#1a472a",
    base_font = font_google("Roboto"),
    heading_font = font_google("Cinzel")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css?v=2")
  ),

  nav_panel(
    "Главная",
    div(
      class = "home-hero",
      h1("Гарри Поттер: анализ текстов книг"),
      h3("Интерактивное исследование корпуса книг"),
      div(
        class = "team-list",
        strong("Команда проекта"),
        span("Лесниченко Михаил"),
        span("Тарасов Александр"),
        span("Широков Иван")
      ),
      div(
        class = "home-description",
        p("Это приложение помогает посмотреть на серию книг о Гарри Поттере как на единый корпус текстов: сравнить книги, увидеть повторяющиеся темы и заметить, как меняется повествование от главы к главе."),
        p("Во вкладках можно выбрать книгу и изучить эмоциональную динамику, характерные слова, частоту появления персонажей и связи между ними. Отдельно можно вставить небольшой фрагмент текста и посмотреть, к какой книге он больше всего похож по языку."),
        p("Проект рассчитан на исследовательский просмотр: графики реагируют на выбранные настройки, а тяжелые расчеты запускаются по кнопке, чтобы приложение оставалось стабильным.")
      ),
      tags$a(
        class = "repo-link",
        href = "https://github.com/LesnichenkoMikhail/Harry-Potter",
        target = "_blank",
        "Репозиторий GitHub"
      )
    )
  ),

  nav_panel(
    "Персонажи",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("book_char", "Книга", choices = book_choices)
      ),
      card(
        card_header("Упоминания персонажей"),
        plotlyOutput("char_plot", height = 520)
      )
    )
  ),

  nav_panel(
    "Сеть",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("book_network", "Книга", choices = book_choices),
        sliderInput(
          "min_connection",
          "Минимальная сила связи",
          min = 1,
          max = 40,
          value = 5
        ),
        actionButton("build_network_btn", "Построить сеть", class = "btn-primary")
      ),
      card(
        card_header("Сеть совместных упоминаний"),
        visNetworkOutput("network_plot", height = 620)
      )
    )
  ),

  nav_panel(
    "Тональность",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("book_sent", "Книга", choices = book_choices),
        selectInput(
          "sent_lexicon",
          "Лексикон",
          choices = c("Bing" = "bing", "AFINN" = "afinn")
        )
      ),
      card(
        card_header("Эмоциональная динамика по главам"),
        plotlyOutput("sent_plot", height = 520)
      )
    )
  ),

  nav_panel(
    "Ключевые слова",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("book_tfidf", "Книга", choices = book_choices),
        radioButtons(
          "tfidf_view",
          "Вид графика",
          choices = c("Столбики" = "bar", "Облако слов" = "cloud"),
          selected = "bar"
        )
      ),
      card(
        card_header("Ключевые слова книги"),
        plotlyOutput("tfidf_plot", height = 520)
      )
    )
  ),

  nav_panel(
    "Классификатор",
    layout_sidebar(
      sidebar = sidebar(
        textAreaInput(
          "input_text",
          "Фрагмент текста",
          rows = 8,
          placeholder = "Например: Harry looked at Ron and Hermione..."
        ),
        actionButton("predict_btn", "Определить книгу", class = "btn-primary")
      ),
      card(
        card_header("Предсказание"),
        h3(textOutput("prediction_result")),
        tableOutput("probability_table")
      ),
      card(
        card_header("Качество модели"),
        p(
          class = "metric-note",
          "Метрики ниже посчитаны только на тестовых главах, которые не использовались при обучении."
        ),
        tableOutput("model_metrics"),
        p(
          class = "metric-note",
          "Регуляризация штрафует слишком сложную модель. Чем больше значение, тем сильнее модель сжимает веса признаков; чем меньше значение, тем свободнее она учитывает слова и частоты персонажей."
        ),
        tableOutput("model_settings")
      )
    )
  )
)
