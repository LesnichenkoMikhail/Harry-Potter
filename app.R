# Точка входа: инициализация и запуск приложения
source("global.R")
source("R/ui.R")
source("R/server.R")

shiny::shinyApp(ui, server)
