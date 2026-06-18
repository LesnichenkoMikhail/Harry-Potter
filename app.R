source("global.R", local = TRUE)
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

shiny::shinyApp(ui, server)
