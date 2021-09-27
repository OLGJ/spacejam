library(shiny)
library(shinythemes)

# Call CME function and set dates to use in app
data <- spacejam("2010-01-01", "2010-12-31")$data

# Define UI for miles per gallon app ----
ui <- fluidPage(
  theme = shinytheme("darkly"),

  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "CME data",
    tabPanel("A plot",
             sidebarPanel(
               tags$h3("Input:"),
               sliderInput("range", "Range:",
                           min = 1, max = tail(data$index, 1),
                           value = c(2,5), step = 1)),

             mainPanel(
               h1("CME Plot"),
               #h4("CME speed:"),  ### Remove comment to also display speed as text
               #textOutput("SliderText"),
               h4("Distribution:"),
               plotOutput('Hist'))

    ),
    tabPanel("Cool pictures",
             sidebarPanel(
               tags$h3("Pictures"),
               selectInput("pic", "Choose picture:",
                           choices = c("Pic1", "Pic2"))),
             mainPanel(
               h3("Selected awesome picture:"),
               uiOutput("pic1"),
               h5("Picture credit: @NASA"),
               h3("Fun fact:"),
               h4("CMEs are magnetically generated solar phenomenon that can
                  send billions of tons of solar particles, or plasma, into
                  space that can reach Earth one to three days later and affect
                  electronic systems in satellites and on the ground.
                  Credit: NASA")
             ))
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  my_range <- reactive({
    range_ <- c(input$range[1]:input$range[2])
    use_index <- data$index[range_]
    use_speed <- data$speed[range_]
    use_type <- data$type[range_]
    df <- data.frame("index" = use_index,
                     "speed" = use_speed,
                     "type" = use_type)
    return(df)
    })

  #output$SliderText <- renderText({my_range()$speed}) ### Uncomment to display
  output$Hist <- renderPlot({
    req(my_range())
    hist(x = my_range()$speed, border="blue", main="Speed for selected values",
         xlab="Speed")
  })
  # For selecting/displaying pictures
  pic_choice <- reactive({
    picture_variabel <- input$pic
  })
  output$pic1 <- renderUI({
    if(pic_choice() == "Pic1"){img(src='cme1.jpg', height = "300px")}
    else if(pic_choice() == "Pic2"){img(src='cme304_june.jpg', height = "300px")}
  })
}
shinyApp(ui, server)
