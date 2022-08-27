# rsconnect::deployApp('D:/shiny/shiny_learn/multiple_testing')
library(shiny)
library(dplyr)
library(purrr)
d <- as_tibble(sample.int(6, 5, replace = T))

d %>% mutate(path = case_when(value == 1 ~ 'one', 
                              value == 2 ~ 'two', 
                              value == 3 ~ 'three',
                              value == 4 ~ 'four',
                              value == 5 ~ 'five',
                              value == 6 ~ 'six'))
# server_module <- function(id,
#                           img_path) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$photo <- renderImage({
#         list(src = img_path,
#              contentType = "image/jpeg",
#              width = 400,
#              height = 400)
#       },
#       deleteFile = FALSE)
#     })
# }
draw_dice_image <- function(id, value) {
  renderImage({
    list(
      src = file.path('images', paste0('dice-six-faces-', value, ".png")),
      contentType = "image/jpeg",
      width = 100,
      height = 100
    )
  }, deleteFile = FALSE)
}


ui <- fluidPage(
  titlePanel('Игра "верю-не верю" на кубах'),
  textOutput("welcome"),
  uiOutput("tab"),
  sidebarLayout(sidebarPanel(actionButton("click", "Let's go!"),
                             fluidRow(column(6, sliderInput('number', 'Какой номер?', min = 1, max = 6, value = 3)),
                                      column(6, sliderInput('times', 'Сколько?', min = 1, max = 10, value = 1, step = 1))),
                             actionButton('lets_guess', 'Мой ход', class = "btn-block"),
                             fluidRow(actionButton('believe', 'Верю'),
                                      actionButton('check', 'Не верю!', class = "btn-danger"))),
                mainPanel(h1("That's your dices!"),
                          fluidRow(title = 'a', column(1, imageOutput("photo1")), 
                                   column(1, imageOutput("photo2")),
                                   column(1, imageOutput("photo3")),
                                   column(1, imageOutput("photo4")),
                                   column(1, imageOutput("photo5"))),
                          h1("That's competitor dices!"),
                          fluidRow(column(1, offset = 0, imageOutput("compet_photo1")), 
                                            column(1, imageOutput("compet_photo2")),
                                            column(1, imageOutput("compet_photo3")),
                                            column(1, imageOutput("compet_photo4")),
                                            column(1, imageOutput("compet_photo5"))))),
  
  # tableOutput("dice"),
  # ,
  textOutput("guess"),
  # ,
  textOutput('computer_turn')
  # ,
  # tableOutput('gameover'),
  
)

server <- function(input, output, session) {
  # output$welcome <- renderText('Правила игры:')
  url <- a("На википедии", href="https://ru.wikipedia.org/wiki/%D0%9F%D0%B5%D1%80%D1%83%D0%B4%D0%BE")
  output$tab <- renderUI({
    tagList("Правила игры:", url)
  })
  
  dice_data <- reactiveValues()
  observeEvent(input$click, {
    dice_data$competitor <- as_tibble(sample.int(6, 5, replace = TRUE)) %>% 
      mutate(path = case_when(value == 1 ~ 'one', 
                              value == 2 ~ 'two', 
                              value == 3 ~ 'three',
                              value == 4 ~ 'four',
                              value == 5 ~ 'five',
                              value == 6 ~ 'six'))
    dice_data$player <- as_tibble(sample.int(6, 5, replace = TRUE)) %>% 
      mutate(path = case_when(value == 1 ~ 'one', 
                              value == 2 ~ 'two', 
                              value == 3 ~ 'three',
                              value == 4 ~ 'four',
                              value == 5 ~ 'five',
                              value == 6 ~ 'six'))
    
    # cat(dice_data$player)
    # map2(.x = dice_data$player$value,
    #      .y = file.path("images/", paste0('dice-six-faces-', dice_data$player[1,2], ".png")),
    #      .f = server_module)
    # req(input$click)
    # removed since header
    # output$your_dices <- renderText('Your dices!')
    map(1:5, .f = ~ draw_dice_image('images', dice_data$player[.x, 2]))
    output$photo1 <- draw_dice_image('images', dice_data$player[1,2])
    output$photo2 <- draw_dice_image('images', dice_data$player[2,2])
    output$photo3 <- draw_dice_image('images', dice_data$player[3,2])
    output$photo4 <- draw_dice_image('images', dice_data$player[4,2])
    output$photo5 <- draw_dice_image('images', dice_data$player[5,2])
    
  }
  )
  # if (!input$click) {
  #   validate("x can not be negative for this transformation")
  # } 
  output$dice <- renderTable({dice_data$player})
  # output$guess <- renderText("let's guess")
  guess_data <- reactiveValues()
  observeEvent(input$number, {
    updateSliderInput(inputId = 'times', min = input$number+1)
  })
  observeEvent(input$lets_guess, {
    guess_data$times <- input$times + 1
    guess_data$number <- input$number + 1
    output$computer_turn <- renderText({paste(guess_data$times, 'раза', 'по',
                                              guess_data$number,  sep = ' ')})
  })
  # observeEvent()
  observeEvent(input$check, {
    output$gameover <- renderTable({dice_data$competitor})
    # output$your_dices <- renderText('Your dices!')
    output$compet_photo1 <- {
      # req(input$click)
      draw_dice_image('images', dice_data$competitor[1,2])
    }
    output$compet_photo2 <- draw_dice_image('images', dice_data$competitor[2,2])
    output$compet_photo3 <- draw_dice_image('images', dice_data$competitor[3,2])
    output$compet_photo4 <- draw_dice_image('images', dice_data$competitor[4,2])
    output$compet_photo5 <- draw_dice_image('images', dice_data$competitor[5,2])
  })
  # output$gameover <- renderTable(check_dice())
}

shinyApp(ui, server)
