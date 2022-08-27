# rsconnect::deployApp('D:/shiny/shiny_learn/multiple_testing')
library(shiny)
library(dplyr)
library(purrr)
library(stringr)
# d <- as_tibble(sample.int(6, 5, replace = T))

as_tibble(sample.int(6, 5, replace = T)) %>% 
  mutate(path = case_when(value == 1 ~ 'one', 
                          value == 2 ~ 'two', 
                          value == 3 ~ 'three',
                          value == 4 ~ 'four',
                          value == 5 ~ 'five',
                          value == 6 ~ 'six'))

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

believe_or_checkout <- function(my_dices, bet_dice, bet_times) {
  my_dices_of_this_bet <- my_dices %>% 
    pull(value) %>% 
    str_detect(as.character(bet_dice)) %>% 
    sum()
  # print(my_dices_of_this_bet)
  # print(bet_times)
  if (bet_times - my_dices_of_this_bet >= 2) {
    return('gameover')
  }
  else {
    comp_max_dice <- my_dices %>% 
      count(value) %>% 
      arrange(desc(n), desc(value)) %>% 
      slice(1)
    comp_max_dice[2] <- comp_max_dice[2] + 1
    return(comp_max_dice)
  }
}

display_gameover <- function(my_dices, output) {
  output$gameover <- renderTable({dice_data$competitor})
  output$compet_photo1 <- draw_dice_image('images', dice_data$competitor[1,2])
  output$compet_photo2 <- draw_dice_image('images', dice_data$competitor[2,2])
  output$compet_photo3 <- draw_dice_image('images', dice_data$competitor[3,2])
  output$compet_photo4 <- draw_dice_image('images', dice_data$competitor[4,2])
  output$compet_photo5 <- draw_dice_image('images', dice_data$competitor[5,2])
}

gameoverUI <- function(id) {
  tagList(tableOutput(NS(id, 'gameover')))
}

gameoverServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    output$gameover <- renderTable({df})
  })
}

ui <- fluidPage(
  titlePanel('Игра "верю-не верю" на кубах'),
  textOutput("welcome"),
  uiOutput("tab"),
  sidebarLayout(sidebarPanel(actionButton("click", "Let's go!"),
                             fluidRow(column(6, sliderInput('number', 'Какой номер?', min = 1, max = 6, value = 3)),
                                      column(6, sliderInput('times', 'Сколько?', min = 1, max = 10, value = 1, step = 1))),
                             actionButton('lets_guess', 'Мой ход', class = "btn-block"),
                             textOutput('computer_turn'),
                             fluidRow(actionButton('believe', 'Верю'),
                                      actionButton('check', 'Не верю!', class = "btn-danger")),
                             # tableOutput('gameover')
                             gameoverUI('end')),
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
  # textOutput("guess"),
  # ,
  
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
  # observeEvent(input$number, {
  #   updateSliderInput(inputId = 'times', min = input$number+1)
  # })
  observeEvent(input$lets_guess, {
    guess_data$result <- believe_or_checkout(my_dices = dice_data$competitor, bet_dice = input$number, bet_times = input$times)
    if (guess_data$result[1] == 'gameover') {
      gameoverServer('end', dice_data$competitor)
      display_gameover(dice_data$competitor, output)
      # output$your_dices <- renderText('Your dices!')
    }
    else {
      output$computer_turn <- renderText({paste(guess_data$result[2], 'раза', 'по',
                                                guess_data$result[1],  sep = ' ')})
    }
    # guess_data$times <- input$times + 1
    # guess_data$number <- input$number + 1
    # output$computer_turn <- renderText({paste(guess_data$times, 'раза', 'по',
    #                                           guess_data$number,  sep = ' ')})
  })
  # observeEvent()
  observeEvent(input$check, {
    # display_gameover(dice_data$competitor)
    gameoverServer('end', dice_data$competitor)
    # output$gameover <- renderTable({dice_data$competitor})
    # # output$your_dices <- renderText('Your dices!')
    # output$compet_photo1 <- {
    #   # req(input$click)
    #   draw_dice_image('images', dice_data$competitor[1,2])
    # }
    # output$compet_photo2 <- draw_dice_image('images', dice_data$competitor[2,2])
    # output$compet_photo3 <- draw_dice_image('images', dice_data$competitor[3,2])
    # output$compet_photo4 <- draw_dice_image('images', dice_data$competitor[4,2])
    # output$compet_photo5 <- draw_dice_image('images', dice_data$competitor[5,2])
  })
  # output$gameover <- renderTable(check_dice())
}

shinyApp(ui, server)
