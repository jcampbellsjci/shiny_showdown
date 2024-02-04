library(shiny)
library(shinyjqui)
library(DT)
library(png)
library(plotly)
library(ggpubr)
library(tidyverse)


#### Pre-Run ####

##### Player Selection #####

# Reading in player database
# Making a few formatting edits
showdown_df <- read_csv("../data/player_db.csv") %>%
  rename_with(.fn = ~ tolower(.)) %>%
  rename_with(.fn = ~ str_replace(., " ", "_")) %>%
  mutate(gen_position = case_when(
    main_position %in% c("OF", "LF/RF", "CF") ~ "OF",
    main_position %in% c("RELIEVER", "CLOSER") ~ "CLOSER",
    .default = main_position))

# Team lineups and rotations are randomly selected
# TODO: Create a "draft page" where you can choose from a random selection at each position
# Looping through positions and randomly picking players for two teams
position <- c("CA", "1B", "2B", "3B", "SS", "OF", "STARTER", "CLOSER")
team_list <- list()
for (i in position) {
  player_option <- showdown_df %>%
    filter(gen_position == i)
  
  # We sample 3 outfielders per team
  if(i == "OF"){
    sample <- player_option %>%
      sample_n(6) %>%
      mutate(team = c(rep("A", 3), rep("B", 3)))
  # We sample 1 of every other position per team
  } else{
    sample <- player_option %>%
      sample_n(2) %>%
      mutate(team = c("A", "B"))
  }
  
  team_list[[i]] <- sample
}
team_df <- bind_rows(team_list)

# Going to pull a random position player to play DH for each team
dh_sample <- showdown_df %>%
  filter(!(gen_position %in% c("STARTER", "CLOSER"))) %>%
  filter(!(str_c(player, year) %in% str_c(team_df$player, team_df$year))) %>%
  mutate(gen_position = "DH") %>%
  sample_n(2) %>%
  mutate(team = c("A", "B"))
team_df <- team_df %>%
  bind_rows(dh_sample)

# Assigning players to a specific team dataframe
team_a <- team_df %>%
  filter(team == "A")
team_a_batters <- team_a %>%
  filter(!(gen_position %in% c("STARTER", "CLOSER")))
team_a_pitchers <- team_a %>%
  filter(gen_position %in% c("STARTER", "CLOSER"))

team_b <- team_df %>%
  filter(team == "B")
team_b_batters <- team_b %>%
  filter(!(gen_position %in% c("STARTER", "CLOSER")))
team_b_pitchers <- team_b %>%
  filter(gen_position %in% c("STARTER", "CLOSER"))


##### Field Plot #####

# Setting up a tibble that will be used for a base plot
# These are coordinates for points that will be overlayed on a baseball diamond
base_plot <- tibble(value = c(1, 2, 3),
                    x = c(.675, .01, -.675),
                    y = c(.01, .7, .01))


#### UI ####

ui <- fluidPage(
  
  titlePanel("MLB Showdown"),
  
  # Going to set up different tabs for various information
  tabsetPanel(
    
    ##### Parameter Tab #####
    
    # Setting up a parameterized value panel
    # This is where you can set game settings
    tabPanel(
      title = "parameters",
      
      sliderInput(inputId = "inning_input",
                  label = "Number of Innings",
                  min = 1, max = 9,
                  value = 9),
      
      selectInput(inputId = "player_selection_input",
                  label = "Player Selection Style",
                  choices = c("Random"))),
    
    
    ##### Lineup Tab #####
    
    # Setting up a lineup setter panel
    # This is where you can view and edit your roster
    tabPanel(
      title = "lineup",
      
      # Creating a row of lineup setting actions for team a
      fluidRow(column(4, orderInput(inputId = "dest_a",
                                    label = "Team-A Lineup",
                                    items = team_a_batters$player)),
               column(4, actionButton(inputId = "set_lineup_a",
                                      label = "Set Team-A Lineup"),
                      align = "center", style = "margin-top: 40px;"),
               column(4, actionButton(inputId = "autoset_lineup_a",
                                      label = "Auto Set Team-A Lineup"),
                      align = "center", style = "margin-top: 40px;")),
      
      br(),
      
      fluidRow(column(12, DT::dataTableOutput(outputId = "test"))),
      
      # Creating a row showing roster for team a
      fluidRow(column(4, fluidRow(
        column(12, DT::dataTableOutput(outputId = "team_a_batters")),
        column(12, DT::dataTableOutput(outputId = "team_a_pitchers"),
               div(style = "height:10px;")))),
        column(4, div(uiOutput(outputId = "lineup_image_a",
                               align = "center"))),
        column(4, div(uiOutput(outputId = "rotation_image_a",
                               align = "center")))),
      
      br(),
      
      # Creating a row of lineup setting actions for team b
      fluidRow(column(4, orderInput(inputId = "dest_b",
                                    label = "Team-B Lineup",
                                    items = team_b_batters$player)),
               column(4, actionButton(inputId = "set_lineup_b",
                                      label = "Set Team-B Lineup"),
                      align = "center", style = "margin-top: 40px;"),
               column(4, actionButton(inputId = "autoset_lineup_b",
                                      label = "Auto Set Team-B Lineup"),
                      align = "center", style = "margin-top: 40px;")),
      
      br(),
      
      # Creating a row showing roster for team b
      fluidRow(
        column(4, fluidRow(
          column(12, DT::dataTableOutput(outputId = "team_b_batters")),
          column(12, DT::dataTableOutput(outputId = "team_b_pitchers"),
                 div(style = "height:10px;")))),
        column(4, div(uiOutput(outputId = "lineup_image_b",
                               align = "center"))),
        column(4, div(uiOutput(outputId = "rotation_image_b",
                               align = "center"))))),
    
    
    ##### Game Tab #####
    
    # Setting up a game panel
    # This is where you actually play the game
    tabPanel(
      title = "game",
      
      # Creating row for rolling die and seeing output
      fluidRow(column(2, actionButton(inputId = "die_roll",
                                      label = "Roll Dice",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                      align = "center",
                      style = "margin-top: 40px;"),
               column(2, fluidRow(column(12, align = "center",
                                         p(strong("Command Roll")),
                                         style = "font-size:17px; margin-top: 20px;",
                                         div(style = "height:10px;"))),
                      fluidRow(column(12, align = "center",
                                      textOutput(outputId = "die_roll_1"),
                                      style = "font-size:16px;",
                                      div(style = "height:10px;")))),
               column(2, fluidRow(column(12, align = "center",
                                         p(strong("Outcome Roll")),
                                         style = "font-size:17px; margin-top: 20px;",
                                         div(style = "height:10px;"))),
                      fluidRow(column(12, align = "center",
                                      textOutput(outputId = "die_roll_2"),
                                      style = "font-size:16px;",
                                      div(style = "height:10px;"))))),
      
      br(),
      
      # Creating row showing batter v pitcher and game outcome
      fluidRow(column(6, DT::dataTableOutput(outputId = "pitch_outcome")),
               column(6, DT::dataTableOutput(outputId = "outcome2"))),
      
      br(),
      
      # Creating row showing batter and pitcher cards
      fluidRow(column(6, div(uiOutput(outputId = "game_image_a",
                                      align = "center"))),
               column(6, div(uiOutput(outputId = "game_image_b",
                                      align = "center")))),
      
      br(),
      
      # Creating row tracking player movement through inning
      # TODO: Possibly create new tab for this and other stats
      fluidRow(column(6, DT::dataTableOutput(outputId = "game_summary")),
               column(6, DT::dataTableOutput(outputId = "outcome_movement"))),
      
      br(),
      
      # Creating row showing scoreboard and baseball diamond plot
      fluidRow(column(6, DT::dataTableOutput(outputId = "scoreboard")),
               column(6, plotlyOutput(outputId = "diamond_plot"))))
    )
  )


#### Server ####

server <- function(input, output, session) {
  #### Lineup Page ####
  
  # We'll set up a reactive lineup input
  # This will take our lineup input and rearrange our team based on it
  # If you auto-set the lineup, it'll be ordered on player points
  team_a_lineup <- reactiveValues(set_lineup = tibble(
    player = team_a_batters$player))
  observeEvent(input$autoset_lineup_a, {
    team_a_lineup$set_lineup <- tibble(player = input$dest_a) %>%
      inner_join(team_a_batters) %>%
      arrange(desc(points))
    
    updateOrderInput(session, "dest_a",
                     items = team_a_lineup$set_lineup$player)
  })
  observeEvent(input$set_lineup_a, {
    team_a_lineup$set_lineup <- tibble(player = input$dest_a)
  })
  # Rendering table that will be shown
  # Ordering by what we set our lineup order to
  output$team_a_batters <- DT::renderDataTable({
    datatable(team_a_batters %>%
                arrange(match(player, team_a_lineup$set_lineup$player)) %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't'))
  })
  # Doing same thing for team b
  team_b_lineup <- reactiveValues(set_lineup = tibble(
    player = team_b_batters$player))
  observeEvent(input$autoset_lineup_b, {
    team_b_lineup$set_lineup <- tibble(player = input$dest_b) %>%
      inner_join(team_b_batters) %>%
      arrange(desc(points))
    
    updateOrderInput(session, "dest_b",
                     items = team_b_lineup$set_lineup$player)
  })
  observeEvent(input$set_lineup_b, {
    team_b_lineup$set_lineup <- tibble(player = input$dest_b)
  })
  output$team_b_batters <- DT::renderDataTable({
    datatable(team_b_batters %>%
                arrange(match(player, team_b_lineup$set_lineup$player)) %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't'))
  })
  
  
  # Creating images that will show up in lineup page when you select a player row
  output$lineup_image_a <- renderUI({
    tags$image(src = str_replace(
      team_a_batters %>%
        arrange(match(player, team_a_lineup$set_lineup$player)) %>%
        .[input$team_a_batters_rows_selected, ] %>%
        pull(card_path),
      "card_images/", ""), width = "298px", height = "415px")
  })
  output$lineup_image_b <- renderUI({
    tags$image(src = str_replace(
      team_b_batters %>%
        arrange(match(player, team_b_lineup$set_lineup$player)) %>%
        .[input$team_b_batters_rows_selected, ] %>%
        pull(card_path),
      "card_images/", ""), width = "298px", height = "415px")
  })
  
  
  # Creating tables and getting images for pitchers on lineup screen
  output$team_a_pitchers <- DT::renderDataTable({
    datatable(team_a_pitchers %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't'))
  })
  output$team_b_pitchers <- DT::renderDataTable({
    datatable(team_b_pitchers %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't'))
  })
  output$rotation_image_a <- renderUI({
    tags$image(src = str_replace(
      team_a_pitchers %>%
        .[input$team_a_pitchers_rows_selected, ] %>%
        pull(card_path),
      "card_images/", ""), width = "298px", height = "415px")
  })
  output$rotation_image_b <- renderUI({
    tags$image(src = str_replace(
      team_b_pitchers %>%
        .[input$team_b_pitchers_rows_selected, ] %>%
        pull(card_path),
      "card_images/", ""), width = "298px", height = "415px")
  })
  
  
  #### Game Page ####
  
  out_outcomes <- c("so_range", "gb_range", "fb_range", "pu_range")
  total_outs <- reactiveVal(0)
  outs <- reactiveVal(0)
  innings <- reactiveVal(1)
  
  output$game_summary <- renderDataTable({
    datatable(tibble(total_outs = total_outs(),
                     outs = outs(),
                     innings = innings()),
              options = list(dom = 't'))
  })
  
  batting_team <- reactive({
    if(floor(innings()) / innings() == 1){
      team_a_batters
    } else(
      team_b_batters
    )
  })
  pitching_team <- reactive({
    if(floor(innings()) / innings() == 1){
      team_b_pitchers
    } else(
      team_a_pitchers
    )
  })
  
  
  # The game page will be reliant on various reactive values
  # These are reactive based on the roll die action button being clicked
  # Die roll counter will be summing up how often the die is rolled
  die_roll_counter_a <- reactiveVal(0)
  die_roll_counter_b <- reactiveVal(0)
  # Lineup counter will be summing up how many times we go through the lineup
  lineup_counter_a <- reactiveVal(0)
  lineup_counter_b <- reactiveVal(0)
  # Die roll values will be tracking possible values for the first and second roll
  die_roll_value_1 <- reactiveVal(0)
  die_roll_value_2 <- reactiveVal(0)
  
  # We'll observe each time someone clicks the roll die button and sample a 1-20 roll
  # First roll will determine pitcher vs. batter chart
  # Second roll will determine outcome
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 die_roll_value_1(sample(1:20, 1))}
                 else {
                   die_roll_value_1(0)
                 }})
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 die_roll_value_2(sample(1:20, 1))}
                 else {
                   die_roll_value_2(0)
                 }})
  die_roll <- reactive({
    tibble(roll_1 = die_roll_value_1(),
           roll_2 = die_roll_value_2())
  })
  # We'll present the roll output in the tab
  output$die_roll_1 <- renderText({die_roll()$roll_1})
  output$die_roll_2 <- renderText({die_roll()$roll_2})
  
  # We'll now observe the number of die rolls and times through the lineup
  # We'll use this to index the batter that should be up when a user clicks
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 ifelse(batting_team()$team == "A",
                        die_roll_counter_a(die_roll_counter_a() + 1),
                        die_roll_counter_b(die_roll_counter_b() + 1))
                 }})
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 ifelse(batting_team()$team == "A",
                        lineup_counter_a(lineup_counter_a() + (1 / 9)),
                        lineup_counter_b(lineup_counter_b() + (1 / 9)))
                 }})
  batter_index_a <- renderText({
    if(unique(batting_team()$team) == "A"){
      die_roll_counter_a() - (9 * floor(lineup_counter_a()))      
    }
  })
  batter_index_b <- renderText({
    if(unique(batting_team()$team) == "B"){
      die_roll_counter_b() - (9 * floor(lineup_counter_b()))      
    }
  })
  
  # Determining batter that is up to bat
  batter <- reactive({
    if(unique(batting_team()$team) == "A"){
      if(batter_index_a() == 0 & floor(lineup_counter_a() != 0)){
        batting_team() %>%
          arrange(match(player, team_a_lineup$set_lineup$player)) %>%
          .[9, ]
      }
      else{
        batting_team() %>%
          arrange(match(player, team_a_lineup$set_lineup$player)) %>%
          .[batter_index_a(), ]
      }
    }
    else{
      if(batter_index_b() == 0 & floor(lineup_counter_b() != 0)){
        batting_team() %>%
          arrange(match(player, team_b_lineup$set_lineup$player)) %>%
          .[9, ]
      }
      else{
        batting_team() %>%
          arrange(match(player, team_b_lineup$set_lineup$player)) %>%
          .[batter_index_b(), ]
      }
    }
  })
  
  # Determining pitcher that is on the mound
  pitcher <- reactive({
    pitching_team() %>%
      filter(gen_position == "STARTER")
  })
  
  
  # Determining if initial die roll means batter or pitcher wins
  pitch_winner <- reactive({
    ifelse(pitcher()$command + die_roll()$roll_1 >= batter()$command,
           "pitcher", "batter")})
  # Creating an output data table that will show result of at bat
  output$pitch_outcome <- renderDataTable({
    tryCatch({
      if(unique(batting_team()$team) == "A"){
        if((batter_index_a() == 0 & floor(lineup_counter_a() == 0)) |
           (input$btnLabel == "Change Sides")){
          tibble()
        } else{
          datatable(
            tibble(metric = c("player", "command", "output", "winner"),
                   batter = c(batter()$player, batter()$command, batter()$command,
                              ifelse(pitch_winner() == "pitcher", 0, 1)),
                   pitcher = c(pitcher()$player, pitcher()$command,
                               pitcher()$command + die_roll()$roll_1,
                               ifelse(pitch_winner() == "pitcher", 1, 0))),
            options = list(dom = 't'))
        }
      } else{
        if((batter_index_b() == 0 & floor(lineup_counter_b() == 0)) |
           input$btnLabel == "Change Sides"){
          tibble()
        } else{
          datatable(
            tibble(metric = c("player", "command", "output", "winner"),
                   batter = c(batter()$player, batter()$command, batter()$command,
                              ifelse(pitch_winner() == "pitcher", 0, 1)),
                   pitcher = c(pitcher()$player, pitcher()$command,
                               pitcher()$command + die_roll()$roll_1,
                               ifelse(pitch_winner() == "pitcher", 1, 0))),
            options = list(dom = 't'))
        }
      }
    },
    error = function(e){
      tibble()
    })
  })
  
  # Determining what the outcome of the pitch will be
  outcome <- reactive({
    if(unique(batting_team()$team) == "A"){
      if((batter_index_a() == 0 & floor(lineup_counter_a() == 0)) |
         input$btnLabel == "Change Sides"){
        tibble()
      } else{
        if(pitch_winner() == "pitcher"){
          pitcher() %>%
            pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                         values_to = "range") %>%
            select(player, year, outcome, range) %>%
            mutate(range_min = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 1]),
                   range_max = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 2])) %>%
            filter(!is.na(range) & range != "—") %>%
            mutate(range_max = ifelse(is.na(range_max), range_min, range_max)) %>%
            mutate(value = map2(range_min, range_max, seq)) %>%
            unnest(value) %>%
            filter(value == die_roll()$roll_2)
        } else{
          batter() %>%
            pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                         values_to = "range") %>%
            select(player, year, outcome, range) %>%
            mutate(range_min = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 1]),
                   range_max = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 2])) %>%
            filter(!is.na(range) & range != "—") %>%
            mutate(range_max = ifelse(is.na(range_max), range_min, range_max)) %>%
            mutate(value = map2(range_min, range_max, seq)) %>%
            unnest(value) %>%
            filter(value == die_roll()$roll_2)
        }
      }
    } else{
      if((batter_index_b() == 0 & floor(lineup_counter_b() == 0)) |
         input$btnLabel == "Change Sides"){
        tibble()
      } else{
        if(pitch_winner() == "pitcher"){
          pitcher() %>%
            pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                         values_to = "range") %>%
            select(player, year, outcome, range) %>%
            mutate(range_min = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 1]),
                   range_max = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 2])) %>%
            filter(!is.na(range) & range != "—") %>%
            mutate(range_max = ifelse(is.na(range_max), range_min, range_max)) %>%
            mutate(value = map2(range_min, range_max, seq)) %>%
            unnest(value) %>%
            filter(value == die_roll()$roll_2)
        } else{
          batter() %>%
            pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                         values_to = "range") %>%
            select(player, year, outcome, range) %>%
            mutate(range_min = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 1]),
                   range_max = as.numeric(str_split(range, "–",
                                                    simplify = T)[, 2])) %>%
            filter(!is.na(range) & range != "—") %>%
            mutate(range_max = ifelse(is.na(range_max), range_min, range_max)) %>%
            mutate(value = map2(range_min, range_max, seq)) %>%
            unnest(value) %>%
            filter(value == die_roll()$roll_2)
        }
      }
    }
  })
  output$outcome2 <- renderDataTable({
    tryCatch({
      if(unique(batting_team()$team) == "A"){
        if((batter_index_a() == 0 & floor(lineup_counter_a() == 0)) |
           input$btnLabel == "Change Sides"){
          tibble()
        } else{
          datatable(
            tibble(outcome = c("so_range", "gb_range", "fb_range", "bb_range",
                               "single_range", "single_plus_range", "double_range",
                               "triple_range", "hr_range", "pu_range")) %>%
              left_join(batter() %>%
                          pivot_longer(cols = so_range:pu_range,
                                       names_to = "outcome",
                                       values_to = "batter_range") %>%
                          select(outcome, batter_range)) %>%
              left_join(pitcher() %>%
                          pivot_longer(cols = so_range:pu_range,
                                       names_to = "outcome",
                                       values_to = "pitcher_range") %>%
                          select(outcome, pitcher_range)),
            options = list(dom = 't')) %>%
            formatStyle(columns = ifelse(pitch_winner() == "pitcher", 3, 2),
                        valueColumns = 1, 
                        backgroundColor = styleEqual(outcome()$outcome, "#c8e1cc"))
        }
      } else{
        if((batter_index_b() == 0 & floor(lineup_counter_b() == 0)) |
           input$btnLabel == "Change Sides"){
          tibble()
        } else{
          datatable(
            tibble(outcome = c("so_range", "gb_range", "fb_range", "bb_range",
                               "single_range", "single_plus_range", "double_range",
                               "triple_range", "hr_range", "pu_range")) %>%
              left_join(batter() %>%
                          pivot_longer(cols = so_range:pu_range,
                                       names_to = "outcome",
                                       values_to = "batter_range") %>%
                          select(outcome, batter_range)) %>%
              left_join(pitcher() %>%
                          pivot_longer(cols = so_range:pu_range,
                                       names_to = "outcome",
                                       values_to = "pitcher_range") %>%
                          select(outcome, pitcher_range)),
            options = list(dom = 't')) %>%
            formatStyle(columns = ifelse(pitch_winner() == "pitcher", 3, 2),
                        valueColumns = 1, 
                        backgroundColor = styleEqual(outcome()$outcome, "#c8e1cc"))
        }
      }
    },
    
    error = function(e){
      tibble()
    })
  })
  
  observeEvent(input$die_roll, {total_outs(
    ifelse(outcome()$outcome %in% out_outcomes, total_outs() + 1,
           total_outs()))})
  observeEvent(input$die_roll, {outs(ifelse(
    outcome()$outcome %in% out_outcomes, outs() + 1, outs()))})
  observeEvent(input$die_roll, {outs(ifelse(
    input$btnLabel == "Change Sides", 0, outs()))})
  observeEvent(input$die_roll, {innings(ifelse(
    input$btnLabel == "Change Sides", innings() + .5, innings()))})
  
  observeEvent(input$die_roll, {
    if (outs() == 3) {
      updateActionButton(session, "die_roll", "Change Sides")
    } else {
      updateActionButton(session, "die_roll", "Roll Die")
    }
  })
  
  outcome_tracker <- reactiveVal({tibble(player = character(),
                                         team = character(),
                                         outcome = character(),
                                         inning = integer())})
  
  diamond_tracker <- reactiveVal({tibble(base = c(1, 2, 3))})
  
  observeEvent(input$die_roll, {
    if(input$btnLabel != "Change Sides") {
      outcome_tracker() %>%
        add_row(player = batter()$player,
                team = batter()$team,
                outcome = outcome()$outcome,
                inning = innings()) %>%
        outcome_tracker()}
    })
  observeEvent(input$die_roll, {
    outcome_tracker() %>%
      mutate(value = ifelse(
        outcome %in% c("bb_range", "single_range", "single_plus_range"), 1,
        ifelse(outcome == "double_range", 2,
               ifelse(outcome == "triple_range", 3,
                      ifelse(outcome == "hr_range", 4, 0))))) %>%
      outcome_tracker()
  })
  
  output$outcome_movement <- DT::renderDataTable(
    DT::datatable(outcome_tracker() %>%
                    filter(inning == innings()) %>%
                    mutate(index = row_number()) %>%
                    arrange(-index) %>%
                    mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
                    arrange(index) %>%
                    mutate(new_value = ifelse(new_value >= 4, 4, new_value))))
  
  total_innings <- tibble(team = rep(c("A", "B"), 9), inning = rep(c(1:9), 2))
  output$scoreboard <- DT::renderDataTable(
    tryCatch({
      DT::datatable(outcome_tracker() %>%
                      group_by(team, inning = floor(inning)) %>%
                      mutate(index = row_number()) %>%
                      arrange(inning, -index) %>%
                      mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
                      arrange(inning, index) %>%
                      mutate(new_value = ifelse(new_value >= 4, 4, new_value)) %>%
                      summarize(score = sum(case_when(new_value == 4 ~ 1,
                                                      .default = 0))) %>%
                      right_join(total_innings) %>%
                      pivot_wider(names_from = inning, values_from = score))
    },
    error = function(e){
      tibble(team = rep(c("A", "B"), 9), inning = rep(c(1:9), 2),
             score = "") %>%
        arrange(inning, team) %>%
        pivot_wider(names_from = inning, values_from = score)
    })
  )
  
  
  output$diamond_plot <- renderPlotly({
    a <- outcome_tracker() %>%
      filter(inning == innings()) %>%
      mutate(index = row_number()) %>%
      arrange(-index) %>%
      mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
      arrange(index) %>%
      mutate(new_value = ifelse(new_value >= 4, 4, new_value)) %>%
      inner_join(base_plot, by = c("new_value" = "value"))
    
    
    tryCatch({
      diamond <- a %>%
        ggplot(aes(x = x, y = y, text = paste0("Player: ", player, "\n",
                                               "Team: ", team))) +
        geom_point(size = 7, pch = 21, fill = "lightblue", col = "black") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(x = "", y = "") +
        theme(axis.text = element_blank())
    },
    error = function(e){
      diamond <- tibble(x = 100, y = 100) %>%
        ggplot(aes(x = x, y = y)) +
        geom_point(size = 7, pch = 21, fill = "lightblue", col = "black") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(x = "", y = "") +
        theme(axis.text = element_blank())
    })

    
    ggplotly(diamond, tooltip = "text") %>% 
      layout(images = list(
        list(source =  base64enc::dataURI(file = "www/diamond.png"),
             xref = "x", yref = "y",
             x = -1.1, y = 1.1,
             sizex = 2.2, sizey = 2.2,
             sizing = "stretch", opacity = 0.8,
             layer = "below")))
  })
  
  
  # Creating images that will show up in the game page
  output$game_image_a <- renderUI({
    tryCatch({
      if(input$btnLabel == "Change Sides") {
        tags$image(src = str_replace(
          batting_team() %>% .[0, ] %>% pull(card_path),
          "card_images/", ""), width = "298px", height = "415px")
      } else {
        tags$image(src = str_replace(
          batter() %>% pull(card_path),
          "card_images/", ""), width = "298px", height = "415px")}},
      error = function(e){
        tags$image(src = str_replace(
          batting_team() %>% .[0, ] %>% pull(card_path),
          "card_images/", ""), width = "298px", height = "415px")
      })
    })
  output$game_image_b <- renderUI({
    tags$image(src = str_replace(
      pitcher() %>% pull(card_path),
      "card_images/", ""), width = "298px", height = "415px")
  })
  
  
  
}

shinyApp(ui = ui, server = server)