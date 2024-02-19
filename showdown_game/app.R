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


##### Random Game Objects #####

# Setting up a tibble that will be used for a base plot
# These are coordinates for points that will be overlayed on a baseball diamond
base_plot <- tibble(value = c(1, 2, 3),
                    x = c(.675, .01, -.675),
                    y = c(.01, .7, .01))

# Identifying outcomes that are outs
out_outcomes <- c("so_range", "gb_range", "fb_range", "pu_range")


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
      fluidRow(column(4, orderInput(inputId = "lineup_order_a",
                                    label = "Team-A Lineup",
                                    items = team_a_batters$player)),
               column(4, actionButton(inputId = "set_lineup_a",
                                      label = "Set Team-A Lineup"),
                      align = "center", style = "margin-top: 40px;"),
               column(4, actionButton(inputId = "autoset_lineup_a",
                                      label = "Auto Set Team-A Lineup"),
                      align = "center", style = "margin-top: 40px;")),
      
      br(),
      
      fluidRow(column(12, dataTableOutput(outputId = "test"))),
      
      # Creating a row showing roster for team a
      fluidRow(column(4, fluidRow(
        column(12, dataTableOutput(outputId = "team_a_batters")),
        column(12, dataTableOutput(outputId = "team_a_pitchers"),
               div(style = "height:10px;")))),
        column(4, div(uiOutput(outputId = "lineup_image_a",
                               align = "center"))),
        column(4, div(uiOutput(outputId = "rotation_image_a",
                               align = "center")))),
      
      br(),
      
      # Creating a row of lineup setting actions for team b
      fluidRow(column(4, orderInput(inputId = "lineup_order_b",
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
          column(12, dataTableOutput(outputId = "team_b_batters")),
          column(12, dataTableOutput(outputId = "team_b_pitchers"),
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
      
      conditionalPanel(
        condition = "(input.inning_input == floor(output.current_inning))"
      ),
      conditionalPanel(
        condition = "(input.inning_input != floor(output.current_inning))",
        # Creating row for rolling die and seeing output
        # This row will also contain the scoreboard
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
                                        div(style = "height:10px;")))),
                 column(4, dataTableOutput(outputId = "scoreboard")),
                 column(1, fluidRow(column(12, align = "center",
                                           p(strong("Inning")),
                                           style = "font-size:17px; margin-top: 20px;",
                                           div(style = "height:10px;"))),
                        fluidRow(column(12, align = "center",
                                        textOutput(outputId = "current_inning"),
                                        style = "font-size:16px;",
                                        div(style = "height:10px;")))),
                 column(1, fluidRow(column(12, align = "center",
                                           p(strong("Outs")),
                                           style = "font-size:17px; margin-top: 20px;",
                                           div(style = "height:10px;"))),
                        fluidRow(column(12, align = "center",
                                        textOutput(outputId = "current_outs"),
                                        style = "font-size:16px;",
                                        div(style = "height:10px;"))))),
        
        br(),
        
        # Creating row showing batter v pitcher and game outcome
        fluidRow(column(6, fluidRow(
          column(12, dataTableOutput(outputId = "pitch_outcome"))),
          fluidRow(column(12, plotlyOutput(outputId = "diamond_plot")))),
          column(3, fluidRow(
            column(12, style = "height:100px;"),
            column(12, div(uiOutput(outputId = "game_image_b",
                                    align = "center"))))),
          column(3, fluidRow(
            column(12, style = "height:100px;"),
            column(12, div(uiOutput(outputId = "game_image_a",
                                    align = "center")))))),
        
        
        br(),
        
        # Creating row tracking player movement through inning
        # TODO: Possibly create new tab for this and other stats
        fluidRow(dataTableOutput(outputId = "game_summary"))
      )),
    
    
    ##### Stat Panel #####
    
    # We'll add an additional panel to track game stats
    tabPanel(
      title = "stats",
      
      fluidRow(dataTableOutput(outputId = "game_log")))
    )
  )


#### Server ####

server <- function(input, output, session) {
  
  ##### Setting Lineups #####
  
  ###### Reactive Input ######
  
  # We'll set up a reactive lineup input
  # This will take our lineup input and rearrange our team based on it
  # If you auto-set the lineup, it'll be ordered on player points
  
  # Creating a reactive lineup tibble for team a
  team_a_lineup <- reactiveValues(
    set_lineup = team_a_batters)
  
  # Observing if user presses autoset lineup for team a
  observeEvent(eventExpr = input$autoset_lineup_a, {
    # Changing reactive lineup to be arranged by points
    team_a_lineup$set_lineup <- team_a_batters %>%
      arrange(desc(points))
    
    # Changing set lineup input to match the new lineup order
    updateOrderInput(session = session, inputId = "lineup_order_a",
                     items = team_a_lineup$set_lineup$player)})
  
  # Observing if user presses set lineup for team a
  observeEvent(eventExpr = input$set_lineup_a, {
    # Changing reactive lineup to be arranged by set lineup input
    team_a_lineup$set_lineup <- team_a_batters %>%
      arrange(match(player, input$lineup_order_a))})

  ###### Table Output ######
  
  # Creating dataframe to show team a lineup, ordered by our input order
  output$team_a_batters <- renderDataTable({
    datatable(team_a_lineup$set_lineup %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't', ordering = F))})
  
  # Creating a reactive lineup tibble for team b
  team_b_lineup <- reactiveValues(
    set_lineup = team_b_batters)
  
  # Observing if user presses autoset lineup for team b
  observeEvent(eventExpr = input$autoset_lineup_b, {
    # Changing reactive lineup to be arranged by points
    team_b_lineup$set_lineup <- team_b_batters %>%
      arrange(desc(points))
    
    # Changing set lineup input to match the new lineup order
    updateOrderInput(session = session, inputId = "lineup_order_b",
                     items = team_b_lineup$set_lineup$player)})
  
  # Observing if user presses set lineup for team b
  observeEvent(eventExpr = input$set_lineup_b, {
    # Changing reactive lineup to be arranged by set lineup input
    team_b_lineup$set_lineup <- team_b_batters %>%
      arrange(match(player, input$lineup_order_b))})
  
  # Creating dataframe to show team b lineup, ordered by our input order
  output$team_b_batters <- renderDataTable({
    datatable(team_b_lineup$set_lineup %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't', ordering = F))})
  
  # We'll also have a pitcher dataframe for each team
  # There won't be anything to set here, though
  
  # Creating dataframe to show team a pitchers
  output$team_a_pitchers <- renderDataTable({
    datatable(team_a_pitchers %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't', ordering = F))
  })
  
  # Creating dataframe to show team b pitchers
  output$team_b_pitchers <- renderDataTable({
    datatable(team_b_pitchers %>%
                select(player, year, gen_position),
              selection = list(mode = "single", selected = c(1)),
              options = list(dom = 't', ordering = F))
  })
  
  ###### Image Output ######
  
  # We'll show card images on the lineup page
  # There will be a batter and pitcher card shown
  # Image will be dynamic based on row selected on lineup output
  
  # Getting card image for selected team a batter
  output$lineup_image_a <- renderUI({
    tags$image(src = str_replace(
      team_a_lineup$set_lineup %>%
        .[input$team_a_batters_rows_selected, ] %>%
        pull(card_path),
      "showdown_game/www/", ""), width = "298px", height = "415px")
  })
  
  # Getting card image for selected team a pitcher
  output$rotation_image_a <- renderUI({
    tags$image(src = str_replace(
      team_a_pitchers %>%
        .[input$team_a_pitchers_rows_selected, ] %>%
        pull(card_path),
      "showdown_game/www/", ""), width = "298px", height = "415px")
  })
  
  # Getting card image for selected team b batter
  output$lineup_image_b <- renderUI({
    tags$image(src = str_replace(
      team_b_lineup$set_lineup %>%
        .[input$team_b_batters_rows_selected, ] %>%
        pull(card_path),
      "showdown_game/www/", ""), width = "298px", height = "415px")
  })
  
  # Getting card image for selected team b pitcher
  output$rotation_image_b <- renderUI({
    tags$image(src = str_replace(
      team_b_pitchers %>%
        .[input$team_b_pitchers_rows_selected, ] %>%
        pull(card_path),
      "showdown_game/www/", ""), width = "298px", height = "415px")
  })
  
  
  ##### Game Page #####
  
  # Setting reactive values to track total outs, current outs, and innings
  total_outs <- reactiveVal(0)
  outs <- reactiveVal(0)
  innings <- reactiveVal(1)
  
  # Identifying batting and pitching teams
  # Based on if inning is a whole number or not
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
  
  # Die roll counter sum up how often the die is rolled
  die_roll_counter_a <- reactiveVal(0)
  die_roll_counter_b <- reactiveVal(0)
  # Lineup counter sum up how many times we go through the lineup
  lineup_counter_a <- reactiveVal(0)
  lineup_counter_b <- reactiveVal(0)
  # Die roll values track possible values for the first and second roll
  die_roll_value_1 <- reactiveVal(0)
  die_roll_value_2 <- reactiveVal(0)
  
  # Simulating two 20-side die rolls
  # First is addition to pitcher control
  # Second is outcome roll
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
  
  # Storing results in a tibble
  die_roll <- reactive({
    tibble(roll_1 = die_roll_value_1(),
           roll_2 = die_roll_value_2())
  })
  
  # Die roll output gets presented in game tab
  output$die_roll_1 <- renderText({die_roll()$roll_1})
  output$die_roll_2 <- renderText({die_roll()$roll_2})
  
  # Counting the number of times a die is rolled while team is batting
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 ifelse(batting_team()$team == "A",
                        die_roll_counter_a(die_roll_counter_a() + 1),
                        die_roll_counter_b(die_roll_counter_b() + 1))}})
  # Counting how far we go through the lineup each die roll
  observeEvent(input$die_roll,
               {if(input$btnLabel != "Change Sides") {
                 ifelse(batting_team()$team == "A",
                        lineup_counter_a(lineup_counter_a() + (1 / 9)),
                        lineup_counter_b(lineup_counter_b() + (1 / 9)))}})
  
  # Use die roll counter and lineup counter to identify which batter is up
  batter_index_a <- renderText({
    if(unique(batting_team()$team) == "A"){
      die_roll_counter_a() - (9 * floor(lineup_counter_a()))}
    })
  batter_index_b <- renderText({
    if(unique(batting_team()$team) == "B"){
      die_roll_counter_b() - (9 * floor(lineup_counter_b()))}
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
  
  # Determining what the outcome of the pitch will be
  outcome_pitcher <- reactive({
    pitcher() %>%
      pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                   values_to = "range") %>%
      select(player, year, outcome, range) %>%
      mutate(range_min = as.numeric(str_split(range, "–",
                                              simplify = T)[, 1]),
             range_max = as.numeric(str_split(range, "–",
                                              simplify = T)[, 2])) %>%
      filter(!is.na(range) & range != "—") %>%
      mutate(range_max = ifelse(is.na(range_max), range_min,
                                range_max)) %>%
      mutate(value = map2(range_min, range_max, seq)) %>%
      unnest(value) %>%
      filter(value == die_roll()$roll_2)})
  
  outcome_batter <- reactive({
    batter() %>%
      pivot_longer(cols = so_range:pu_range, names_to = "outcome",
                   values_to = "range") %>%
      select(player, year, outcome, range) %>%
      mutate(range_min = as.numeric(str_split(range, "–",
                                              simplify = T)[, 1]),
             range_max = as.numeric(str_split(range, "–",
                                              simplify = T)[, 2])) %>%
      filter(!is.na(range) & range != "—") %>%
      mutate(range_max = ifelse(is.na(range_max), range_min,
                                range_max)) %>%
      mutate(value = map2(range_min, range_max, seq)) %>%
      unnest(value) %>%
      filter(value == die_roll()$roll_2)})
  
  outcome <- reactive({
    if(pitch_winner() == "pitcher"){
      outcome_pitcher()
      } else{
        outcome_batter()}})
  
  output$pitch_outcome <- renderDataTable({
    tryCatch({
      if(input$btnLabel == "Change Sides"){
        datatable(
          tibble(metric = c("player", "command", "output", "winner"),
                 batter = NA,
                 pitcher = NA),
          options = list(dom = 't', ordering = F))
      } else {
        datatable(
          tibble(metric = c("player", "command", "output", "winner"),
                 batter = c(batter()$player, batter()$command,
                            batter()$command,
                            ifelse(pitch_winner() == "pitcher", 0, 1)),
                 pitcher = c(pitcher()$player, pitcher()$command,
                             pitcher()$command + die_roll()$roll_1,
                             ifelse(pitch_winner() == "pitcher", 1, 0))) %>%
            bind_rows(outcome_pitcher() %>%
                        mutate(batter_pitcher = "pitcher") %>%
                        bind_rows(outcome_batter() %>%
                                    mutate(batter_pitcher = "batter")) %>%
                        mutate(metric = "outcome") %>%
                        select(metric, batter_pitcher,
                               outcome) %>%
                        pivot_wider(names_from = batter_pitcher,
                                    values_from = outcome)),
          options = list(dom = 't', ordering = F)) %>%
          formatStyle(columns = ifelse(pitch_winner() == "pitcher", 3, 2),
                      valueColumns = 1,
                      backgroundColor = styleEqual("outcome", "#c8e1cc"))
      }},
    error = function(e){
      datatable(
        tibble(metric = c("player", "command", "output", "winner"),
               batter = NA,
               pitcher = NA),
        options = list(dom = 't', ordering = F))
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
  
  output$current_outs <- renderText({outs()})
  
  output$current_inning <- renderText({innings()})
  outputOptions(output, "current_inning", suspendWhenHidden = F)
  
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
  
  output$game_log <- renderDataTable(
    datatable(outcome_tracker() %>%
                    filter(inning == innings()) %>%
                    mutate(index = row_number()) %>%
                    arrange(-index) %>%
                    mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
                    arrange(index) %>%
                    mutate(new_value = ifelse(new_value >= 4, 4, new_value))))
  
  total_innings <- tibble(team = rep(c("A", "B"), 9), inning = rep(c(1:9), 2))
  
  score_df <- reactive({
    outcome_tracker() %>%
      group_by(team, inning = floor(inning)) %>%
      mutate(index = row_number()) %>%
      arrange(inning, -index) %>%
      mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
      arrange(inning, index) %>%
      mutate(new_value = ifelse(new_value >= 4, 4, new_value)) %>%
      summarize(score = sum(case_when(new_value == 4 ~ 1,
                                      .default = 0)))})
  
  total_score <- reactive({
    score_df() %>%
      group_by(team) %>%
      summarize(score = sum(score))
  })
  output$team_a_score <- renderText({
    total_score() %>%
      filter(team == "A") %>%
      pull(score)})
  outputOptions(output, "team_a_score", suspendWhenHidden = F)
  output$team_b_score <- renderText({
    total_score() %>%
      filter(team == "B") %>%
      pull(score)})
  outputOptions(output, "team_b_score", suspendWhenHidden = F)
  output$in_lead <- renderText({
    total_score() %>%
      mutate(max_score = max(score)) %>%
      filter(max_score == score) %>%
      pull(team)
  })
  
  output$scoreboard <- renderDataTable(
    tryCatch({
      datatable(score_df() %>%
                  right_join(total_innings) %>%
                  pivot_wider(names_from = inning,
                              values_from = score) %>%
                  left_join(total_score()) %>%
                  mutate(score = ifelse(is.na(score), 0, score)),
                options = list(dom = 't', ordering = F))
    },
    error = function(e){
      datatable(tibble(team = rep(c("A", "B"), 9),
                       inning = rep(c(1:9), 2),
                       score = "") %>%
                  arrange(inning, team) %>%
                  pivot_wider(names_from = inning,
                              values_from = score) %>%
                  mutate(score = c(0, 0)),
                options = list(dom = 't', ordering = F))
    })
  )
  
  
  output$diamond_plot <- renderPlotly({
    tryCatch({
      a <- outcome_tracker() %>%
        filter(inning == innings()) %>%
        mutate(index = row_number()) %>%
        arrange(-index) %>%
        mutate(new_value = ifelse(value == 0, 0, cumsum(value))) %>%
        arrange(index) %>%
        mutate(new_value = ifelse(new_value >= 4, 4, new_value)) %>%
        inner_join(base_plot, by = c("new_value" = "value"))
      
      diamond <- a %>%
        ggplot(aes(x = x, y = y, text = paste0("Player: ", player, "\n",
                                               "Team: ", team))) +
        geom_point(size = 7, pch = 21, fill = "lightblue", col = "black") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(x = "", y = "") +
        theme(axis.text = element_blank())
      
      ggplotly(diamond, tooltip = "text") %>% 
        layout(images = list(
          list(source =  base64enc::dataURI(file = "www/diamond.png"),
               xref = "x", yref = "y",
               x = -1.1, y = 1.1,
               sizex = 2.2, sizey = 2.2,
               sizing = "stretch", opacity = 0.8,
               layer = "below")))
    },
    error = function(e){
      diamond <- tibble(x = 100, y = 100) %>%
        ggplot(aes(x = x, y = y)) +
        geom_point(size = 7, pch = 21, fill = "lightblue", col = "black") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(x = "", y = "") +
        theme(axis.text = element_blank())
      
      ggplotly(diamond, tooltip = "text") %>% 
        layout(images = list(
          list(source =  base64enc::dataURI(file = "www/diamond.png"),
               xref = "x", yref = "y",
               x = -1.1, y = 1.1,
               sizex = 2.2, sizey = 2.2,
               sizing = "stretch", opacity = 0.8,
               layer = "below")))
    })
  })
  
  
  # Creating images that will show up in the game page
  output$game_image_a <- renderUI({
    tryCatch({
      if(input$btnLabel == "Change Sides") {
        tags$image(src = str_replace(
          batting_team() %>% .[0, ] %>% pull(card_path),
          "showdown_game/www/", ""), width = "298px", height = "415px")
      } else {
        tags$image(src = str_replace(
          batter() %>% pull(card_path),
          "showdown_game/www/", ""), width = "298px", height = "415px")}},
      error = function(e){
        tags$image(src = str_replace(
          batting_team() %>% .[0, ] %>% pull(card_path),
          "showdown_game/www/", ""), width = "298px", height = "415px")
      })
    })
  output$game_image_b <- renderUI({
    tags$image(src = str_replace(
      pitcher() %>% pull(card_path),
      "showdown_game/www/", ""), width = "298px", height = "415px")
  })
  
  
  
}

shinyApp(ui = ui, server = server)