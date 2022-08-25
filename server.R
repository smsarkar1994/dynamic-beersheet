library(dplyr)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)
library(shiny)
library(googlesheets4)

server <- function(input, output) {
  observeEvent(input$auth, {
    # gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
    # options(gargle_oauth_cache = ".secrets")
    # 
    # gs4_auth(cache = ".secrets", email = input$email)
    # Authenticate using token. If no browser opens, the authentication works.
    # gs4_auth(email = input$email)
    # options(
    #   # gargle_oauth_cache = ".secrets",
    #   gargle_oauth_email = input$email
    # )
    # gs4_auth(token = input$email)
    output$test <- renderText({
      list.files(getwd())
    })
  })
  
  bs <- reactive({
    req(input$file1)
    userinfo <- read_excel(input$file1$datapath, sheet = "metadata")
    bs <- read_excel(input$file1$datapath, sheet = "BeerSheet_csv_updated")
    
    return(bs)
  })
  
  userinfo <- reactive({
    req(input$file1)
    userinfo <- read_excel(input$file1$datapath, sheet = "metadata")
    
    userinfo$draft_url[1] = gsub("https://sleeper.com/draft/nfl/", "", userinfo$draft_url[1])
    return(userinfo)
  })
  
  observeEvent(input$do, {
    req(input$file1)
    draft_id = userinfo()$draft_url[1]
    user_id = userinfo()$sleeper_userid[1]
    g_url = userinfo()$googlesheet_url[1]
    
    url <- glue("https://api.sleeper.app/v1/draft/{draft_id}/picks")
    
    draft <- GET(url) %>%
      content()
    
    m1 <- draft %>%
      map_df(magrittr::extract, c("round", "roster_id", "player_id", "picked_by",
                                  "pick_no", "draft_slot"))
    
    m2 <- draft %>%
      map("metadata") %>%
      map_df(magrittr::extract, c("player_id", "first_name", "last_name", "years_exp", 
                                  "team", "status", "position", "number",
                                  "injury_status"))
    
    dat <- left_join(m1, m2, by = "player_id")
    
    dat <- dat %>%
      mutate(key = paste0(first_name, " ", last_name, team))
    
    bs <- bs() %>%
      mutate(`drafted?` = ifelse(bs()$key %in% dat$key, 1, 0))
    
    
    write_sheet(bs, ss = g_url, sheet = "BeerSheet_csv_updated")
    
    output$draft_id <- renderText({
      HTML(paste0("<b>Draft ID: </b>", draft_id))
    })
    
    output$user_id <- renderText({
      paste0("<b>Sleeper User ID: </b>", user_id)
    })
    
    output$last_pick <- renderText({
      paste0("<b>Last Updated Pick: </b>", max(dat$pick_no))
    })
    
    output$status <- renderText({
      paste0("<b>Last Updated: </b>", Sys.time())
    })
  })
  

  
  
}
