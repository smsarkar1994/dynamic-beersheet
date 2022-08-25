library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)
library(shiny)
library(googlesheets4)

ui <- fluidPage(
  
  # App title ----
  titlePanel(
    h1("Dynamic BeerSheet"),
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(position = "left",
                 h4("Authenticate Google Account"),
                 textInput("email", "Email"),
                 actionButton("auth", "Authenticate", class = "btn-primary"),
                 textOutput("test"),
                 br(),
                 br(),
                 h4("Download Custom BeerSheet Template"),
                 p(a(href = "https://www.dropbox.com/s/5umw1xs051ilqxo/custom_beersheet_template.xlsx?dl=0",
                     'Right click and press "Open Link in Browser" to download.')),
                 br(),
                 h4("Upload File"),
                 fileInput("file1", "Choose Custom BeerSheet File (see instructions)",
                           multiple = TRUE),
                 br(),
                 actionButton("do", "Update BeerSheet", class = "btn-primary"),
                 htmlOutput("draft_id"),
                 htmlOutput("user_id"),
                 htmlOutput("last_pick"),
                 htmlOutput("status")
                 # accept = c("text/csv",
                 #            "text/comma-separated-values,text/plain",
                 #            ".csv")),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(position = "right",
              tabsetPanel(
                tabPanel("Description",
                         h3("Description"),
                         p("Welcome to my Dynamic BeerSheet App. Drafting in 
                         Fantasy is a frantic and stressful process. Luckily, there are 
                         lots of great draft kits out there to help us out -
                         in particular the fantastic BeerSheets created by Reddit
                         user /u/BeerSheets."),
                         p("However, it can be tough to simultaneously juggle
                           the stress of the draft and update your BeerSheets
                           as players are being taken off the board. This is especially
                           hard if you have tight draft windows (<30s) or do in-person
                           drafts where you can be distracted by the antics of your leaguemates"),
                         p("Enter my", strong("Dynamic BeerSheets App."), "This app is designed to 
                           integrate with your draft and", strong("dynamically update your BeerSheet 
                                                                   to show who's been drafted and who's still left.")),
                         p("Note that this app currently
                         only works for", strong("Sleeper Leagues."), "If there is 
                         sufficient interest, I will look into generalizing this app for 
                           ESPN and Yahoo Leagues."),
                         p("This app requires the following:"),
                         p("1) A Google Account (the dynamic spreadsheet will be a 
                           Google Sheet)."),
                         p("2) Microsoft Excel."),
                         br(), 
                         p("Shoutout to the great folk(s) at BeerSheets and the awesome
                           work they do. As well as Sleeper for having a well-documented
                           and easy-to-use API.")
                         
                ),
                
                
                tabPanel("Instructions",
                         h3("Instructions"),
                         p("To run this app, follow these instructions:"),
                         p("1) ", strong("Authenticate your Google Account."), 
                           "To do this, enter a valid gmail address and press \"Authenticate\".
                           After signing in,", strong("be sure to check the box that allows
                                                      for the app to edit your Google Sheets spreadsheets."),
                           "If you run into errors/the app crashes at any point, it's likely because
                           you didn't authenticate the right account.", strong("You only need to authenticate once
                                                                               ever per account.")),
                         p("  Note: The app will only ever edit the Google Sheet(s) you specify in the input."),
                         br(),
                         p("2) Download the", strong("Custom BeerSheets Template"), "from the 
                           panel on the left."),
                         p("3) Go to the ", a(href = "https://footballabsurdity.com/beersheet-request-form/", "BeerSheets site"),
                           "and request a BeerSheet based on your league settings."), 
                         p("4) Download ", strong("BOTH"), "the .csv and .xlsx files from BeerSheets."),
                         p('5) Paste the .csv file into the "BeerSheet_csv_raw" tab and the .xlsx file 
                           into the "Beersheet_xlsx_raw" tab of the template. Take care to', strong("completely overwrite"),
                           "anything in those tabs. I recommend copy and pasting the entire sheet from the source."),
                         p("6) Navigate to", a(href = "https://drive.google.com/drive/my-drive", "Google Drive"),
                           "and create a new Google Sheet. Make sure this Google Drive matches
                           the gmail account you authenticated above."),
                         p("7) In the new Google Sheet, upload the Formatted Custom BeerSheet by 
                           navigating to File -> Import -> Upload"),
                         p('8) In the "metadata" tab of the', strong("Excel file (not the Google Sheet)"),
                           "update the 3 fields with a link to your Sleeper draft room, your Sleeper usename, 
                           and a link to the Google Sheet you just created."),
                         p("9) You're ready to go! Click the \"Update BeerSheet\"
                           button whenever you want to update the Google Sheet
                           with your draft progress.")
                         
                )
              )
              
              
              # tableOutput("contents")
              
    )
  )
)

server <- function(input, output) {
  observeEvent(input$auth, {
    gs4_auth(email = input$email)
    
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
  
  
  
  # output$contents <- renderTable({
  # 
  # })
  
  
  
}

shinyApp(ui = ui, server = server)