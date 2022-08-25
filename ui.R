library(dplyr)
library(rvest)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(readxl)
library(shiny)
library(googlesheets4)
library(formattable)
library(DT)

ui <- fluidPage(
  
  # App title ----
  titlePanel(
    h1("Dynamic BeerSheet"),
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(position = "left",
                 # h4("Authenticate Google Account"),
                 # textInput("email", "Email"),
                 # actionButton("auth", "Authenticate", class = "btn-primary"),
                 # textOutput("test"),
                 # br(),
                 # br(),
                 h4("Download Custom BeerSheet Template"),
                 p(a(href = "https://www.dropbox.com/s/5umw1xs051ilqxo/custom_beersheet_template.xlsx?dl=0",
                     'Click here for Dropbox download link.',
                     target="_blank")),
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
                         p("1) Download the", strong("Custom BeerSheets Template"), "from the 
                           panel on the left."),
                         p("2) Go to the ", a(href = "https://footballabsurdity.com/beersheet-request-form/", "BeerSheets site"),
                           "and request a BeerSheet based on your league settings."), 
                         p("3) Download ", strong("BOTH"), "the .csv and .xlsx files from BeerSheets."),
                         p('4) Paste the .csv file into the "BeerSheet_csv_raw" tab and the .xlsx file 
                           into the "Beersheet_xlsx_raw" tab of the template. Take care to', strong("completely overwrite"),
                           "anything in those tabs. I recommend copy and pasting the entire sheet from the source."),
                         p("5) Navigate to", a(href = "https://drive.google.com/drive/my-drive", "Google Drive")),
                         p("6) In the new Google Sheet, upload the Formatted Custom BeerSheet by 
                           navigating to File -> Import -> Upload and selecting the new excel file."),
                         p("7) Edit the Google Sheet permissions so anyone can edit it. You can do so by clicking the green \"Share\"
                           button on the top right -> click \"Save\" -> under \"General Access\", 
                           change it so \"Anyone with the link\" is an editor", strong("(not just a viewer)."),
                           "The app WILL NOT work if you don't do this step."),
                         p('8) In the "metadata" tab of the', strong("Excel file (not the Google Sheet)"),
                           "update the 3 fields with a link to your Sleeper draft room, your Sleeper usename, 
                           and a link to the Google Sheet you just created."),
                         p("9) You're ready to go! Click the \"Update BeerSheet\"
                           button whenever you want to update the Google Sheet
                           with your draft progress.")
                         
                )
              )
              
    )
  )
)