library(pacman)
pacman::p_load(shiny,
               tidyverse,
               forstringr,
               plotly,
               lubridate,
               bslib,
               bsicons,
               shinyjs,
               DT,
               digest,
               googlesheets4,
               DBI,
               RSQLite,
               shinybusy)


con <- dbConnect(RSQLite::SQLite(), "")
sheet_ID <- '1PK5nDP-xsz9165HUxEMZFN8p5v9FYIiOJYK5KEfHc-g'
sheet_names <- sheet_names(sheet_ID)

for (sheet_name in sheet_names) {
  dbWriteTable(con, 
               sheet_name,
               read_sheet(ss = sheet_ID,
                          sheet = sheet_name),
               overwrite = TRUE)
}

weight_choices <- dbGetQuery(
  con,
  statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name
            FROM ATHLETES 
            WHERE Athlete_ID IN (SELECT DISTINCT Athlete_ID FROM WEIGHT)
            ORDER BY NAME'
)

first_weight_select <- head(weight_choices, 1)