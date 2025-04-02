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
               googledrive,
               googlesheets4,
               DBI,
               RSQLite,
               shinybusy,
               toastui,
               randomcoloR,
               rsconnect)

#----------------------------------------------------------
# Authentication
#---------------------------------------------------------- 
gs4_auth(
  email = 'tjc521123@gmail.com',
  cache = '.secrets',
  scopes = 'drive'
)

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

athletes <- dbGetQuery(
  con,
  statement = 'SELECT DISTINCT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name
               FROM ATHLETES ORDER BY Name'
)

weight_choices <- dbGetQuery(
  con,
  statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name
            FROM ATHLETES 
            WHERE Athlete_ID IN (SELECT DISTINCT Athlete_ID FROM WEIGHT)
            ORDER BY NAME'
)

sql <-'SELECT DISTINCT Lift_Name
       FROM LIFTS
       WHERE Lift_ID IN (SELECT Lift_ID FROM SESSION
                         WHERE Athlete_ID IN (SELECT Athlete_ID 
                                              FROM ATHLETES
                                              WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name))'
query <- sqlInterpolate(
  con,
  sql = sql,
  name = dbGetQuery(
    con,
    statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name
                 FROM ATHLETES LIMIT 1'
  )$Name[1]
)

lift_choices <- dbGetQuery(
  con,
  statement = query
)

first_weight_select <- head(weight_choices, 1)
