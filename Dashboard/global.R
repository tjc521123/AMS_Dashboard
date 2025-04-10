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

dbExecute(
  conn = con,
  statement = 'CREATE TABLE IF NOT EXISTS ATHLETES (
                Athlete_ID INTEGER AUTO_INCREMENT UNIQUE,
                Athlete_FirstName TEXT,
                Athlete_LastName  TEXT,
                Athlete_DOB       DATE,
                Athlete_Gender    BOOL,
                PRIMARY KEY (Athlete_ID)
               )'
)

dbExecute(
  conn = con,
  statement = 'CREATE TABLE IF NOT EXISTS WEIGHT (
                Athlete_ID INTEGER,
                Date       DATE,
                Weight     REAL,
                Body_Comp  REAL,
                PRIMARY KEY (Athlete_ID, Date),
                FOREIGN KEY (Athlete_ID) REFERENCES ATHLETES(Athlete_ID)
               )'
)

dbExecute(
  conn = con,
  statement = 'CREATE TABLE IF NOT EXISTS LIFTS (
                Lift_ID INTEGER AUTO_INCREMENT UNIQUE,
                LiftName TEXT,
                Main_Lift TEXT,
                Pattern TEXT,
                Scaling REAL,
                Equipment TEXT,
                Category TEXT,
                PRIMARY KEY (Lift_ID)
               )'
)

dbExecute(
  conn = con,
  statement = 'CREATE TABLE IF NOT EXISTS SESSION (
                Athlete_ID INTEGER,
                Session_Date DATE,
                Lift_ID INTEGER,
                Session_Set INTEGER,
                Reps INTEGER,
                Weight REAL,
                RPE REAL,
                PRIMARY KEY (Athlete_ID, Session_Date, Lift_ID, Session_Set),
                FOREIGN KEY (Athlete_ID) REFERENCES ATHLETES(Athlete_ID),
                FOREIGN KEY (Lift_ID) REFERENCES LIFTS(Lift_ID)
               )')

dbExecute(
  conn = con,
  statement = 'CREATE TABLE IF NOT EXISTS MAXES (
                Athlete_ID INTEGER,
                Lift_ID    INTEGER,
                Lift_Max   REAL,
                PRIMARY KEY (Athlete_ID, Lift_ID),
                FOREIGN KEY (Athlete_ID) REFERENCES ATHLETES(Athlete_ID),
                FOREIGN KEY (Lift_ID) REFERENCES LIFTS(Lift_ID)
               )'
)

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

metric_choices <- c('Stress Index', 
                    'Tonnage', 
                    'Exertion Load', 
                    'Stress Index', 
                    'Rel Vol Load') %>% unique() %>% sort()

first_weight_select <- head(weight_choices, 1)

sql <- 'CREATE TEMPORARY VIEW vw_Load_Metrics AS 
        SELECT Athlete_ID, Session_Date, Lift_ID, Session_Set, Reps, Weight, RPE,
               RIR, Perc_1RM, Stress_Index, Lift_Tonnage, Tonnage, Set_Exertion_Load,
               SUM(Set_Exertion_Load) OVER (
                PARTITION BY Session_Date, Athlete_ID, Lift_ID
               ) AS Exertion_Load,
               SUM(Stress_Index) OVER (
                PARTITION BY Session_Date, Athlete_ID, Lift_ID
               ) AS Lift_Stress_Index,
               SUM(Stress_Index) OVER (
                PARTITION BY Session_Date, Athlete_ID
               ) AS Stress_Index,
               SUM(Set_Rel_Vol_Load) OVER (
                PARTITION BY Session_Date, Athlete_ID, Lift_ID
               ) AS Lift_Rel_Vol_Load,
               SUM(Set_Rel_Vol_Load) OVER (
                PARTITION BY Session_Date, Athlete_ID
               ) AS Rel_Vol_Load
        FROM   (WITH RECURSIVE cte(x, e, s) AS (
                  SELECT 1, 1, 1
                  UNION ALL
                  SELECT x+1, exp(x), exp(x - 1) + exp(x) FROM cte WHERE x<10
                )
                SELECT Athlete_ID, Session_Date, Lift_ID, Session_Set, Reps, Weight, RPE, Set_Tonnage,
                       RIR, Perc_1RM, (Perc_1RM * Reps * (1 + (1 / (RIR + 1)))) AS Stress_Index,
                       SUM(Set_Tonnage) OVER (
                        PARTITION BY Session_Date, Athlete_ID, Lift_ID
                       ) AS Lift_Tonnage,
                       SUM(Set_Tonnage) OVER (
                        PARTITION BY Session_Date, Athlete_ID
                       ) AS Tonnage,
                       (Perc_1RM * Reps) AS Set_Rel_Vol_Load,
                       SUM(Weight * exp(-0.215 * RIR) * exp(Reps) / s) OVER (
                        PARTITION BY Session_Date, Athlete_ID, Lift_ID, Session_Set
                       ) AS Set_Exertion_Load
               FROM (SELECT Athlete_ID, Session_Date, Lift_ID, Session_Set, Reps, Weight, RPE,
                    (10 - RPE) AS RIR, (1 / (1 + 0.0333 * (Reps + 10 - RPE))) AS Perc_1RM,
                    (Reps * Weight) AS Set_Tonnage
                    FROM SESSION
                    GROUP BY Athlete_ID, Session_Date, Lift_ID, Session_Set)
               INNER JOIN cte ON
                    x = Reps)'

dbExecute(con,
          statement = sql)

# dbGetQuery(con,
#            statement = 'SELECT * FROM vw_Load_Metric') %>%
#   View()





















