library(pacman)
pacman::p_load(shiny,
               tidyverse,
               googlesheets4,
               randomNames,
               DBI)
createDatabase   <- function(con, sheet_ID) {
  sheet_names <- sheet_names(sheet_ID)
  
  for (sheet_name in sheet_names) {
    dbWriteTable(con, 
                 sheet_name,
                 read_sheet(ss = sheet_ID,
                            sheet = sheet_name))
  }
  
  print(dbListTables(con))
}

createNewAthlete <- function(con, tbl_athletes, sheet_ID) {
  new_ID        <- max(tbl_athletes$Athlete_ID) + 1
  new_Gender    <- if(round(runif(1))) 'Female' else 'Male'
  new_FirstName <- randomNames(1, 
                               gender = if(new_Gender == 'Female') 1 else 0,
                               which.names = 'first')
  new_LastName  <- randomNames(1, 
                               which.names = 'last')
  new_DOB       <- sample(seq(as.Date('1960/01/01'),
                              as.Date('2005/12/31'),
                              by = 'day'), 1) %>% as.integer()
  new_DOB       <- new_DOB * 24 * 3600
  
  sql <- 'INSERT INTO ATHLETES 
            (Athlete_ID, Athlete_FirstName, Athlete_LastName, Athlete_DOB, Athlete_Gender)
          VALUES
            (?id, ?first, ?last, ?dob, ?gender)'
  query <- sqlInterpolate(con,
                          sql    = sql,
                          id     = new_ID,
                          first  = new_FirstName,
                          last   = new_LastName,
                          dob    = new_DOB,
                          gender = new_Gender)

  dbExecute(con,
            statement = query)
  print(paste0('Created athlete: ', new_FirstName, ' ', new_LastName))
}

createNewWeight  <- function(con, sheet_ID, athlete_id, date) {
  today <- date
  
  athlete_ids <- dbGetQuery(con,
                            statement = 'SELECT DISTINCT Athlete_ID FROM ATHLETES')

  sql <- 'SELECT * FROM WEIGHT
          WHERE Athlete_Id = ?id
          ORDER BY Date DESC LIMIT 1'
  query <- sqlInterpolate(con,
                          sql = sql,
                          id  = athlete_id)
  last_weigh_in <- dbGetQuery(con,
                              statement = query)

  last_weight <- if(!is.na(last_weigh_in$Weight[1])) last_weigh_in$Weight[1] else runif(1,
                                                                                        min = 150,
                                                                                        max = 300)
  
  last_comp <- if(!is.na(last_weigh_in$Body_Comp[1])) last_weigh_in$Body_Comp[1] else runif(1, 
                                                                                            min = 0.1,
                                                                                            max = 0.5)
  
  last_weight <- last_weight + runif(1, min = -1.5, max = 1.5) %>% round(1)
  last_comp   <- last_comp + runif(1, min = -0.0075, max = 0.0075) %>% round(4)
  sql <- 'INSERT INTO WEIGHT
            (Athlete_ID, Date, Weight, Body_Comp)
          VALUES
            (?id, ?date, ?weight, ?comp)'
  query <- sqlInterpolate(con,
                          sql    = sql,
                          id     = athlete_id,
                          date   = today,
                          weight = last_weight,
                          comp   = last_comp)
  dbExecute(con,
            statement = query)
  
  print(paste0('New Weight for ID: ', athlete_id))
}

createNewSession <- function(con, sheet_ID, athlete_id, lift_ids, date) {
  today <- date
  num_lifts <- ceiling(runif(1) * 4)
  lifts     <- sample(lift_ids, num_lifts)
  
  for (lift in lifts) {
    num_sets <- round(runif(1, min = 1, max = 5))
    reps     <- round(runif(1, min = 5, max = 15))
    weight   <- round(runif(1, min = 50, max = 300), digits = 0)
    rpe      <- ceiling(round(runif(1, min = 5, max = 10), digits = 1) * 2) / 2
    
    for (set in 1:num_sets) {
      sql <- 'INSERT INTO SESSION
                (Athlete_ID, Session_Date, Lift_ID, Session_Set, Reps, Weight, RPE)
              VALUES
                (?id, ?date, ?lift, ?set, ?reps, ?weight, ?rpe)'
      query <- sqlInterpolate(con,
                              sql    = sql,
                              id     = athlete_id,
                              date   = today,
                              lift   = lift,
                              set    = set,
                              reps   = reps,
                              weight = weight,
                              rpe    = rpe)
      dbExecute(con,
                statement = query)
      
      est_1rm <- ceiling(weight + 0.0333 * weight * (reps + (10 - rpe)) * 5) / 5
      updateMaxes(con, 
                  sheet_ID = sheet_ID, 
                  athlete_id = athlete_id, 
                  lift_id = lift, 
                  est_1rm = est_1rm)
    }
  }
  
  print(paste0('New session for ID ', athlete_id))
}

updateMaxes      <- function(con, sheet_ID, athlete_id, lift_id, est_1rm) {
  sql <- 'SELECT * FROM MAXES 
          WHERE Athlete_ID = ?athlete AND
            Lift_ID = ?lift'
  query <- sqlInterpolate(con,
                          sql     = sql,
                          athlete = athlete_id,
                          lift    = lift_id)
  athlete_maxes <- dbGetQuery(con,
                              statement = query)
  
  if (nrow(athlete_maxes) == 0) {
    sql <- 'INSERT INTO MAXES
              (Athlete_ID, Lift_ID, Lift_Max)
            VALUES
              (?athlete, ?lift, ?max)'
    query <- sqlInterpolate(con,
                            sql     = sql,
                            athlete = athlete_id,
                            lift    = lift_id,
                            max     = est_1rm)
    dbExecute(con,
              statement = query)
  } else if (est_1rm > athlete_maxes$Lift_Max[1]) {
    sql <- 'UPDATE MAXES
            SET Lift_Max = ?max
            WHERE Athlete_ID = ?athlete AND
              Lift_ID = ?lift'
    query <- sqlInterpolate(con,
                            sql     = sql,
                            max     = est_1rm,
                            athlete = athlete_id,
                            lift    = lift_id)
    dbExecute(con,
              statement = query)
  }
}

main <- function() {
  sheet_ID <- '1PK5nDP-xsz9165HUxEMZFN8p5v9FYIiOJYK5KEfHc-g'
  con <- dbConnect(RSQLite::SQLite(), "")
  
  createDatabase(con = con,
                 sheet_ID = sheet_ID)
  
  today <- as.integer(Sys.Date()) * 3600 * 24 - 50 * 3600 * 24
  lifts <- list()
  ids <- list()
  for (day in 1:50) {
    random_check <- runif(1)
  
    if (random_check < 0.2) {
      query <- 'SELECT * FROM ATHLETES'
      athlete_df <- as.data.frame(dbGetQuery(con,
                                             query))
      createNewAthlete(con, athlete_df, sheet_ID)
    }
    
    athlete_ids <- dbGetQuery(con,
                              statement = 'SELECT DISTINCT Athlete_ID FROM ATHLETES')
    lift_ids    <- dbGetQuery(con,
                              statement = 'SELECT DISTINCT Lift_ID FROM LIFTS')
    
    for (athlete_id in athlete_ids$Athlete_ID) {
      ids <- append(ids, athlete_id)
      lifts <- append(lifts, list(sample(lift_ids$Lift_ID, 10)))
      names(lifts) <- ids
      
      random_check <- runif(1)
      
      if (random_check < 0.9) {
        createNewWeight(con, sheet_ID, athlete_id, today)
      }
      
      if (random_check < 0.66) {
        createNewSession(con, sheet_ID, athlete_id, lifts[[as.character(athlete_id)]], today)
      }
    }
    
    today <- today + 3600 * 24
  }
  
  for (sheet in sheet_names(sheet_ID)) {
    if (sheet != 'LIFTS') {
      sql <- 'SELECT * FROM ?table ORDER BY Athlete_ID'
      query <- sqlInterpolate(
        con,
        sql = sql,
        table = sheet
      )
        
      dbGetQuery(con,
                 statement = query) %>%
        sheet_write(ss = sheet_ID,
                    sheet = sheet)
    }
  }
}

main()



