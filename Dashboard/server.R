function(input, output, session) {
  #----------------------------------------------------------
  # onStop - Updates google sheet with database
  #----------------------------------------------------------  
  onStop(function(conn = con, sheet = sheet_ID) {
    for (sheet_name in sheet_names) {
      sql <- 'SELECT * FROM ?table'
      query <- sqlInterpolate(
        conn,
        sql = sql,
        table = sheet_name
      )
      
      dbGetQuery(
        con,
        statement = query
      ) %>%
        sheet_write(
          ss = sheet,
          sheet = sheet_name
        )
    }
  })
  
  #----------------------------------------------------------
  # Refresh Data
  #----------------------------------------------------------  
  observeEvent(
    input$refresh_data_button,
    {
      for (sheet_name in sheet_names) {
        dbWriteTable(con, 
                     sheet_name,
                     read_sheet(ss = sheet_ID,
                                sheet = sheet_name),
                     overwrite = TRUE)
      }
      
      output$ath_table <- renderDataTable({
        ath_table <- dbGetQuery(
          con, 
          statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name,
                        strftime("%Y-%m-%d", Athlete_DOB, "unixepoch") AS DOB,
                        Athlete_Gender AS Gender
                       FROM ATHLETES ORDER BY Name'
        )
        
        datatable(ath_table) %>%
          formatDate(
            'DOB',
            method = 'toLocaleDateString'
          )
      })

      choices <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT Main_Lift FROM LIFTS ORDER BY Main_Lift'
      )
      updateSelectInput(
        inputId  = 'addEx_main',
        choices  = choices,
        selected = head(choices, 1)
      )

      choices <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT Pattern FROM LIFTS ORDER BY Pattern'
      )
      updateSelectInput(
        inputId  = 'addEx_pattern',
        choices  = choices,
        selected = head(choices, 1)
      )

      choices <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT Equipment FROM LIFTS ORDER BY Equipment'
      )
      updateSelectInput(
        inputId  = 'addEx_equip',
        choices  = choices,
        selected = head(choices, 1)
      )

      choices <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT Category FROM LIFTS ORDER BY Category'
      )
      updateSelectInput(
        inputId  = 'addEx_category',
        choices  = choices,
        selected = head(choices, 1)
      )
      
      output$ex_table <- renderDataTable({
        ex_table <- dbGetQuery(
          con,
          statement = 'SELECT Lift_Name AS Lift,
                         Main_Lift AS "Main Lift",
                         Pattern,
                         Scaling,
                         Equipment,
                         Category
                       FROM LIFTS ORDER BY Lift_Name'
        )
        
        datatable(ex_table) %>%
          formatPercentage(
            columns = 'Scaling'
          )
      })
    }
  )
  
  #----------------------------------------------------------
  # Create Athlete Table
  #----------------------------------------------------------
  output$ath_table <- renderDataTable({
    ath_table <- dbGetQuery(
      con, 
      statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name,
                        strftime("%Y-%m-%d", Athlete_DOB, "unixepoch") AS DOB,
                        Athlete_Gender AS Gender
                       FROM ATHLETES ORDER BY Name'
    )
    
    datatable(ath_table) %>%
      formatDate(
        'DOB',
        method = 'toLocaleDateString'
      )
  })
  
  #----------------------------------------------------------
  # Update Athlete Table
  #----------------------------------------------------------
  observeEvent(
    input$addAth_button,
    {
      id <- dbGetQuery(
        con,
        statement = 'SELECT MAX(Athlete_ID) AS ID FROM ATHLETES'
      )$ID + 1
      first_name <- input$addAth_first_name
      last_name  <- input$addAth_last_name
      dob        <- as.integer(input$addAth_DOB) * 3600 * 24
      gender     <- input$addAth_gender
      
      sql <- 'INSERT INTO ATHLETES
                (Athlete_ID, Athlete_FirstName, Athlete_LastName, Athlete_DOB, Athlete_Gender)
              VALUES
                (?id, ?first, ?last, ?dob, ?gender)'
      query <- sqlInterpolate(
        con,
        sql    = sql,
        id     = id,
        first  = first_name,
        last   = last_name,
        dob    = dob,
        gender = gender
      )
      
      dbExecute(
        con,
        statement = query
      )
      
      athletes <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name
                     FROM ATHLETES ORDER BY Name'
      )
      
      output$ath_table <- renderDataTable({
        ath_table <- dbGetQuery(
          con, 
          statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name,
                        strftime("%Y-%m-%d", Athlete_DOB, "unixepoch") AS DOB,
                        Athlete_Gender AS Gender
                       FROM ATHLETES ORDER BY Name'
        )

        datatable(ath_table) %>%
          formatDate(
            'DOB',
            method = 'toLocaleDateString'
          )
      })
    }
  )
  
  show_del_modal <- function(athlete_name, athlete_id, delete_ath = FALSE) {
    modalDialog(
      title = 'Delete Athlete',
      span(
        'Are you sure you want to delete:'
      ),
      span(
        paste(athlete_name, '\n', 'ID: ', athlete_id, sep = '')
      ),
      
      footer = tagList(
        actionButton(
          inputId = 'del_confirm',
          label   = 'Delete'
        ),
        modalButton(
          label = 'Cancel'
        )
      )
    )
  }
  
  observeEvent(
    input$del_confirm,
    {
      sql <- 'DELETE FROM ATHLETES
            WHERE Athlete_FirstName = ?first
              AND Athlete_LastName = ?last'
      
      name  <- strsplit(input$sel_del_athlete, ', ')
      first <- name[[1]][2]
      last  <- name[[1]][1]
      
      query <- sqlInterpolate(
        con,
        sql = sql,
        first = first,
        last  = last
      )
      
      dbExecute(
        con,
        statement = query
      )
      
      athletes <- dbGetQuery(
        con,
        statement = 'SELECT DISTINCT CONCAT(Athlete_LastName, ",", Athlete_FirstName) AS Name
                   FROM ATHLETES ORDER BY Name'
      )
      
      output$ath_table <- renderDataTable({
        ath_table <- dbGetQuery(
          con, 
          statement = 'SELECT CONCAT(Athlete_LastName, ", ", Athlete_FirstName) AS Name,
                      strftime("%Y-%m-%d", Athlete_DOB, "unixepoch") AS DOB,
                      Athlete_Gender AS Gender
                     FROM ATHLETES ORDER BY Name'
        )
        
        datatable(ath_table) %>%
          formatDate(
            'DOB',
            method = 'toLocaleDateString'
          )
      })
      
      removeModal()
    }
  )
  
  observeEvent(
    input$del_athlete,
    {
      name  <- strsplit(input$sel_del_athlete, ', ')
      first <- name[[1]][2]
      last  <- name[[1]][1]
      athlete_name = paste(first, last)
      
      athlete_id = dbGetQuery(
        con, 
        statement = sqlInterpolate(
          con,
          sql = 'SELECT Athlete_ID FROM ATHLETES
                 WHERE Athlete_FirstName = ?first
                   AND Athlete_LastName = ?last',
          first = first,
          last  = last
        )
      )
      
      showModal(
        show_del_modal(
          athlete_name = athlete_name,
          athlete_id   = athlete_id
        )
      )
    }
  )
  
  #----------------------------------------------------------
  # Create Calendar Tab
  #----------------------------------------------------------
  output$calendar <- renderCalendar({
    sql <- 'SELECT calendarID, start, end, category, title, state, GROUP_CONCAT(body) AS body
            FROM (SELECT ath.Athlete_ID AS calendarID,
                         strftime("%Y-%m-%d", sess.Session_Date, "unixepoch") AS start,
                         strftime("%Y-%m-%d", sess.Session_Date, "unixepoch") AS end,
                         "allday" AS category,
                         ROUND(MAX(Session_Set)) AS Sets,
                         "Free" AS state,
                         CONCAT(ath.Athlete_FirstName, " ", ath.Athlete_LastName) AS title,
                         CONCAT(lift.Lift_Name, ": ", sess.Session_Set, "x", sess.Reps, "@", sess.Weight, "\n") AS body
                  FROM SESSION sess
                  INNER JOIN ATHLETES ath ON
                    ath.Athlete_ID = sess.Athlete_ID
                    INNER JOIN LIFTS lift ON
                      lift.Lift_ID = sess.Lift_ID
                  GROUP BY calendarID, start, sess.Lift_ID, sess.Reps, sess.Weight)
            GROUP BY calendarID, start'
    
    cal_data <- dbGetQuery(
      con,
      statement = sql
    )
    View(cal_data)

    color_list <- list(distinctColorPalette(length((unique(cal_data$title)))))

    ids        <- list(unique(cal_data$title))
    
    calendar(
      data = cal_data,
      view = tolower(input$sel_calendar_view),
      navigation = TRUE
    ) 
  })
  #----------------------------------------------------------
  # Create Exercise Table
  #----------------------------------------------------------
  output$ex_table <- renderDataTable({
    ex_table <- dbGetQuery(
      con, 
      statement = 'SELECT Lift_Name AS Lift,
                         Main_Lift AS "Main Lift",
                         Pattern,
                         Scaling,
                         Equipment,
                         Category
                       FROM LIFTS ORDER BY Lift_Name'
    )
    
    datatable(ex_table) %>%
      formatPercentage(
        columns = 'Scaling'
      )
  })
  
  #----------------------------------------------------------
  # Add New Exercise
  #----------------------------------------------------------
  observeEvent(
    input$addEx_button,
    {
      id <- dbGetQuery(
        con,
        statement = 'SELECT MAX(Lift_ID) AS ID FROM LIFTS'
      )$ID + 1
      
      name    <- input$addEx_name
      main    <- input$addEx_main
      pattern <- input$addEx_pattern
      scale   <- input$addEx_scaling / 100
      equip   <- input$addEx_equip
      cat     <- input$addEx_category
      
      sql <- 'INSERT INTO LIFTS
                (Lift_ID, Lift_Name, Main_Lift, Pattern, Scaling, Equipment, Category)
              VALUES
                (?id, ?name, ?main, ?pattern, ?scale, ?equip, ?cat)'
      query <- sqlInterpolate(
        con,
        sql     = sql,
        id      = id,
        name    = name,
        main    = main,
        pattern = pattern,
        scale   = scale,
        equip   = equip,
        cat     = cat
      )
      
      dbExecute(
        con,
        statement = query
      )
      
      output$ex_table <- renderDataTable({
        ex_table <- dbGetQuery(
          con, 
          statement = 'SELECT Lift_Name AS Lift,
                         Main_Lift AS "Main Lift",
                         Pattern,
                         Scaling,
                         Equipment,
                         Category
                       FROM LIFTS ORDER BY Lift_Name'
        )
        
        datatable(ex_table) %>%
          formatPercentage(
            columns = 'Scaling'
          )
      })
    }
  )
  #----------------------------------------------------------
  # Create Weight Plot
  #----------------------------------------------------------
  output$plot_ath_weight <- renderPlotly({
    
    sql <- 'SELECT strftime("%Y-%m-%d", Date, "unixepoch") AS Date, Weight
            FROM WEIGHT
            WHERE Athlete_ID = (SELECT Athlete_ID FROM ATHLETES
                                WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)'
      
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete
    )
    
    data <- dbGetQuery(
      con,
      statement = query
    ) %>%
      mutate(
        Date = as.Date(Date)
      )
    
    plot <- data %>%
      ggplot(mapping = aes(x = Date, y = Weight)) +
      geom_line() +
      labs(
        x = 'Date',
        y = 'Body Weight (lbs)'
      ) +
      ylim(
        0, NA
      )
    
    ggplotly(plot)
  })
  
  #----------------------------------------------------------
  # Create Weight Showcase
  #----------------------------------------------------------
  output$showcase_weight_change <- renderUI({
    sql <- 'SELECT MAX(Weight) AS Max
            FROM WEIGHT
            WHERE Athlete_ID = (SELECT Athlete_ID FROM ATHLETES
                                WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)'
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete
    )
    
    max_weight <- dbGetQuery(
      con,
      statement = query
    )$Max[1]
    
    sql <- 'SELECT MIN(Weight) AS Min
            FROM WEIGHT
            WHERE Athlete_ID = (SELECT Athlete_ID FROM ATHLETES
                                WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)'
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete
    )
    
    min_weight <- dbGetQuery(
      con,
      statement = query
    )$Min[1]
    
    weight_change <- round(max_weight - min_weight, 1)
    
    value_box(
      title = 'Weight Change',
      value = weight_change,
      theme = 'green',
      showcase = bs_icon('graph-down'),
      p('Heaviest: ', round(max_weight, 1)),
      p('Lowest: ', round(min_weight, 1))
    )
  })
  
  #----------------------------------------------------------
  # Update Lift Selection
  #----------------------------------------------------------
  observeEvent(
    input$sel_athlete,
    {
      sql <-'SELECT DISTINCT Lift_Name
       FROM LIFTS
       WHERE Lift_ID IN (SELECT Lift_ID FROM SESSION
                         WHERE Athlete_ID IN (SELECT Athlete_ID 
                                              FROM ATHLETES
                                              WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name))'
      query <- sqlInterpolate(
        con,
        sql = sql,
        name = input$sel_athlete
      )
      
      lift_choices <- dbGetQuery(
        con,
        statement = query
      )
      
      updateSelectInput(
        inputId = 'sel_lift_ath',
        choices = lift_choices
      )
    }
  )
  
  
  
  
  
  
  
  
  
  
  #----------------------------------------------------------
  # Create Lift Showcase
  #----------------------------------------------------------
  output$showcase_lift_estMax <- renderUI({
    sql <- 'SELECT Date, MAX(Est) AS Est, Reps, RPE, Weight
            FROM (SELECT strftime("%Y-%m-%d", Session_Date, "unixepoch") AS Date, 
                         (Weight + 0.0333 * Weight * (Reps + (10 - RPE))) AS Est, 
                         Weight, Reps, RPE
                  FROM SESSION
                  WHERE Athlete_ID IN (SELECT Athlete_ID FROM ATHLETES
                                       WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)
                    AND Lift_ID IN (SELECT Lift_ID FROM LIFTS
                                    WHERE Lift_Name = ?lift))'
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete,
      lift = input$sel_lift_ath
    )
    
    data <- dbGetQuery(
      con,
      statement = query
    )

    value_box(
      title = 'Best Estimated 1RM',
      value = round(data$Est[1]),
      theme = 'green',
      showcase = as.Date(data$Date[1]),
      showcase_layout = showcase_left_center(
        width = 0.4
      ),
      p(paste(data$Weight[1], 'lbs. x', data$Reps[1], "@RPE ", data$RPE[1], sep = ''))
    )
  })
  
  output$showcase_lift_best <- renderUI({
    sql <- 'SELECT strftime("%Y-%m-%d", Session_Date, "unixepoch") AS Date, Weight, Reps, RPE
            FROM SESSION
            WHERE Athlete_ID IN (SELECT Athlete_ID FROM ATHLETES
                                 WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)
              AND Lift_ID IN (SELECT Lift_ID FROM LIFTS
                              WHERE Lift_Name = ?lift)
            GROUP BY Date ORDER BY Weight DESC'
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete,
      lift = input$sel_lift_ath
    )
    
    data <- dbGetQuery(
      con,
      statement = query
    )

    value_box(
      title = 'Best Performed',
      value = round(data$Weight[1]),
      theme = 'green',
      showcase = as.Date(data$Date[1]),
      showcase_layout = showcase_left_center(
        width = 0.4
      ),
      p(paste(data$Reps[1], "@RPE ", data$RPE[1], sep = ''))
    )
  })
  #----------------------------------------------------------
  # Create Lift Plot
  #----------------------------------------------------------
  output$plot_ath_lift <- renderPlotly({
    
    sql <- 'SELECT strftime("%Y-%m-%d", Session_Date, "unixepoch") AS Date, Est, Weight
            FROM (SELECT Session_Date, 
                         MAX((Weight + 0.0333 * Weight * (Reps + (10 - RPE)))) AS Est,
                         Weight,
                         Reps, RPE, Athlete_ID, Lift_ID
                  FROM SESSION
                  GROUP BY Athlete_ID, Session_Date, Lift_ID)
            WHERE Athlete_ID IN (SELECT Athlete_ID FROM ATHLETES
                                WHERE CONCAT(Athlete_LastName, ", ", Athlete_FirstName) = ?name)
              AND Lift_ID IN (SELECT Lift_ID FROM LIFTS
                              WHERE Lift_Name = ?lift)'
    
    query <- sqlInterpolate(
      con,
      sql = sql,
      name = input$sel_athlete,
      lift = input$sel_lift_ath
    )
    
    data <- dbGetQuery(
      con,
      statement = query
    ) %>%
      mutate(
        Date = as.Date(Date)
      )

    plot <- data %>%
      ggplot(mapping = aes(x = Date, y = Weight)) +
      geom_line(color = 'red') +
      geom_point(color = 'red') +
      geom_line(aes(y = Est), color = 'blue') +
      geom_point(aes(y = Est), color = 'blue') +
      labs(
        x = 'Date',
        y = 'Estimated 1-RM (lbs)'
      ) +
      ylim(
        0, NA
      )
    
    ggplotly(plot)
  })
}