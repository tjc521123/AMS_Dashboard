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
      name = input$sel_weight_ath
    )
    
    data <- dbGetQuery(
      con,
      statement = query
    ) %>%
      mutate(
        Date = as.Date(Date)
      )
    print(data)
    
    plot <- data %>%
      ggplot(mapping = aes(x = Date, y = Weight)) +
      geom_line() +
      geom_point() +
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
      name = input$sel_weight_ath
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
      name = input$sel_weight_ath
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
  
  
  
  
  
  
  
  
  
  
  
  
}