add_busy_spinner(
  spin = 'fading-circle',
  timeout = 10)

page_navbar(
  title = 'Athlete Management System',
  theme = bs_theme(preset = 'spacelab'),
  
  sidebar = sidebar(
    width = '15%',
    actionButton(
      inputId = 'refresh_data_button',
      label   = 'Refresh Data',
      class   = 'btn-primary'
    )
  ),
  
  #----------------------------------------------------------
  # Athlete Info Tab
  #----------------------------------------------------------
  nav_panel(
    title = 'Athlete Overview',
    fluidRow(
      layout_columns(
        col_widths = c(3, 9),
        
        card(
          card_header(
            'Select Athlete To View'
          ),
          
          selectInput(
            inputId = 'sel_athlete',
            label   = '',
            choices = weight_choices
          ),
          
          uiOutput(
            outputId = 'showcase_weight_change'
          )
        ),
        
        card(
          card_header(
            'Athlete Body Weight Over Time'
          ),

          plotlyOutput(
            outputId = 'plot_ath_weight'
          )
        )
      )
    ),
    
    fluidRow(
      layout_columns(
        col_widths = c(3, 9),
        
        card(
          card_header(
            'Select Lift'
          ),
          
          selectInput(
            inputId = 'sel_lift_ath',
            label   = '',
            choices = lift_choices
          ),
          
          uiOutput(
            outputId = 'showcase_lift_estMax'
          ),
          
          uiOutput(
            outputId = 'showcase_lift_best'
          )
        ),
        
        card(
          card_header(
            'Estimated 1-RM Over Time'
          ),
          
          plotlyOutput(
            outputId = 'plot_ath_lift'
          )
        )
      )
    )
    
  ),
  #----------------------------------------------------------
  # Calendar Tab
  #----------------------------------------------------------
  nav_panel(
    title = 'Calendar',
    
    selectInput(
      inputId  = 'sel_calendar_view',
      label    = 'Select Calendar View',
      choices  = c('Month', 'Week', 'Day'),
      selected = 'Month'
    ),
    
    calendarOutput(
      outputId = 'calendar'
    )
  ),
  
  
  
  
  #----------------------------------------------------------
  # Athlete Tab
  #----------------------------------------------------------
  nav_panel(
    title = 'Athletes',
    
    layout_columns(
      col_widths = c(2, 10),
      card(
        textInput(
          inputId = 'addAth_first_name',
          label   = 'Athlete First Name',
          value   = ''
        ),
        
        textInput(
          inputId = 'addAth_last_name',
          label   = 'Athlete Last Name',
          value   = ''
        ),
        
        dateInput(
          inputId = 'addAth_DOB',
          label   = 'Enter DOB',
          max     = Sys.Date()
        ),
        
        selectInput(
          inputId = 'addAth_gender',
          label   = 'Gender',
          choices = c('Male', 'Female', 'Other')
        ),
        
        actionButton(
          inputId = 'addAth_button',
          label   = 'Add New Athlete',
          class   = 'btn-primary'
        )
      ),
      
      card(
        dataTableOutput(
          outputId = 'ath_table'
        )
      )
    )
  ),
  
  #----------------------------------------------------------
  # Lift Tab
  #----------------------------------------------------------
  nav_panel(
    title = 'Exercises',
    
    layout_columns(
      col_widths = c(2, 10),
      card(
        width = '25%',
        textInput(
          inputId = 'addEx_name',
          label   = 'New Exercise Name',
          value   = ''
        ),
        
        selectInput(
          inputId = 'addEx_main',
          label   = 'Main Exercise',
          choices = dbGetQuery(
            con,
            statement = 'SELECT DISTINCT Main_Lift FROM LIFTS ORDER BY Main_Lift'
          )
        ),
        
        selectInput(
          inputId = 'addEx_pattern',
          label   = 'Movement Pattern',
          choices = dbGetQuery(
            con,
            statement = 'SELECT DISTINCT Pattern FROM LIFTS ORDER BY Pattern'
          )
        ),
        
        sliderInput(
          inputId = 'addEx_scaling',
          label   = 'Scaling To Main Lift',
          value   = 0,
          min     = 0,
          max     = 100,
          step    = 1
        ),
        
        selectInput(
          inputId = 'addEx_equip',
          label   = 'Equipment Used',
          choices = dbGetQuery(
            con,
            statement = 'SELECT DISTINCT Equipment FROM LIFTS ORDER BY Equipment'
          )
        ),
        
        selectInput(
          inputId = 'addEx_category',
          label   = 'Exercise Category',
          choices = dbGetQuery(
            con,
            statement = 'SELECT DISTINCT Category FROM LIFTS ORDER BY Category'
          )
        ),
        
        actionButton(
          inputId = 'addEx_button',
          label   = 'Add Exercise',
          class   = 'btn-primary'
        )
      ),
      
      card(
        dataTableOutput(
          outputId = 'ex_table'
        )
      )
    )
  )
)