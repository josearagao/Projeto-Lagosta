# ==================================================================
# Fishing Database Management - Shiny App
# Improved version with validation, auto-refresh,
# uppercase text, no accents, and readable TripData location fields
# ==================================================================

### admin or / admin123
### dataentry / data123

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(shinydashboard)

# Optional package used for Excel export.
# Install once in R if needed: install.packages("writexl")

# --- App settings ---
db_file <- "db_Estatpesca.sqlite"
backup_dir <- "database_backups"

# Simple local users. Change these passwords before using the app in production.
app_users <- data.frame(
  user = c("admin", "dataentry"),
  password = c("admin123", "data123"),
  role = c("admin", "dataentry"),
  stringsAsFactors = FALSE
)

# --- Connect to SQLite database ---
db <- dbConnect(SQLite(), db_file)

# Disconnect when app stops
onStop(function() {
  dbDisconnect(db)
})

# ======================================
# Helper functions
# ======================================

# Converts text to uppercase and removes orthographic accents.
# Example: "Ceará" becomes "CEARA".

clean_text <- function(x) {
  x <- trimws(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  return(x)
}

get_states <- function() {
  dbGetQuery(db, "SELECT UFcode, UFname, UFacronym FROM State ORDER BY UFname")
}

get_munis <- function(UFcode) {
  dbGetQuery(
    db,
    "SELECT Mcode, Mname FROM Municipality WHERE UFcode = ? ORDER BY Mname",
    params = list(UFcode)
  )
}

get_locals <- function(Mcode) {
  dbGetQuery(
    db,
    "SELECT Lcode, Lname FROM Local WHERE Mcode = ? ORDER BY Lname",
    params = list(Mcode)
  )
}

get_boats <- function() {
  dbGetQuery(db, "SELECT Bacronym, Bname FROM Boat ORDER BY Bname")
}

get_gears <- function() {
  dbGetQuery(db, "SELECT Aacronym, Aname FROM Fishgear ORDER BY Aname")
}

get_species <- function() {
  dbGetQuery(db, "SELECT Scode, Scientific FROM Specie ORDER BY Scientific")
}

get_fishgrounds <- function() {
  dbGetQuery(db, "SELECT FGname, Depth, BottomType, Latitude, Longitude, Reference FROM FishGround ORDER BY FGname")
}

empty_choice <- function(label = "-- select --") {
  setNames("", label)
}

safe_choices <- function(values, names) {
  c(empty_choice(), setNames(values, names))
}

backup_database <- function(reason = "manual") {
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  reason <- gsub("[^A-Za-z0-9_]+", "_", reason)
  backup_file <- file.path(backup_dir, paste0("db_Estatpesca_", stamp, "_", reason, ".sqlite"))
  if (file.exists(db_file)) {
    file.copy(db_file, backup_file, overwrite = TRUE)
  }
  backup_file
}

db_exists <- function(sql, params = list()) {
  res <- dbGetQuery(db, sql, params = params)
  nrow(res) > 0
}

require_reference <- function(sql, params, message) {
  if (!db_exists(sql, params)) {
    stop(message)
  }
}

with_auto_backup <- function(reason, expr) {
  backup_database(reason)
  force(expr)
}

# Tables that can be edited directly.

editable_tables <- c(
  "State", "Municipality", "Local", "Boat", "Fishgear",
  "Specie", "Vessel", "TripData", "LandData",
  "FishGround", "LandSample", "LandBiom"
)

# Fields that should not be modified in the edit window.
# I protect main ID/key fields to avoid breaking relationships between tables.
protected_fields <- function(table_name) {
  switch(table_name,
         "State" = c("UFcode"),
         "Municipality" = c("Mcode"),
         "Local" = c("Lcode"),
         "Boat" = c("Bacronym"),
         "Fishgear" = c("Aacronym"),
         "Specie" = c("Scode"),
         "Vessel" = c("Vname"),
         "TripData" = c("Tcode"),
         "LandData" = c("Tcode", "Scode"),
         "FishGround" = c("FGname"),
         "LandSample" = c("LScode"),
         "LandBiom" = c("LBcode", "LScode"),
         character(0))
}

# Gets state acronym, municipality name, and local name from selected codes.

get_trip_location_labels <- function(UFcode, Mcode, Lcode) {
  df <- dbGetQuery(db, "
    SELECT
      State.UFacronym,
      Municipality.Mname,
      Local.Lname
    FROM State
    LEFT JOIN Municipality
      ON State.UFcode = Municipality.UFcode
    LEFT JOIN Local
      ON Municipality.Mcode = Local.Mcode
    WHERE State.UFcode = ?
      AND Municipality.Mcode = ?
      AND Local.Lcode = ?
  ", params = list(UFcode, Mcode, Lcode))
  
  if (nrow(df) == 0) {
    stop("Invalid state, municipality, or local selection.")
  }
  
  df[1, ]
}

# ======================================
# UI
# ======================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Fishing Database"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      div(style = "padding: 10px;",
          textInput("login_user", "User", value = ""),
          passwordInput("login_password", "Password"),
          actionButton("login_btn", "Login"),
          actionButton("logout_btn", "Logout"),
          br(), br(),
          textOutput("login_status")
      ),
      
      menuItem("Cadast", icon = icon("folder-open"),
               menuSubItem("States", tabName = "states"),
               menuSubItem("Municipality", tabName = "municipality"),
               menuSubItem("Local", tabName = "local"),
               menuSubItem("Boats", tabName = "boats"),
               menuSubItem("Fishing Gear", tabName = "fishgear"),
               menuSubItem("Species", tabName = "species"),
               menuSubItem("FishGround", tabName = "fishground")
      ),
      
      menuItem("Moviment", icon = icon("ship"),
               menuSubItem("Vessels", tabName = "vessels"),
               menuSubItem("Trip Data", tabName = "tripdata")
      ),
      
      menuItem("Samples", tabName = "samples", icon = icon("fish")),
      menuItem("Data Viewer", tabName = "viewer", icon = icon("table")),
      menuItem("Reports", tabName = "reports", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------- STATES ----------
      
      tabItem(tabName = "states",
              box(title = "States", width = 12, status = "primary", solidHeader = TRUE,
                  textInput("UFname", "State name"),
                  textInput("UFacronym", "Acronym"),
                  actionButton("add_state", "Add State"),
                  br(), br(),
                  verbatimTextOutput("state_msg")
              )
      ),
      
      # ---------- MUNICIPALITY ----------
      
      tabItem(tabName = "municipality",
              box(title = "Municipality", width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("UFcode_m", "Select State", choices = NULL),
                  textInput("Mname", "Municipality name"),
                  actionButton("add_muni", "Add Municipality"),
                  br(), br(),
                  verbatimTextOutput("muni_msg")
              )
      ),
      
      # ---------- LOCAL ----------
      
      tabItem(tabName = "local",
              box(title = "Local", width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("UFcode_l", "Select State", choices = NULL),
                  selectInput("Mcode_l", "Municipality", choices = empty_choice()),
                  textInput("Lname", "Local name"),
                  actionButton("add_local", "Add Local"),
                  br(), br(),
                  verbatimTextOutput("local_msg")
              )
      ),
      
      # ---------- BOATS ----------
      
      tabItem(tabName = "boats",
              box(title = "Boats", width = 12, status = "primary", solidHeader = TRUE,
                  textInput("Bname", "Boat type name"),
                  textInput("Bacronym", "Boat acronym"),
                  actionButton("add_boat", "Add Boat"),
                  br(), br(),
                  verbatimTextOutput("boat_msg")
              )
      ),
      
      # ---------- FISHING GEAR ----------
      
      tabItem(tabName = "fishgear",
              box(title = "Fishing Gear", width = 12, status = "primary", solidHeader = TRUE,
                  textInput("Aname", "Gear name"),
                  textInput("Aacronym", "Gear acronym"),
                  textInput("Atype", "Gear type"),
                  actionButton("add_gear", "Add Fishgear"),
                  br(), br(),
                  verbatimTextOutput("gear_msg")
              )
      ),
      
      # ---------- SPECIES ----------
      
      tabItem(tabName = "species",
              box(title = "Species", width = 12, status = "primary", solidHeader = TRUE,
                  textInput("Sname_1", "Common name 1"),
                  textInput("Sname_2", "Common name 2"),
                  textInput("Scientific", "Scientific name"),
                  actionButton("add_specie", "Add Specie"),
                  br(), br(),
                  verbatimTextOutput("specie_msg")
              )
      ),
      
      # ---------- FISHGROUND ----------
      
      tabItem(tabName = "fishground",
              box(title = "FishGround", width = 12, status = "primary", solidHeader = TRUE,
                  textInput("FGname", "Fishing Ground name"),
                  numericInput("FGdepth", "Depth", value = 0, min = 0, step = 0.1),
                  selectInput("FGbottom", "Bottom type", choices = c(
                    "-- select --" = "",
                    "W - Seaweed" = "W",
                    "S - Sand" = "S",
                    "M - Mud" = "M",
                    "R - Rock/stones" = "R"
                  )),
                  textInput("FGlat", "Latitude"),
                  textInput("FGlon", "Longitude"),
                  textInput("FGref", "Reference"),
                  actionButton("add_fishground", "Add FishGround"),
                  br(), br(),
                  verbatimTextOutput("fishground_msg")
              )
      ),
      
      # ---------- VESSELS ----------
      
      tabItem(tabName = "vessels",
              box(title = "Vessels", width = 12, status = "warning", solidHeader = TRUE,
                  selectInput("UFcode_v", "State", choices = NULL),
                  selectInput("Mcode_v", "Municipality", choices = empty_choice()),
                  selectInput("Lcode_v", "Local", choices = empty_choice()),
                  textInput("Vname", "Vessel name"),
                  selectInput("Bacronym_v", "Boat type", choices = NULL),
                  textInput("Vowner", "Owner name"),
                  numericInput("Vlength", "Length", value = 0, min = 0, step = 0.1),
                  numericInput("Vpropul", "Propulsion type", value = 0, min = 0),
                  actionButton("add_vessel", "Add Vessel"),
                  br(), br(),
                  verbatimTextOutput("vessel_msg")
              )
      ),
      
      # ---------- TRIP DATA + LAND DATA ----------
      
      tabItem(tabName = "tripdata",
              fluidRow(
                box(title = "Trip Information", width = 4, status = "warning", solidHeader = TRUE,
                    textInput("vname", "Vessel Name"),
                    selectInput("UFcode_trip", "State", choices = NULL),
                    selectInput("Mcode_trip", "Municipality", choices = empty_choice()),
                    selectInput("Lcode_trip", "Local", choices = empty_choice()),
                    selectInput("bacronym_trip", "Boat Type", choices = NULL),
                    selectInput("aacronym_trip", "Fishing Gear", choices = NULL),
                    dateInput("dept_date", "Departure Date"),
                    dateInput("retu_date", "Return Date")
                ),
                
                box(title = "Land Data / Species", width = 8, status = "warning", solidHeader = TRUE,
                    selectInput("scode", "Species", choices = NULL),
                    numericInput("quantity", "Quantity", value = 0, min = 0),
                    actionButton("add_species", "Add Species to Table"),
                    actionButton("clear_species", "Clear Species Table"),
                    hr(),
                    DTOutput("species_table"),
                    hr(),
                    actionButton("new_trip", "Insert Trip & Species Data"),
                    br(), br(),
                    verbatimTextOutput("trip_msg")
                )
              )
      ),
      
      # ---------- SAMPLES ----------
      
      tabItem(tabName = "samples",
              fluidRow(
                box(title = "Land Sample", width = 4, status = "info", solidHeader = TRUE,
                    selectInput("UFcode_sample", "State", choices = NULL),
                    selectInput("Mcode_sample", "Municipality", choices = empty_choice()),
                    selectInput("Lcode_sample", "Local", choices = empty_choice()),
                    dateInput("sample_date", "Sample Date"),
                    selectInput("bacronym_sample", "Boat Type", choices = NULL),
                    selectInput("aacronym_sample", "Fishing Gear", choices = NULL),
                    selectInput("fgname_sample", "Fishing Ground", choices = NULL)
                ),
                
                box(title = "Land Biometrics", width = 4, status = "info", solidHeader = TRUE,
                    selectInput("scientific_biom", "Species", choices = NULL),
                    selectInput("sex_biom", "Sex", choices = c(
                      "-- select --" = "",
                      "F - Female" = "F",
                      "M - Male" = "M",
                      "N - Not identified" = "N"
                    )),
                    selectInput("length_type_biom", "Length Type", choices = c(
                      "-- select --" = "",
                      "TT - Total length" = "TT",
                      "CA - Tail length" = "CA",
                      "CF - Cephalothorax length" = "CF"
                    )),
                    numericInput("length_biom", "Length (mm)", value = 0, min = 0, step = 0.1),
                    numericInput("weight_biom", "Weight (g)", value = 0, min = 0, step = 0.1),
                    actionButton("add_biom", "Add Biometric Record"),
                    actionButton("clear_biom", "Clear Biometric Table"),
                    hr(),
                    DTOutput("biom_table"),
                    hr(),
                    actionButton("new_sample", "Insert Sample & Biometrics"),
                    br(), br(),
                    verbatimTextOutput("sample_msg")
                )
              )
      ),
      
      # ---------- DATA VIEWER ----------
      
      tabItem(tabName = "viewer",
              box(title = "View & Export Data", width = 12, status = "success", solidHeader = TRUE,
                  selectInput("table_select", "Select Table", choices = c(
                    "State", "Municipality", "Local", "Boat", "Fishgear",
                    "Specie", "Vessel", "TripData", "LandData",
                    "FishGround", "LandSample", "LandBiom",
                    "TripData + LandData", "LandSample + LandBiom"
                  )),
                  actionButton("refresh_table", "Refresh Data"),
                  actionButton("edit_selected", "Edit selected row"),
                  actionButton("delete_selected", "Delete selected row", class = "btn-danger"),
                  downloadButton("export_csv", "Export to CSV"),
                  downloadButton("export_xlsx", "Export to Excel"),
                  actionButton("manual_backup", "Create Backup"),
                  br(), br(),
                  textOutput("backup_msg"),
                  hr(),
                  helpText("To edit: choose a table, select one row, then click 'Edit selected row'."),
                  DTOutput("table_view")
              )
      ),
      
      # ---------- REPORTS ----------
      
      tabItem(tabName = "reports",
              fluidRow(
                box(title = "Report Filters", width = 4, status = "info", solidHeader = TRUE,
                    dateInput("report_date_from", "From", value = Sys.Date() - 365),
                    dateInput("report_date_to", "To", value = Sys.Date()),
                    actionButton("generate_report", "Generate Report")
                ),
                box(title = "Total Catch by Species", width = 8, status = "info", solidHeader = TRUE,
                    DTOutput("report_species")
                )
              ),
              fluidRow(
                box(title = "Total Catch by Vessel", width = 6, status = "info", solidHeader = TRUE,
                    DTOutput("report_vessel")
                ),
                box(title = "Total Catch by Month", width = 6, status = "info", solidHeader = TRUE,
                    DTOutput("report_month")
                )
              ),
              fluidRow(
                box(title = "Samples by Species", width = 6, status = "info", solidHeader = TRUE,
                    DTOutput("report_sample_species")
                ),
                box(title = "Biometrics Summary by Species", width = 6, status = "info", solidHeader = TRUE,
                    DTOutput("report_biom_summary")
                )
              ),
              fluidRow(
                box(title = "Samples by Fishing Ground", width = 12, status = "info", solidHeader = TRUE,
                    DTOutput("report_fishground")
                )
              )
      )
    )
  )
)

# ======================================
# SERVER
# ======================================

server <- function(input, output, session) {
  
  logged_user <- reactiveVal(NULL)
  logged_role <- reactiveVal(NULL)
  
  is_logged_in <- reactive({ !is.null(logged_user()) })
  is_admin <- reactive({ identical(logged_role(), "admin") })
  
  output$login_status <- renderText({
    if (is_logged_in()) {
      paste("Logged in as", logged_user(), "- role:", logged_role())
    } else {
      "Not logged in"
    }
  })
  
  observeEvent(input$login_btn, {
    usr <- trimws(input$login_user)
    pwd <- input$login_password
    match <- app_users[app_users$user == usr & app_users$password == pwd, , drop = FALSE]
    if (nrow(match) == 1) {
      logged_user(match$user[1])
      logged_role(match$role[1])
      showNotification("Login successful.", type = "message")
    } else {
      logged_user(NULL)
      logged_role(NULL)
      showNotification("Invalid user or password.", type = "error")
    }
  })
  
  observeEvent(input$logout_btn, {
    logged_user(NULL)
    logged_role(NULL)
    showNotification("Logged out.", type = "message")
  })
  
  require_login <- function() {
    if (!is_logged_in()) {
      stop("Please login before saving data.")
    }
  }
  
  require_admin <- function() {
    if (!is_admin()) {
      stop("Only the administrator can edit or delete existing records.")
    }
  }
  
  # Tables are created by the CREATE database script.
  # The previous line initialize_sample_tables() was removed because
  # the function was not present in this script.
  
  refresh_trigger <- reactiveVal(0)
  
  refresh_dropdowns <- function() {
    refresh_trigger(refresh_trigger() + 1)
  }
  
  refresh_fishground_sample_dropdown <- function(selected = NULL) {
    fishgrounds <- get_fishgrounds()
    updateSelectInput(
      session,
      "fgname_sample",
      choices = safe_choices(fishgrounds$FGname, fishgrounds$FGname),
      selected = if (!is.null(selected) && selected %in% fishgrounds$FGname) selected else ""
    )
  }
  
  # ---------- Load and refresh dropdowns ----------
  
  observe({
    refresh_trigger()
    
    states <- get_states()
    boats <- get_boats()
    gears <- get_gears()
    species <- get_species()
    fishgrounds <- get_fishgrounds()
    
    updateSelectInput(session, "UFcode_m",
                      choices = safe_choices(states$UFcode, states$UFname))
    
    updateSelectInput(session, "UFcode_l",
                      choices = safe_choices(states$UFcode, states$UFname))
    
    updateSelectInput(session, "UFcode_v",
                      choices = safe_choices(states$UFcode, states$UFname))
    
    updateSelectInput(session, "UFcode_trip",
                      choices = safe_choices(states$UFcode, states$UFname))
    
    updateSelectInput(session, "Bacronym_v",
                      choices = safe_choices(boats$Bacronym, boats$Bname))
    
    updateSelectInput(session, "bacronym_trip",
                      choices = safe_choices(boats$Bacronym, boats$Bname))
    
    updateSelectInput(session, "aacronym_trip",
                      choices = safe_choices(gears$Aacronym, gears$Aname))
    
    updateSelectInput(session, "scode",
                      choices = safe_choices(species$Scode, species$Scientific))
    
    updateSelectInput(session, "UFcode_sample",
                      choices = safe_choices(states$UFcode, states$UFname))
    
    updateSelectInput(session, "bacronym_sample",
                      choices = safe_choices(boats$Bacronym, boats$Bname))
    
    updateSelectInput(session, "aacronym_sample",
                      choices = safe_choices(gears$Aacronym, gears$Aname))
    
    updateSelectInput(session, "scientific_biom",
                      choices = safe_choices(species$Scientific, species$Scientific))
    
    updateSelectInput(session, "fgname_sample",
                      choices = safe_choices(fishgrounds$FGname, fishgrounds$FGname))
  })
  
  observeEvent(input$tabs, {
    if (identical(input$tabs, "samples")) {
      refresh_fishground_sample_dropdown()
    }
  }, ignoreInit = FALSE)
  
  # ---------- Dynamic dropdowns ----------
  
  observeEvent(input$UFcode_l, {
    req(input$UFcode_l != "")
    munis <- get_munis(input$UFcode_l)
    updateSelectInput(session, "Mcode_l",
                      choices = safe_choices(munis$Mcode, munis$Mname))
  })
  
  observeEvent(input$UFcode_v, {
    req(input$UFcode_v != "")
    munis <- get_munis(input$UFcode_v)
    updateSelectInput(session, "Mcode_v",
                      choices = safe_choices(munis$Mcode, munis$Mname))
    updateSelectInput(session, "Lcode_v", choices = empty_choice())
  })
  
  observeEvent(input$Mcode_v, {
    req(input$Mcode_v != "")
    locals <- get_locals(input$Mcode_v)
    updateSelectInput(session, "Lcode_v",
                      choices = safe_choices(locals$Lcode, locals$Lname))
  })
  
  observeEvent(input$UFcode_trip, {
    req(input$UFcode_trip != "")
    munis <- get_munis(input$UFcode_trip)
    updateSelectInput(session, "Mcode_trip",
                      choices = safe_choices(munis$Mcode, munis$Mname))
    updateSelectInput(session, "Lcode_trip", choices = empty_choice())
  })
  
  observeEvent(input$Mcode_trip, {
    req(input$Mcode_trip != "")
    locals <- get_locals(input$Mcode_trip)
    updateSelectInput(session, "Lcode_trip",
                      choices = safe_choices(locals$Lcode, locals$Lname))
  })
  
  observeEvent(input$UFcode_sample, {
    req(input$UFcode_sample != "")
    munis <- get_munis(input$UFcode_sample)
    updateSelectInput(session, "Mcode_sample",
                      choices = safe_choices(munis$Mcode, munis$Mname))
    updateSelectInput(session, "Lcode_sample", choices = empty_choice())
  })
  
  observeEvent(input$Mcode_sample, {
    req(input$Mcode_sample != "")
    locals <- get_locals(input$Mcode_sample)
    updateSelectInput(session, "Lcode_sample",
                      choices = safe_choices(locals$Lcode, locals$Lname))
  })
  
  # ======================================
  # Insertions: Cadast
  # ======================================
  
  observeEvent(input$add_state, {
    tryCatch({
      require_login()
      UFname_clean <- clean_text(input$UFname)
      UFacronym_clean <- clean_text(input$UFacronym)
      req(nzchar(UFname_clean), nzchar(UFacronym_clean))
      with_auto_backup("before_add_state", NULL)
      
      dbExecute(db,
                "INSERT INTO State (UFname, UFacronym) VALUES (?, ?)",
                params = list(UFname_clean, UFacronym_clean))
      
      updateTextInput(session, "UFname", value = "")
      updateTextInput(session, "UFacronym", value = "")
      refresh_dropdowns()
      output$state_msg <- renderText("State added successfully.")
      
    }, error = function(e) {
      output$state_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_muni, {
    tryCatch({
      require_login()
      Mname_clean <- clean_text(input$Mname)
      req(input$UFcode_m != "", nzchar(Mname_clean))
      require_reference("SELECT 1 FROM State WHERE UFcode = ?", list(input$UFcode_m), "Selected State does not exist.")
      with_auto_backup("before_add_municipality", NULL)
      
      dbExecute(db,
                "INSERT INTO Municipality (UFcode, Mname) VALUES (?, ?)",
                params = list(input$UFcode_m, Mname_clean))
      
      updateTextInput(session, "Mname", value = "")
      output$muni_msg <- renderText("Municipality added successfully.")
      
    }, error = function(e) {
      output$muni_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_local, {
    tryCatch({
      require_login()
      Lname_clean <- clean_text(input$Lname)
      req(input$UFcode_l != "", input$Mcode_l != "", nzchar(Lname_clean))
      require_reference("SELECT 1 FROM Municipality WHERE Mcode = ? AND UFcode = ?", list(input$Mcode_l, input$UFcode_l), "Selected Municipality does not belong to the selected State.")
      with_auto_backup("before_add_local", NULL)
      
      dbExecute(db,
                "INSERT INTO Local (UFcode, Mcode, Lname) VALUES (?, ?, ?)",
                params = list(input$UFcode_l, input$Mcode_l, Lname_clean))
      
      updateTextInput(session, "Lname", value = "")
      output$local_msg <- renderText("Local added successfully.")
      
    }, error = function(e) {
      output$local_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_boat, {
    tryCatch({
      require_login()
      Bname_clean <- clean_text(input$Bname)
      Bacronym_clean <- clean_text(input$Bacronym)
      req(nzchar(Bname_clean), nzchar(Bacronym_clean))
      with_auto_backup("before_add_boat", NULL)
      
      dbExecute(db,
                "INSERT INTO Boat (Bname, Bacronym) VALUES (?, ?)",
                params = list(Bname_clean, Bacronym_clean))
      
      updateTextInput(session, "Bname", value = "")
      updateTextInput(session, "Bacronym", value = "")
      refresh_dropdowns()
      output$boat_msg <- renderText("Boat added successfully.")
      
    }, error = function(e) {
      output$boat_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_gear, {
    tryCatch({
      require_login()
      Aname_clean <- clean_text(input$Aname)
      Aacronym_clean <- clean_text(input$Aacronym)
      Atype_clean <- clean_text(input$Atype)
      req(nzchar(Aname_clean), nzchar(Aacronym_clean))
      with_auto_backup("before_add_fishgear", NULL)
      
      dbExecute(db,
                "INSERT INTO Fishgear (Aname, Aacronym, Atype) VALUES (?, ?, ?)",
                params = list(Aname_clean, Aacronym_clean, Atype_clean))
      
      updateTextInput(session, "Aname", value = "")
      updateTextInput(session, "Aacronym", value = "")
      updateTextInput(session, "Atype", value = "")
      refresh_dropdowns()
      output$gear_msg <- renderText("Fishing gear added successfully.")
      
    }, error = function(e) {
      output$gear_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_specie, {
    tryCatch({
      require_login()
      Sname_1_clean <- clean_text(input$Sname_1)
      Sname_2_clean <- clean_text(input$Sname_2)
      Scientific_clean <- clean_text(input$Scientific)
      req(nzchar(Scientific_clean))
      with_auto_backup("before_add_specie", NULL)
      
      dbExecute(db,
                "INSERT INTO Specie (Sname_1, Sname_2, Scientific) VALUES (?, ?, ?)",
                params = list(Sname_1_clean, Sname_2_clean, Scientific_clean))
      
      updateTextInput(session, "Sname_1", value = "")
      updateTextInput(session, "Sname_2", value = "")
      updateTextInput(session, "Scientific", value = "")
      refresh_dropdowns()
      output$specie_msg <- renderText("Specie added successfully.")
      
    }, error = function(e) {
      output$specie_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$add_vessel, {
    tryCatch({
      require_login()
      Vname_clean <- clean_text(input$Vname)
      Vowner_clean <- clean_text(input$Vowner)
      req(input$UFcode_v != "", input$Mcode_v != "", input$Lcode_v != "")
      req(nzchar(Vname_clean), input$Bacronym_v != "")
      require_reference("SELECT 1 FROM Local WHERE Lcode = ? AND Mcode = ? AND UFcode = ?", list(input$Lcode_v, input$Mcode_v, input$UFcode_v), "Selected Local/Municipality/State combination is invalid.")
      require_reference("SELECT 1 FROM Boat WHERE Bacronym = ?", list(input$Bacronym_v), "Selected Boat type does not exist.")
      with_auto_backup("before_add_vessel", NULL)
      
      dbExecute(db,
                "INSERT INTO Vessel 
                 (UFcode, Mcode, Lcode, Vname, Bacronym, Vowner, Vlength, Vpropul)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(input$UFcode_v, input$Mcode_v, input$Lcode_v,
                              Vname_clean, input$Bacronym_v, Vowner_clean,
                              input$Vlength, input$Vpropul))
      
      updateTextInput(session, "Vname", value = "")
      updateTextInput(session, "Vowner", value = "")
      updateNumericInput(session, "Vlength", value = 0)
      updateNumericInput(session, "Vpropul", value = 0)
      output$vessel_msg <- renderText("Vessel added successfully.")
      
    }, error = function(e) {
      output$vessel_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  # ======================================
  # TripData + LandData
  # ======================================
  
  species_trip <- reactiveVal(data.frame(
    Scode = integer(),
    Scientific = character(),
    Quantity = numeric(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add_species, {
    tryCatch({
      req(input$scode != "", input$quantity > 0)
      
      sp_data <- get_species()
      sp_name <- sp_data$Scientific[sp_data$Scode == input$scode]
      
      new_row <- data.frame(
        Scode = as.integer(input$scode),
        Scientific = sp_name,
        Quantity = input$quantity,
        stringsAsFactors = FALSE
      )
      
      species_trip(bind_rows(species_trip(), new_row))
      output$trip_msg <- renderText("Species added to temporary table.")
      
    }, error = function(e) {
      output$trip_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$clear_species, {
    species_trip(data.frame(
      Scode = integer(),
      Scientific = character(),
      Quantity = numeric(),
      stringsAsFactors = FALSE
    ))
    output$trip_msg <- renderText("Temporary species table cleared.")
  })
  
  output$species_table <- renderDT({
    datatable(
      species_trip(),
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  observeEvent(input$new_trip, {
    tryCatch({
      require_login()
      Vname_clean <- clean_text(input$vname)
      req(nzchar(Vname_clean))
      req(input$UFcode_trip != "", input$Mcode_trip != "", input$Lcode_trip != "")
      req(input$bacronym_trip != "", input$aacronym_trip != "")
      req(nrow(species_trip()) > 0)
      require_reference("SELECT 1 FROM Boat WHERE Bacronym = ?", list(input$bacronym_trip), "Selected Boat type does not exist.")
      require_reference("SELECT 1 FROM Fishgear WHERE Aacronym = ?", list(input$aacronym_trip), "Selected Fishing Gear does not exist.")
      for (scode_i in species_trip()$Scode) {
        require_reference("SELECT 1 FROM Specie WHERE Scode = ?", list(scode_i), paste("Species code", scode_i, "does not exist."))
      }
      
      if (input$retu_date < input$dept_date) {
        stop("Return date cannot be before departure date.")
      }
      
      labels <- get_trip_location_labels(
        UFcode = input$UFcode_trip,
        Mcode = input$Mcode_trip,
        Lcode = input$Lcode_trip
      )
      
      UF_acronym <- clean_text(labels$UFacronym)
      municipality_name <- clean_text(labels$Mname)
      local_name <- clean_text(labels$Lname)
      
      with_auto_backup("before_add_trip", NULL)
      dbBegin(db)
      
      # IMPORTANT:
      # This assumes that TripData has columns UFcode, Mcode and Lcode
      # that can store text values.
      # In this improved version:
      # UFcode receives the state acronym, for example CE.
      # Mcode receives the municipality name, for example FORTALEZA.
      # Lcode receives the local name, for example MUCURIPE.
      
      dbExecute(db,
                "INSERT INTO TripData 
                 (UFcode, Mcode, Lcode, Vname, Bacronym, Aacronym, Dept_date, Retu_date)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(UF_acronym, municipality_name, local_name,
                              Vname_clean, input$bacronym_trip,
                              input$aacronym_trip,
                              as.character(input$dept_date),
                              as.character(input$retu_date)))
      
      new_trip <- dbGetQuery(db, "SELECT last_insert_rowid() AS Tcode")$Tcode[1]
      
      df <- species_trip()
      
      for (i in seq_len(nrow(df))) {
        dbExecute(db,
                  "INSERT INTO LandData (Tcode, Scode, Quantity)
                   VALUES (?, ?, ?)",
                  params = list(new_trip, df$Scode[i], df$Quantity[i]))
      }
      
      dbCommit(db)
      
      species_trip(data.frame(
        Scode = integer(),
        Scientific = character(),
        Quantity = numeric(),
        stringsAsFactors = FALSE
      ))
      
      updateTextInput(session, "vname", value = "")
      output$trip_msg <- renderText(paste("TripData and LandData added successfully. Tcode:", new_trip))
      
    }, error = function(e) {
      if (dbIsValid(db)) {
        try(dbRollback(db), silent = TRUE)
      }
      output$trip_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  # ======================================
  # Samples: LandSample and LandBiom (FishGround is now in Cadast)
  # ======================================
  
  observeEvent(input$add_fishground, {
    tryCatch({
      require_login()
      FGname_clean <- clean_text(input$FGname)
      Reference_clean <- clean_text(input$FGref)
      req(nzchar(FGname_clean))
      req(input$FGbottom != "")
      with_auto_backup("before_add_fishground", NULL)
      
      dbExecute(db,
                "INSERT INTO FishGround (FGname, Depth, BottomType, Latitude, Longitude, Reference)
                 VALUES (?, ?, ?, ?, ?, ?)",
                params = list(FGname_clean, input$FGdepth, input$FGbottom,
                              input$FGlat, input$FGlon, Reference_clean))
      
      updateTextInput(session, "FGname", value = "")
      updateNumericInput(session, "FGdepth", value = 0)
      updateSelectInput(session, "FGbottom", selected = "")
      updateTextInput(session, "FGlat", value = "")
      updateTextInput(session, "FGlon", value = "")
      updateTextInput(session, "FGref", value = "")
      
      refresh_dropdowns()
      refresh_fishground_sample_dropdown(selected = FGname_clean)
      output$fishground_msg <- renderText("FishGround added successfully. It is now available in the Sample Data fishing ground list.")
      
    }, error = function(e) {
      output$fishground_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  biom_sample <- reactiveVal(data.frame(
    Scientific = character(),
    Sex = character(),
    Tlength = character(),
    Vlength = numeric(),
    Vweight = numeric(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add_biom, {
    tryCatch({
      req(input$scientific_biom != "")
      req(input$sex_biom != "")
      req(input$length_type_biom != "")
      req(input$length_biom > 0)
      
      new_row <- data.frame(
        Scientific = input$scientific_biom,
        Sex = input$sex_biom,
        Tlength = input$length_type_biom,
        Vlength = input$length_biom,
        Vweight = input$weight_biom,
        stringsAsFactors = FALSE
      )
      
      biom_sample(bind_rows(biom_sample(), new_row))
      output$sample_msg <- renderText("Biometric record added to temporary table.")
      
    }, error = function(e) {
      output$sample_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  observeEvent(input$clear_biom, {
    biom_sample(data.frame(
      Scientific = character(),
      Sex = character(),
      Tlength = character(),
      Vlength = numeric(),
      Vweight = numeric(),
      stringsAsFactors = FALSE
    ))
    output$sample_msg <- renderText("Temporary biometric table cleared.")
  })
  
  output$biom_table <- renderDT({
    datatable(
      biom_sample(),
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  observeEvent(input$new_sample, {
    tryCatch({
      require_login()
      req(input$UFcode_sample != "", input$Mcode_sample != "", input$Lcode_sample != "")
      req(input$bacronym_sample != "", input$aacronym_sample != "")
      req(input$fgname_sample != "")
      req(nrow(biom_sample()) > 0)
      require_reference("SELECT 1 FROM Boat WHERE Bacronym = ?", list(input$bacronym_sample), "Selected Boat type does not exist.")
      require_reference("SELECT 1 FROM Fishgear WHERE Aacronym = ?", list(input$aacronym_sample), "Selected Fishing Gear does not exist.")
      require_reference("SELECT 1 FROM FishGround WHERE FGname = ?", list(input$fgname_sample), "Selected FishGround does not exist.")
      for (sp_i in biom_sample()$Scientific) {
        require_reference("SELECT 1 FROM Specie WHERE Scientific = ?", list(sp_i), paste("Species", sp_i, "does not exist."))
      }
      
      labels <- get_trip_location_labels(
        UFcode = input$UFcode_sample,
        Mcode = input$Mcode_sample,
        Lcode = input$Lcode_sample
      )
      
      UF_acronym <- clean_text(labels$UFacronym)
      municipality_name <- clean_text(labels$Mname)
      local_name <- clean_text(labels$Lname)
      
      with_auto_backup("before_add_sample", NULL)
      dbBegin(db)
      
      dbExecute(db,
                "INSERT INTO LandSample
                 (UFacronym, Mname, Lname, SampleDate, Bacronym, Aacronym, FGname)
                 VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(UF_acronym, municipality_name, local_name,
                              as.character(input$sample_date),
                              input$bacronym_sample, input$aacronym_sample,
                              input$fgname_sample))
      
      new_sample <- dbGetQuery(db, "SELECT last_insert_rowid() AS LScode")$LScode[1]
      
      df <- biom_sample()
      
      for (i in seq_len(nrow(df))) {
        dbExecute(db,
                  "INSERT INTO LandBiom
                   (LScode, Scientific, Sex, Tlength, Vlength, Vweight)
                   VALUES (?, ?, ?, ?, ?, ?)",
                  params = list(new_sample, df$Scientific[i], df$Sex[i],
                                df$Tlength[i], df$Vlength[i], df$Vweight[i]))
      }
      
      dbCommit(db)
      
      biom_sample(data.frame(
        Scientific = character(),
        Sex = character(),
        Tlength = character(),
        Vlength = numeric(),
        Vweight = numeric(),
        stringsAsFactors = FALSE
      ))
      
      output$sample_msg <- renderText(paste("LandSample and LandBiom added successfully. LScode:", new_sample))
      
    }, error = function(e) {
      if (dbIsValid(db)) {
        try(dbRollback(db), silent = TRUE)
      }
      output$sample_msg <- renderText(paste("Error:", e$message))
    })
  })
  
  # ======================================
  # Data viewer and export
  # ======================================
  
  table_data <- reactiveVal(data.frame())
  
  load_selected_table <- function() {
    if (input$table_select == "LandSample + LandBiom") {
      dbGetQuery(db, "
        SELECT
          LandSample.LScode,
          LandSample.UFacronym AS State,
          LandSample.Mname AS Municipality,
          LandSample.Lname AS Local,
          LandSample.SampleDate,
          LandSample.Bacronym,
          Boat.Bname,
          LandSample.Aacronym,
          Fishgear.Aname,
          LandSample.FGname,
          FishGround.Depth,
          FishGround.BottomType,
          LandBiom.LBcode,
          LandBiom.Scientific,
          LandBiom.Sex,
          LandBiom.Tlength,
          LandBiom.Vlength,
          LandBiom.Vweight
        FROM LandSample
        LEFT JOIN LandBiom
          ON LandSample.LScode = LandBiom.LScode
        LEFT JOIN Boat
          ON LandSample.Bacronym = Boat.Bacronym
        LEFT JOIN Fishgear
          ON LandSample.Aacronym = Fishgear.Aacronym
        LEFT JOIN FishGround
          ON LandSample.FGname = FishGround.FGname
        ORDER BY LandSample.LScode, LandBiom.LBcode
      ")
    } else if (input$table_select == "TripData + LandData") {
      dbGetQuery(db, "
        SELECT 
          TripData.Tcode,
          TripData.UFcode AS State,
          TripData.Mcode AS Municipality,
          TripData.Lcode AS Local,
          TripData.Vname,
          TripData.Bacronym,
          Boat.Bname,
          TripData.Aacronym,
          Fishgear.Aname,
          TripData.Dept_date,
          TripData.Retu_date,
          LandData.Scode,
          Specie.Scientific,
          LandData.Quantity
        FROM TripData
        LEFT JOIN LandData 
          ON TripData.Tcode = LandData.Tcode
        LEFT JOIN Specie
          ON LandData.Scode = Specie.Scode
        LEFT JOIN Boat
          ON TripData.Bacronym = Boat.Bacronym
        LEFT JOIN Fishgear
          ON TripData.Aacronym = Fishgear.Aacronym
        ORDER BY TripData.Tcode, LandData.Scode
      ")
    } else {
      # rowid_internal is used only to identify the exact SQLite row to edit.
      # It is hidden in the table display.
      dbGetQuery(db, paste("SELECT rowid AS rowid_internal, * FROM", input$table_select))
    }
  }
  
  observeEvent(input$refresh_table, {
    tryCatch({
      table_data(load_selected_table())
    }, error = function(e) {
      table_data(data.frame(Error = e$message))
    })
  })
  
  observeEvent(input$table_select, {
    tryCatch({
      table_data(load_selected_table())
    }, error = function(e) {
      table_data(data.frame(Error = e$message))
    })
  })
  
  output$table_view <- renderDT({
    df <- table_data()
    
    hide_cols <- NULL
    if ("rowid_internal" %in% names(df)) {
      hide_cols <- which(names(df) == "rowid_internal") - 1
    }
    
    datatable(
      df,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(visible = FALSE, targets = hide_cols)
        )
      ),
      rownames = FALSE,
      filter = "top",
      selection = "single"
    )
  })
  
  # ======================================
  # Edit selected row
  # ======================================
  
  observeEvent(input$edit_selected, {
    tryCatch({
      require_login()
      require_admin()
      req(input$table_select %in% editable_tables)
      
      selected <- input$table_view_rows_selected
      if (length(selected) != 1) {
        showNotification("Please select exactly one row to edit.", type = "warning")
        return()
      }
      
      df <- table_data()
      row <- df[selected, , drop = FALSE]
      table_name <- input$table_select
      
      fields_to_edit <- setdiff(
        names(df),
        c("rowid_internal", protected_fields(table_name))
      )
      
      input_controls <- lapply(fields_to_edit, function(field) {
        value <- row[[field]][1]
        
        if (is.numeric(df[[field]])) {
          numericInput(
            inputId = paste0("edit_", field),
            label = field,
            value = value
          )
        } else {
          textInput(
            inputId = paste0("edit_", field),
            label = field,
            value = as.character(value)
          )
        }
      })
      
      showModal(modalDialog(
        title = paste("Edit", table_name),
        size = "l",
        easyClose = TRUE,
        do.call(tagList, input_controls),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_edit", "Save changes", class = "btn-primary")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Edit error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$save_edit, {
    tryCatch({
      require_login()
      require_admin()
      req(input$table_select %in% editable_tables)
      
      selected <- input$table_view_rows_selected
      req(length(selected) == 1)
      
      df <- table_data()
      row <- df[selected, , drop = FALSE]
      table_name <- input$table_select
      
      req("rowid_internal" %in% names(df))
      rowid_value <- row$rowid_internal[1]
      
      fields_to_edit <- setdiff(
        names(df),
        c("rowid_internal", protected_fields(table_name))
      )
      
      if (length(fields_to_edit) == 0) {
        stop("There are no editable fields in this table.")
      }
      
      new_values <- lapply(fields_to_edit, function(field) {
        value <- input[[paste0("edit_", field)]]
        
        if (is.numeric(df[[field]])) {
          as.numeric(value)
        } else {
          clean_text(as.character(value))
        }
      })
      
      set_sql <- paste(
        paste0(dbQuoteIdentifier(db, fields_to_edit), " = ?"),
        collapse = ", "
      )
      
      sql <- paste0(
        "UPDATE ", dbQuoteIdentifier(db, table_name),
        " SET ", set_sql,
        " WHERE rowid = ?"
      )
      
      with_auto_backup(paste0("before_edit_", table_name), NULL)
      dbExecute(db, sql, params = c(new_values, list(rowid_value)))
      
      removeModal()
      table_data(load_selected_table())
      refresh_dropdowns()
      showNotification("Record updated successfully.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Save error:", e$message), type = "error", duration = 8)
    })
  })
  
  # ======================================
  # Delete selected row
  # ======================================
  
  observeEvent(input$delete_selected, {
    tryCatch({
      require_login()
      require_admin()
      req(input$table_select %in% editable_tables)
      
      selected <- input$table_view_rows_selected
      if (length(selected) != 1) {
        showNotification("Please select exactly one row to delete.", type = "warning")
        return()
      }
      
      df <- table_data()
      row <- df[selected, , drop = FALSE]
      req("rowid_internal" %in% names(df))
      
      showModal(modalDialog(
        title = "Confirm deletion",
        paste("Are you sure you want to delete this record from", input$table_select, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Delete", class = "btn-danger")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Delete error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$confirm_delete, {
    tryCatch({
      require_login()
      require_admin()
      req(input$table_select %in% editable_tables)
      
      selected <- input$table_view_rows_selected
      req(length(selected) == 1)
      
      df <- table_data()
      row <- df[selected, , drop = FALSE]
      table_name <- input$table_select
      rowid_value <- row$rowid_internal[1]
      
      with_auto_backup(paste0("before_delete_", table_name), NULL)
      # If deleting a TripData record, delete associated LandData first.
      # This avoids orphan landing records.
      if (table_name == "TripData" && "Tcode" %in% names(row)) {
        dbBegin(db)
        dbExecute(db, "DELETE FROM LandData WHERE Tcode = ?", params = list(row$Tcode[1]))
        dbExecute(db, paste0("DELETE FROM ", dbQuoteIdentifier(db, table_name), " WHERE rowid = ?"),
                  params = list(rowid_value))
        dbCommit(db)
      } else if (table_name == "LandSample" && "LScode" %in% names(row)) {
        dbBegin(db)
        dbExecute(db, "DELETE FROM LandBiom WHERE LScode = ?", params = list(row$LScode[1]))
        dbExecute(db, paste0("DELETE FROM ", dbQuoteIdentifier(db, table_name), " WHERE rowid = ?"),
                  params = list(rowid_value))
        dbCommit(db)
      } else {
        dbExecute(db, paste0("DELETE FROM ", dbQuoteIdentifier(db, table_name), " WHERE rowid = ?"),
                  params = list(rowid_value))
      }
      
      removeModal()
      table_data(load_selected_table())
      refresh_dropdowns()
      showNotification("Record deleted successfully.", type = "message")
      
    }, error = function(e) {
      if (dbIsValid(db)) {
        try(dbRollback(db), silent = TRUE)
      }
      showNotification(paste("Delete error:", e$message), type = "error", duration = 8)
    })
  })
  
  # ======================================
  # Reports
  # ======================================
  
  report_species_data <- reactiveVal(data.frame())
  report_vessel_data <- reactiveVal(data.frame())
  report_month_data <- reactiveVal(data.frame())
  report_sample_species_data <- reactiveVal(data.frame())
  report_biom_summary_data <- reactiveVal(data.frame())
  report_fishground_data <- reactiveVal(data.frame())
  
  observeEvent(input$generate_report, {
    tryCatch({
      req(input$report_date_from, input$report_date_to)
      
      date_from <- as.character(input$report_date_from)
      date_to <- as.character(input$report_date_to)
      
      if (input$report_date_to < input$report_date_from) {
        stop("Final date cannot be before initial date.")
      }
      
      species_df <- dbGetQuery(db, "
        SELECT
          Specie.Scientific,
          SUM(LandData.Quantity) AS Total_quantity
        FROM LandData
        LEFT JOIN TripData ON LandData.Tcode = TripData.Tcode
        LEFT JOIN Specie ON LandData.Scode = Specie.Scode
        WHERE TripData.Dept_date BETWEEN ? AND ?
        GROUP BY Specie.Scientific
        ORDER BY Total_quantity DESC
      ", params = list(date_from, date_to))
      
      vessel_df <- dbGetQuery(db, "
        SELECT
          TripData.Vname,
          SUM(LandData.Quantity) AS Total_quantity
        FROM LandData
        LEFT JOIN TripData ON LandData.Tcode = TripData.Tcode
        WHERE TripData.Dept_date BETWEEN ? AND ?
        GROUP BY TripData.Vname
        ORDER BY Total_quantity DESC
      ", params = list(date_from, date_to))
      
      month_df <- dbGetQuery(db, "
        SELECT
          substr(TripData.Dept_date, 1, 7) AS Month,
          SUM(LandData.Quantity) AS Total_quantity
        FROM LandData
        LEFT JOIN TripData ON LandData.Tcode = TripData.Tcode
        WHERE TripData.Dept_date BETWEEN ? AND ?
        GROUP BY substr(TripData.Dept_date, 1, 7)
        ORDER BY Month
      ", params = list(date_from, date_to))
      
      report_species_data(species_df)
      report_vessel_data(vessel_df)
      report_month_data(month_df)
      
      sample_species_df <- dbGetQuery(db, "
        SELECT
          LandBiom.Scientific,
          COUNT(*) AS Number_of_biometrics,
          COUNT(DISTINCT LandSample.LScode) AS Number_of_samples
        FROM LandBiom
        LEFT JOIN LandSample ON LandBiom.LScode = LandSample.LScode
        WHERE LandSample.SampleDate BETWEEN ? AND ?
        GROUP BY LandBiom.Scientific
        ORDER BY Number_of_biometrics DESC
      ", params = list(date_from, date_to))
      
      biom_summary_df <- dbGetQuery(db, "
        SELECT
          LandBiom.Scientific,
          LandBiom.Sex,
          LandBiom.Tlength,
          COUNT(*) AS N,
          ROUND(AVG(LandBiom.Vlength), 2) AS Mean_length_mm,
          ROUND(MIN(LandBiom.Vlength), 2) AS Min_length_mm,
          ROUND(MAX(LandBiom.Vlength), 2) AS Max_length_mm,
          ROUND(AVG(LandBiom.Vweight), 2) AS Mean_weight_g
        FROM LandBiom
        LEFT JOIN LandSample ON LandBiom.LScode = LandSample.LScode
        WHERE LandSample.SampleDate BETWEEN ? AND ?
        GROUP BY LandBiom.Scientific, LandBiom.Sex, LandBiom.Tlength
        ORDER BY LandBiom.Scientific, LandBiom.Sex, LandBiom.Tlength
      ", params = list(date_from, date_to))
      
      fishground_df <- dbGetQuery(db, "
        SELECT
          LandSample.FGname,
          FishGround.Depth,
          FishGround.BottomType,
          COUNT(DISTINCT LandSample.LScode) AS Number_of_samples,
          COUNT(LandBiom.LBcode) AS Number_of_biometrics
        FROM LandSample
        LEFT JOIN LandBiom ON LandSample.LScode = LandBiom.LScode
        LEFT JOIN FishGround ON LandSample.FGname = FishGround.FGname
        WHERE LandSample.SampleDate BETWEEN ? AND ?
        GROUP BY LandSample.FGname, FishGround.Depth, FishGround.BottomType
        ORDER BY Number_of_samples DESC
      ", params = list(date_from, date_to))
      
      report_sample_species_data(sample_species_df)
      report_biom_summary_data(biom_summary_df)
      report_fishground_data(fishground_df)
      
    }, error = function(e) {
      showNotification(paste("Report error:", e$message), type = "error", duration = 8)
    })
  })
  
  output$report_species <- renderDT({
    datatable(report_species_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$report_vessel <- renderDT({
    datatable(report_vessel_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$report_month <- renderDT({
    datatable(report_month_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$report_sample_species <- renderDT({
    datatable(report_sample_species_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$report_biom_summary <- renderDT({
    datatable(report_biom_summary_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$report_fishground <- renderDT({
    datatable(report_fishground_data(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$table_select), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- table_data()
      if ("rowid_internal" %in% names(df)) {
        df$rowid_internal <- NULL
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$export_xlsx <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$table_select), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("The package 'writexl' is required for Excel export. Install it once with: install.packages('writexl')")
      }
      df <- table_data()
      if ("rowid_internal" %in% names(df)) {
        df$rowid_internal <- NULL
      }
      writexl::write_xlsx(df, path = file)
    }
  )
  
  observeEvent(input$manual_backup, {
    tryCatch({
      require_login()
      backup_file <- backup_database("manual")
      output$backup_msg <- renderText(paste("Backup created:", backup_file))
      showNotification("Backup created successfully.", type = "message")
    }, error = function(e) {
      output$backup_msg <- renderText(paste("Backup error:", e$message))
      showNotification(paste("Backup error:", e$message), type = "error", duration = 8)
    })
  })
}

# ======================================
# RUN APP
# ======================================

shinyApp(ui, server)

