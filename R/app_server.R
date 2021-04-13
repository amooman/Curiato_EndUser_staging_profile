#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  Sys.setenv(TZ = "America/Toronto")

  r = reactiveValues(
    data = NULL,
    con = NULL,
    update_time = NULL
  )
  
  w = waiter::Waiter$new(
    id = "edit-table",
    color = waiter::transparent(.8),
    html = waiter::spin_google()
  )
  
  con = DBI::dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    user = "admin",
    password = "6YCurIatoadmin",
    host = "162.252.240.122",
    port = 5432,
    dbname = "DBC-DEV"
  )
  
  onStop(function() DBI::dbDisconnect(con))
  
  data <- shiny::reactivePoll(
    intervalMillis = 2000,
    session = session,
    checkFunc = function() {
      #print("checking...")
      DBI::dbGetQuery(con, "SELECT MAX(timestamp) from cur_app_device_profile;")
    },
    valueFunc = function() {
      #print("invalidating...")
      w$show()
      r$update_time = Sys.time()
      dplyr::tbl(con, "cur_app_device_profile") %>%
        dplyr::collect() %>% 
        dplyr::arrange(dplyr::desc(timestamp))
    }
  )
  
  observeEvent(data(), {
    print("in data()")
    r$con = con
    r$data = data()
  })
  
  output$update_time <- renderUI({
    tags$p("Last update: ", Sys.Date(), "| ", tags$b(format(r$update_time, "%I:%M %p")))
  })
  
  mod_edit_data_server("edit", r)
}
