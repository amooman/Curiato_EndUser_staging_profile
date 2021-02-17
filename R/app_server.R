#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  
  data1 <- tibble::tribble(
    ~Location, ~Posture, ~Time_last_reposition, ~Moisture, ~Time_expose_moisture,
    "301-1", "Fowler", lubridate::hm("3:04"), "Dry", "00:00",
    "301-2", "Supine", lubridate::hm("1:21"), "Wet", "02:45",
    "305-2", "Supine", lubridate::hm("0:26"), "Wet", "00:00",
    "306-1", "Seated", lubridate::hm("1:53"), "Dry", "00:15",
    "311-1", "Right-lateral", lubridate::hm("2:29"), "Dry", "01:11"
  )
  
  
  r = reactiveValues(
    data = NULL,
    con = NULL,
    update_time = NULL
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
  
  # data <- reactive({
  #   print("invalidating...")
  #   mins = 3
  #   #invalidateLater((60000 * mins))
  #   r$update_time = Sys.time()
  #   
  #   dplyr::tbl(con, "cur_app_device_profile") %>% dplyr::collect() %>% 
  #     dplyr::select(
  #       curiatoid, dev_gatewayid, dev_interfaceid, dev_serialid, siteid, 
  #       bed_location, sigposture, offload_time_in_mins, sigmoisture, time_exposed_to_moisture
  #     )
  # })
  
  data <- shiny::reactivePoll(
    intervalMillis = 2000,
    session = session,
    checkFunc = function() {
      print("checking...")
      DBI::dbGetQuery(con, "SELECT MAX(timestamp) from cur_app_device_profile;")
    },
    valueFunc = function() {
      print("invalidating...")
      r$update_time = Sys.time()
      dplyr::tbl(con, "cur_app_device_profile") %>% dplyr::collect()
        # dplyr::select(
        #   curiatoid, dev_gatewayid, dev_interfaceid, dev_serialid, siteid, 
        #   bed_location, sigposture, offload_time_in_mins, sigmoisture, time_exposed_to_moisture
        # )
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
