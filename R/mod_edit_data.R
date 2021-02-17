#' edit_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_edit_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "ui grid",
      div(
        class = "twelve wide column",
        div(
          class = "content",
          DT::DTOutput(ns("table"))
        ),
        div(
          class = "content",
          style = "text-align: center;",
          shiny.semantic::action_button(
            input_id = ns("add_row"),
            label = "Add Row",
            class = "ui blue mini button"
          )
        )
      )
    )
  )
}
    
#' edit_data Server Function
#'
#' @noRd 
mod_edit_data_server <- function(id, r){
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      r_this_mod = reactiveValues(
        data = NULL,
        edited_data = NULL,
        i = NULL,
        edited_col = NULL,
        edited_val = NULL,
        edited_bed_location = NULL,
        edits = 0
      )
      
      observeEvent(r$data, {
        r_this_mod$data = r$data
      })
      
      # observeEvent(input$button, {
      #   print("!!!")
      #   
      # }, ignoreNULL = TRUE)
      
      output$table <- DT::renderDT({
        
        data = cbind(
          r_this_mod$data,
          submit = sapply(1:nrow(r_this_mod$data), button_for_each_cell("submit", "Submit", ns("button")))
          #button_wrong = sapply(1:nrow(r$data), button_for_each_cell("wrong", "#D1D3D4", "Wrong", ns("button")))
          #weight = list(list(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))
        )
        #print(data)
        #data = r$data
        data %>% 
          # dplyr::select(
          #   bed_location, sigposture, offload_time_in_mins, sigmoisture,
          #   time_exposed_to_moisture, submit
          # ) %>% 
          DT::datatable(
            escape = 15,
            editable = "row",
            selection = "none",
            options = list(pageLength = 5)
          )
      })
      
      #observeEvent(input$table_rows_selected, print(input$table_rows_selected))
      
      proxy = DT::dataTableProxy("table", session = session)
      
      observeEvent(input$table_cell_edit, {
        req(is.null(r_this_mod$edited_val))
        #input$button = NULL
        
        # data = r$data %>% 
        #   dplyr::select(bed_location, sigposture, offload_time_in_mins, sigmoisture, time_exposed_to_moisture)
        # 
        colnames_data = colnames(r_this_mod$data)
        
        #edited_data = r_this_mod$data
        
        info = input$table_cell_edit[0:15, ]
        #str(info)
        print(info)
        #info$col = info$col + 1
        i = info$row[1]
        j = info$col
        v = info$value
        
        
        #edited_col = colnames_data[j]
        #edited_data[i] = DT::coerceValue(v, edited_data[i])
        r_this_mod$data = DT::editData(r_this_mod$data, info, "table")
        
        r_this_mod$i = i
        #r_this_mod$edited_col = edited_col
        #r_this_mod$edited_val = v
        #r_this_mod$edited_bed_location = edited_data$bed_location[i]
        
        
        #print(edited_data)
        #print(r_this_mod$edited_val)
        #shinyjs::enable(id = paste0("button_submit_", i))
        button = paste0("button_submit_", i)
        print(button)
        golem::invoke_js("updateSelectedRowButton", list(button = button, value = FALSE))
        
        r_this_mod$edited_data = r_this_mod$data
        
        print(r_this_mod$data)
        
        #DT::replaceData(proxy, x, resetPaging = FALSE)  # important
      })
      
      observeEvent(input$add_row, {
        new_row = NA
        r$data = rbind(new_row, r$data)
      })
      
      observeEvent(input$button, {
        req(r_this_mod$i)
        
        
        cat("r_this_mod$i: ")
        print(r_this_mod$i)
        
        splitted <- strsplit(input$button, "_")[[1]]
        row <- splitted[3]
        cat("row: ")
        print(row)
        
        r_this_mod$edits = r_this_mod$edits + 1
        
        if (r_this_mod$i == row) {
          query = sprintf("UPDATE curiato_enduser_dashboard_prod
                          SET timestamp = NOW(), %s = '%s'
                          WHERE bed_location = '%s';",
                         r_this_mod$edited_col, r_this_mod$edited_val, r_this_mod$edited_bed_location)
          print(query)
          #DBI::dbSendStatement(r$con, query)
          
        }
        
        r_this_mod$edited_val = NULL
        
        
        
      })
      
      observeEvent(r_this_mod$edits, {
        golem::invoke_js("updateButtons", list(value = TRUE))
        golem::invoke_js("updateButton", list(button = ns("button")))
      })
      
    }
  )
  
  
  
 
}
    
## To be copied in the UI
# mod_edit_data_ui("edit_data_ui_1")
    
## To be copied in the server
# callModule(mod_edit_data_server, "edit_data_ui_1")
 
