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
    uiOutput(ns("modal")),
    div(
      class = "ui raised segment",
      div(
        class = "content",
        DT::DTOutput(ns("table"))
      ),
      br(),
      div(
        class = "content",
        style = "text-align: center;",
        shiny.semantic::action_button(
          input_id = ns("add_row"),
          label = "Add Row",
          class = "ui black mini button",
          icon = icon("plus")
        )
      ),
      div(
        class = "ui grid",
        div(
          class = "thirteen wide column"
        ),
        div(
          class = "three wide column",
          div(
            tags$ul(
              tags$li("Double-click on a row to start"),
              tags$li("Ctrl + Enter to end")
            )
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
        info = NULL,
        active_status = NULL
      )
      
      #r$w = waiter::Waiter$new(id = "table")
      
      output$modal <- renderUI({
        shiny.semantic::modal(
          id = ns("add_row_modal"),
          header = h4("Add New Row", style = "text-align: center;"),
          footer = "",
          class = "tiny",
          settings = list(c("transition", "fade"), c("closable", "false"), c("autofocus", "false")),
          #modal_tags = icon("ui close"),
          shiny.semantic::form(
            
            shiny.semantic::field(
              style = "text-align: center;",
              tags$label("Gateway ID"),
              textInput(
                inputId = ns("gatewayid"),
                label = "",
                placeholder = "Required",
                width = "100%"
              )
            ),
            shiny.semantic::field(
              style = "text-align: center;",
              tags$label("Interface ID"),
              textInput(
                inputId = ns("interfaceid"),
                label = "",
                placeholder = "Required",
                width = "100%"
              )
            ),
            shiny.semantic::field(
              style = "text-align: center;",
              tags$label("Serial ID"),
              textInput(
                inputId = ns("serialid"),
                label = "",
                placeholder = "Required",
                width = "100%"
              )
            ),
            shiny.semantic::field(
              style = "text-align: center;",
              tags$label("Site ID"),
              textInput(
                inputId = ns("siteid"),
                label = "",
                placeholder = "Required",
                width = "100%"
              )
            ),
            shiny.semantic::field(
              style = "text-align: center;",
              tags$label("Bed Number"),
              textInput(
                inputId = ns("bed"),
                label = "",
                placeholder = "Optional",
                width = "100%"
              )
            )
          ),
          br(),
          div(
            class = "ui grid",
            div(
              class = "eight wide column",
              style = "text-align: right;",
              shiny.semantic::action_button(
                input_id = ns("add_row_submit"),
                label = "Done",
                class = "ui mini blue button"
              ) %>% tagAppendAttributes(
                onclick = sprintf("Shiny.setInputValue('%s', null)", ns("button"))
              ),
            ),
            div(
              class = "eight wide column",
              style = "text-align: left;",
              shiny.semantic::action_button(
                input_id = ns("add_row_cancel"),
                label = "Cancel",
                class = "ui mini red button"
              ) %>% tagAppendAttributes(
                onclick = sprintf("Shiny.setInputValue('%s', null)", ns("button"))
              ),
            )
            
          )
        )
      })
      
      observeEvent(r$data, {
        r_this_mod$data = r$data %>% 
          dplyr::arrange(dplyr::desc(timestamp))
      })
      
      output$table <- DT::renderDT({
        data = cbind(
          r_this_mod$data,
          active_sts = sapply(1:nrow(r_this_mod$data), active_switch_for_each_cell(ns("active"), r_this_mod$data$active)),
          submit = sapply(1:nrow(r_this_mod$data), button_for_each_cell("submit", "Submit", ns("button")))
        )
        #print(data)
        #data = r$data
        data %>% 
          dplyr::select(
            -active
          ) %>%
          DT::datatable(
            escape = 1:14,
            editable = list(target = "row", disable = list(columns = c(0:3, 8:10, 14, 15))),
            selection = "none",
            options = list(
              pageLength = 5,
              autoWidth = TRUE
            )
          )
      })
      
      #observeEvent(input$table_rows_selected, print(input$table_rows_selected))
      
      proxy = DT::dataTableProxy("table", session = session)
      
      observeEvent(input$table_cell_edit, {
        #req(is.null(r_this_mod$info))
        
        if (!is.null(r_this_mod$info) | !is.null(r_this_mod$active_status)) {
          golem::invoke_js("submitAlert", list(message = "Please submit the previous changes!"))
        } else {
          #colnames_data = colnames(r_this_mod$data)
          
          info = input$table_cell_edit[1:14, ]
          r_this_mod$info = info
          print(info)
          
          i = info$row[1]
          
          button = paste0("button_submit_", i)
          print(button)
          golem::invoke_js("updateSelectedRowButton", list(button = button, value = FALSE))
        }
      })
      
      observeEvent(input$active, {
        
        if (!is.null(r_this_mod$info) | !is.null(r_this_mod$active_status)) {
          golem::invoke_js("submitAlert", list(message = "Please submit the previous changes!"))
        } else {
          splitted <- strsplit(input$active, "_")[[1]]
          row = strsplit(splitted[1], "-")[[1]][2] %>% as.numeric()
          value = splitted[2]
          r_this_mod$active_status = list(row = row, value = value)
          
          button = paste0("button_submit_", row)
          if (r_this_mod$data$active[row] != value) {
            golem::invoke_js("updateSelectedRowButton", list(button = button, value = FALSE))
          } else {
            golem::invoke_js("updateSelectedRowButton", list(button = button, value = TRUE))
          }
        }
      })
      
      observeEvent(input$button, {
        
        if (!is.null(r_this_mod$active_status)) {
          #cat("active_status: ")
          #print(r_this_mod$active_status)
          active = r_this_mod$active_status
          
          if (active$value == "Yes") start_end = "start_date"
          else start_end = "end_date"
          
          
          query <- sprintf("UPDATE cur_app_device_profile
                    SET timestamp = NOW(), active = '%s', %s = NOW()
                    WHERE gatewayid = '%s';",
                    active$value, start_end, r_this_mod$data$gatewayid[active$row])
          print(query)
          DBI::dbSendStatement(r$con, query)
          
          r_this_mod$active_status = NULL
        } else if (!is.null(r_this_mod$info)) {
          
          
          splitted <- strsplit(input$button, "_")[[1]]
          row <- splitted[3] %>% as.numeric()
          
          values = r_this_mod$info$value
          
          query = sprintf("UPDATE cur_app_device_profile
                          SET bednumber = '%s', room = '%s', floor = '%s', bltid = '%s', cellid = '%s', note = '%s'
                          WHERE gatewayid = '%s';",
                          values[6], values[7], values[8], values[12], values[13], values[14], values[2])
          print(query)
          DBI::dbSendStatement(r$con, query)
          
          button = paste0("button_submit_", row)
          golem::invoke_js("updateSelectedRowButton", list(button = button, value = TRUE))
          r_this_mod$info = NULL
        }
        golem::invoke_js("updateButton", list(button = ns("button")))
      })
      # Row adding operations ---------------------------------------------------
      
      observeEvent(input$add_row, {
        shiny.semantic::show_modal(ns("add_row_modal"))
      })
      
      observeEvent(input$add_row_submit, {
        
        
        shinyFeedback::feedbackDanger("gatewayid", input$gatewayid == "", "Needed!")
        shinyFeedback::feedbackDanger("interfaceid", input$interfaceid == "", "Needed!")
        shinyFeedback::feedbackDanger("serialid", input$serialid == "", "Needed!")
        shinyFeedback::feedbackDanger("siteid", input$siteid == "", "Needed!")
        req(input$gatewayid, input$interfaceid, input$serialid, input$siteid)
        
        shiny.semantic::hide_modal(ns("add_row_modal"))
        ## DBQUERY
        query = sprintf("INSERT INTO cur_app_device_profile VALUES 
                        ('%s', '%s', '%s', %s, '%s',
                        null, null, NOW(), 'No', null, null,
                        null, null, null)",
                        input$gatewayid, input$interfaceid, input$serialid, input$siteid, input$bed)
        print(query)
        DBI::dbSendStatement(r$con, query)
        update_text_inputs(session = session)
      })
      
      observeEvent(input$add_row_cancel, {
        shiny.semantic::hide_modal(ns("add_row_modal"))
        update_text_inputs(session = session)
      })
      
    }
  )

}
    

 
