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
        class = "ui grid",
        div(class = "six wide column"),
        div(
          class = "two wide column",
          style = "text-align: center;",
          shiny.semantic::action_button(
            input_id = ns("add_row"),
            label = "Add Row",
            class = "ui black mini button",
            icon = icon("plus")
          )
        ),
        div(
          class = "two wide column",
          style = "text-align: center;",
          shiny.semantic::action_button(
            input_id = ns("delete_row"),
            label = "Delete Row",
            class = "ui black mini button",
            icon = icon("minus")
          )
        ),
        div(class = "six wide column")
      ),
      div(
        class = "ui grid",
        div(
          class = "two wide column"
        ),
        div(
          class = "fourteen wide column",
          div(
            tags$ul(
              style = "float:right;",
              tags$li(tags$code("Double-click"), "on a row to start editing", style = "font-size: .9rem"),
              tags$li(tags$code("Enter"), "when done/", tags$code("Esc"), "to cancel", style = "font-size: .9rem"),
              tags$li(tags$code("Delete row"), "to delete selected row", style = "font-size: .9rem")
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
        r_this_mod$data = r$data
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
          dplyr::mutate(
            timestamp = as.character(timestamp),
            start_date = as.character(start_date),
            end_date = as.character(end_date)
          ) %>% 
          DT::datatable(
            escape = 1:13,
            editable = list(target = "row", disable = list(columns = c(0, 14, 15))),
            selection = "single",
            options = list(
              dom = "fti",
              pageLength = 15,
              autoWidth = TRUE,
              scrollX = TRUE,
              fontSize = "80%"
            )
          )
      })
      
      #observeEvent(input$table_rows_selected, print(input$table_rows_selected))
      
      proxy = DT::dataTableProxy("table", session = session)
      
      observeEvent(input$table_cell_edit, {
        #req(is.null(r_this_mod$info))
        
        info = input$table_cell_edit[1:14, ]
        r_this_mod$info = info
        print(info)
        
        i = info$row[1]
        r_this_mod$edited_row <- i
        
        button = paste0("button_submit_", i)
        print(button)
        golem::invoke_js("updateSelectedRowButton", list(button = button, value = FALSE))
      })
      
      observeEvent(input$active, {
        
        splitted <- strsplit(input$active, "_")[[1]]
        row = strsplit(splitted[1], "-")[[1]][2] %>% as.numeric()
        value = splitted[2]
        r_this_mod$active_row <- row
        
        selected <- paste0("id-", row)
        button = paste0("button_submit_", row)
        
        golem::invoke_js("selectButtonDisable", list(selected = selected))
        golem::invoke_js("updateSelectedRowButton", list(button = button, value = FALSE))
        
        
        r_this_mod$active_status = list(row = row, value = value)
        print(r_this_mod$active_status)
        
      })
      
      observeEvent(input$button, {
        
        splitted <- strsplit(input$button, "_")[[1]]
        row <- splitted[3] %>% as.numeric()
        
        edited_row <- r_this_mod$data[row, 1:3]
        rmarkdown::render(
          "./inst/shiny_to_shell.Rmd",
          output_file = "shiny_to_shell.html",
          params = list(data = edited_row)
        )
        
        if (!is.null(r_this_mod$active_status)) {
          golem::invoke_js("selectButtonEnable", list(value = FALSE))
          active = r_this_mod$active_status
          
          if (active$value == "Yes") start_end = "start_date"
          else start_end = "end_date"
          
          if (r_this_mod$data$active[row] != active$value) {
            query <- sprintf("UPDATE cur_app_device_profile
                    SET timestamp = NOW(), active = '%s', %s = NOW()
                    WHERE gatewayid = '%s';",
                    active$value, start_end, r_this_mod$data$gatewayid[active$row])
            print(query)
            DBI::dbSendStatement(r$con, query)
          }
          
          r_this_mod$active_status = NULL
          
        } else if (!is.null(r_this_mod$info)) {
          
          values = r_this_mod$info$value
          
          query = sprintf("UPDATE cur_app_device_profile
                          SET bednumber = '%s', room = '%s', floor = '%s', bltid = '%s', cellid = '%s', note = '%s'
                          WHERE gatewayid = '%s';",
                          values[6], values[7], values[8], values[12], values[13], values[14], values[2])
          print(query)
          DBI::dbSendStatement(r$con, query)
          
          
          r_this_mod$info = NULL
        }
        
        
        
        button = paste0("button_submit_", row)
        
        print(button)
        
        golem::invoke_js("updateSelectedRowButton", list(button = button, value = TRUE))
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
      
      
      observeEvent(input$delete_row, {
        req(input$table_rows_selected)
        selected = input$table_rows_selected
        data = r$data
        to_delete = r$data[selected, ]
        print(to_delete)
        data = data[-selected, ]
        r$data = data
        
        query = sprintf(
          "DELETE FROM cur_app_device_profile WHERE gatewayid = '%s';",
          to_delete$gatewayid
        )
        print(query)
        DBI::dbSendStatement(r$con, query)
      })
      
    }
  )

}
    

 
