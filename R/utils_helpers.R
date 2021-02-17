

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


## making buttons for the table cells
button_for_each_cell <- function(type, label, id){
  function(i){
    sprintf(
      '<button id="button_%s_%d" type="button" disabled class="ui mini red button" onclick="%s">%s</button>', 
      type, i, sprintf("Shiny.setInputValue('%s', this.id);", id), label)
  }
}

## making buttons for the table cells
menu_button_for_each_cell <- function(type = "menu"){
  function(i){
    sprintf(
      '<button id="button_%s_%d" type="button" class="ui mini button" onclick="%s"><i class="fa fa-home"></i></button>', 
      type, i, "Shiny.setInputValue('button', this.id);")
  }
}


## shinyjs code to disable buttons for the row clicked
jsCode <- "shinyjs.disabled = function(button){document.getElementById(button).disabled = true;}"

# insert_query <- "INSERT INTO public.curiato_enduser_dashboard_prod
# (dev_gatewayid, dev_interfaceid, dev_serialid, device_status, device_status_timestamp, device_status_message, 'timestamp', bed_location, siteid, sigposture, stat_mobility, stat_moisture, stat_avgh_fullbed, stat_avgh_centbed, stat_avgt_fullbed, stat_avgt_centbed, stat_avgf_full, msg_wet_status_timestamp, msg_post_status_timestamp, wet_message_final_type, wet_message_final_status, post_message_final_type, post_message_final_status, time_since_offload, offload_updated_datetime, offload_time_in_mins, offload_alert_colours, sigmoisture, time_exposed_to_moisture)
# VALUES('', '', '', '', '', '', '', '', 0, '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', 0, '', '', '');"



# insert_query <- "INSERT INTO public.cur_app_clinical_feedback
# (curiatoid, gatewayid, interfaceid, timestamp, feedback_type, bed_location, siteid, sigposture, offload_time_in_mins, offload_alert_colours, end_user_note)
# VALUES('', '', '', '', '', '', 0, '', '', '', '');"

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

insert_into_feedback_query <- function(feedback) {
  sprintf(
    "INSERT INTO public.cur_app_clinical_feedback VALUES('%s', '%s', '%s', '%s', %s, '%s', '%s', '%s', %s, '%s', '%s', '%s', '%s');",
    feedback[["curiatoid"]], feedback[["dev_gatewayid"]], feedback[["dev_interfaceid"]],
    feedback[["timestamp"]], feedback[["feedback_type"]], feedback[["bed_location"]],
    feedback[["siteid"]], feedback[["sigposture"]], feedback[["offload_time_in_mins"]],
    feedback[["offload_alert_colours"]], feedback[["end_user_note"]],
    feedback[["dev_serialid"]], feedback[["sigmoisture"]]
  )
}

# curiatoid, dev_gatewayid, dev_inerfaceid, dev_serialid, timestamp, feedback_type, 
# bed_location, siteid, sigposture, offload_time_in_mins, offload_alert_colours, 
# end_user_note, sigmoisture









