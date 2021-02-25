

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

## checkboxes for active status
active_switch_for_each_cell <- function(id, selected){
  
  function(i){
    selected_yes = selected_no = ""
    if (selected[i] == "Yes") selected_yes = "selected"
    else selected_no = "selected"
    sprintf(
      '<div class="form-group shiny-input-container">
        <label class="control-label" for="id-%d"></label>
        <div width="25px">
          <select onclick="%s" id="id-%d"><option value="No" %s>No</option>
      <option value="Yes" %s>Yes</option></select>
          <script type="application/json" data-for="id-%d" data-nonempty="">{}</script>
        </div>
      </div>',
      i, sprintf("Shiny.setInputValue('%s', this.id + '_' + this.value);", id), i, selected_no, selected_yes, i)
  }
}


## shinyjs code to disable buttons for the row clicked
jsCode <- "shinyjs.disabled = function(button){document.getElementById(button).disabled = true;}"


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



# update text inputs
update_text_inputs <- function(session) {
  updateTextInput(
    session = session,
    inputId = "gatewayid",
    label = "",
    value = "",
    placeholder = "Required"
  )
  updateTextInput(
    session = session,
    inputId = "interfaceid",
    label = "",
    value = "",
    placeholder = "Required"
  )
  updateTextInput(
    session = session,
    inputId = "serialid",
    label = "",
    value = "",
    placeholder = "Required"
  )
  updateTextInput(
    session = session,
    inputId = "siteid",
    label = "",
    value = "",
    placeholder = "Required"
  )
  updateTextInput(
    session = session,
    inputId = "bed",
    label = "",
    value = "",
    placeholder = "Optional"
  )
}





