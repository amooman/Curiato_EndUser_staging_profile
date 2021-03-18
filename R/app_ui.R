#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    
    
    shiny.semantic::semanticPage(
      
      
      shiny.semantic::grid(
        myGridTemplate,
        header = div(
          class = "ui grid",
          div(
            class = "sixteen wide column",
            div(
              class = "ui segment",
              style = "background: #D1D3D4;",
              div(
                class = "ui grid",
                div(
                  class = "twelve wide column",
                  tags$img(src = "./www/logo.png", height = "45px", width = "200px")
                ),
                div(
                  class = "four wide column",
                  style = "padding-left: 1.65rem;",
                  uiOutput("update_time")
                )
              )
            )
          )
        ),
        
        main = mod_edit_data_ui("edit"),
        
        # main = shiny.semantic::tabset(
        #   tabs = list(
        #     list(
        #       menu = "Data",
        #       content = mod_main_functionality_ui("main"),
        #       id = "data"
        #     ),
        #     list(
        #       menu = "Edit",
        #       content = mod_edit_data_ui("edit"),
        #       id = "edit"
        #     )
        #     
        #   )
        # ),
        footer = div(
          class = "ui grid",
          style = "margin-top: 7px;",
          div(
            class = "eight wide column",
            div(
              class = "footer",
              "Smart Surface Monitoring Platform"
            )
          ),
          div(
            class = "eight wide column",
            div(
              class = "footer",
              style = "float:right;",
              "©2021 Curiato, Inc. All Rights Reserved."
            )
          )
        )
      )
      
      
      
      
      # div(
      #   class = "ui grid",
      #   # div(
      #   #   class = "one wide column"
      #   # ),
      #   div(
      #     class = "sixteen wide column",
      #     div(
      #       class = "ui segment",
      #       style = "background: #D1D3D4;",
      #       div(
      #         class = "ui grid",
      #         div(
      #           class = "twelve wide column",
      #           tags$img(src = "./www/logo.png", height = "45px", width = "200px")
      #         ),
      #         div(
      #           class = "four wide column",
      #           style = "padding-left: 1.65rem;",
      #           uiOutput("update_time")
      #         )
      #       )
      #     )
      #   )
      # ),
      
      
      # shiny.semantic::tabset(
      #   tabs = list(
      #     list(
      #       menu = "Profile",
      #       content = mod_edit_data_ui("edit"),
      #       id = "edit"
      #     )
      #     
      #   )
      # )
      
      
      
      
      # div(
      #   class = "ui grid",
      #   # div(
      #   #   class = "one wide column"
      #   # ),
      #   div(
      #     class = "five wide column",
      #     div(
      #       class = "footer",
      #       #style = "",
      #       "Smart Surface Monitoring Platform"
      #     )
      #   ),
      #   div(
      #     class = "eight wide column"
      #   ),
      #   div(
      #     class = "three wide column",
      #     div(
      #       class = "footer",
      #       #style = "position: fixed; bottom: 45px; font-size: 17px",
      #       "©2021 Curiato, Inc. All Rights Reserved."
      #     )
      #   )
      # )
      
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'curiat_enduser_staging_profile'
    ),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(spinners = 7),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

