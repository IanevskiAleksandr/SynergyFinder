# Customised buttons Only using CSS3 code (No javascript)

actionButton2 <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link"))
    btn.css.class <- paste("btn",btn.style,sep="-")
  else btn.css.class = ""
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

colclass <- "GreyBlue"
colclass <- paste(colclass,"TrueFalse")

# adapted from Sébastien Rochette
switchButton <- function(inputId, label) {

    tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = colclass,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId
                                 ),
                                 tags$label(class = "onoffswitch-label", 'for' = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    ) 
  }
  

SwitchButtonsmallright <- function(inputId, label) {
 
  tagList(
    tags$div(class = "form-group shiny-input-container",
             tags$div(class = colclass,
                      tags$label(label, class = "labelpos"),
                      tags$div(class = "onoffswitch2",
                               tags$input(type = "checkbox", name = "onoffswitch2", class = "onoffswitch-checkbox",
                                          id = inputId, checked="checked"
                               ),
                               tags$label(class = "onoffswitch-label", 'for' = inputId,
                                          tags$span(class = "onoffswitch-inner"),
                                          tags$span(class = "onoffswitch-switch")
                               )
                      )
             )
    )
  ) 
}



