
valueBox_2 <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-5",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-5 text-right"),
                      div(style = ("font-size: 30px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
valueBox_3 <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-9",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-5",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-5 text-right"),
                      div(style = ("font-size: 15px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
valueBox_der <- function() {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#fcfdff"),
              div(class = "row"
              ),
              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/DER.png",height="90%",align = "center",width="90%",style="display: block; ")
              )
              
              
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
  
}

valueBox_oder <- function() {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#fcfdff"),
              div(class = "row"
              ),
              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/ODER-3.png",height="75%",align = "center",width="75%",style="display: block; align: center ")
              )
              
              
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
  
}

valueBox_ndi <- function() {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#fcfdff"),
              div(class = "row"
              ),
              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/NDI.png",height="80%",align = "center",width="90%",style="display: block; ")
              )),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
  
}

