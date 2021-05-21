
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-3x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 25px; font-weight: bold;"),
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
                      div(style = ("font-size: 18px; font-weight: bold;"),
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

              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/DER.png",height="85%",align = "center",width="85%",style="display: block; text-align: center;")
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

              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/ODER-3.png",height="60%",align = "center",width="60%",style="display: block;  text-align: center;")
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
              div( tags$img(src="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/NDI.png",height="70%",align = "center",width="80%",style="display: block;  text-align: center; ")
              ), br(),
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          ),
      )
  )
  
}
valueBox_about <- function(imagen_vb, icon1, icon2, web1, web2, text, width_, height_) {
  div(class = "col-lg-3 col-md-10",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#ffffff"),
              div(class = "row",
                  div(class = "col-xs-10", style = "align : center;",
                      tags$a(tags$img(src=imagen_vb, height=width_,align = "center",width=height_,style="display: block; "),  href=web1)
                  )
                
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix",
                  div(class = "row",
                      fluidRow(div(class = "col-xs-6",
                          column(4,tags$a(icon(icon1, "fa-2x"),href=web1))
                      ),
                      div(class = "col-xs-4",
                          column(1,tags$a(icon(icon2, "fa-2x"),href=web2)))
                  ))
                      ))
          )
      )
  
}

valueBox_about_oder <- function(imagen_vb, icon1, icon2, web1, web2, text, width_, height_) {
  div(class = "col-lg-3 col-md-10",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#ffffff"),
              div(class = "row",
                  div(class = "col-xs-10", style = "align: center;",
                      column(4),
                      tags$a(tags$img(src=imagen_vb, height=width_,align = "center",width=height_,
                                      style="display: block; "),  href=web1)
                  )
                  
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix",
                  div(class = "row",
                      fluidRow(div(class = "col-xs-6",
                                   column(4,tags$a(icon(icon1, "fa-2x"),href=web1))
                      ),
                      div(class = "col-xs-4",
                          column(1,tags$a(icon(icon2, "fa-2x"),href=web2)))
                      ))
              ))
      )
  )
  
}

valueBox_people1 <- function(imagen_vb, icon1, icon2, web1, web2, text, width_, height_,text2) {
  div(class = "col-lg-3 col-md-11",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#ffffff"),
              div(class = "row",
                  fluidRow(    
                      column(4),
                      div(class = "col-xs-6", style = "align: center;",
                          
                      tags$a(tags$img(src=imagen_vb, height=width_,align = "center",width=height_,
                                      style="display: block; "),  href=web1)
                  ), 
                  div(class = ("col-xs-9 text-right"),
                      div(style = "font-size: 25px; color: #15202b;font-weight: bold; text-align:center; ",
                          column(5),h4(text, style = "text-align:center;font-weight: bold;"), 
                          column(5),h6(text2, style= "color: #7a7a7a; text-align:center")
                      )
                  
              ))
          )),
          div(class = "panel-footer",
              div(class = "clearfix",
                  div(class = "row",
                      fluidRow(div(class = "col-xs-6",
                                   column(4,tags$a(icon(icon1, "fa-2x"),href=web1))
                      ),
                      div(class = "col-xs-4",
                          column(1,tags$a(icon(icon2, "fa-2x"),href=web2)))
                      ))
              ))
      )
  )
  
}

valueBox_people2 <- function(imagen_vb, icon1, icon2, web1, web2, text, width_, height_,text2) {
  div(class = "col-lg-3 col-md-11",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:#ffffff"),
              div(class = "row",
                  fluidRow(    
                    column(4),
                    div(class = "col-xs-6", style = "align: center;",
                        
                        tags$a(tags$img(src=imagen_vb, height=width_,align = "center",width=height_,
                                        style="display: block; "),  href=web1)
                    ), 
                    div(class = ("col-xs-9 text-right"),
                        div(style = "font-size: 25px; color: #15202b;font-weight: bold; text-align:center; ",
                            column(2),h4(text, style = "text-align:center; font-weight: bold;"), 
                            column(2),h6(text2, style= "color: #7a7a7a; text-align:center")
                        )
                        
                    ))
              )),
          div(class = "panel-footer",
              div(class = "clearfix",
                  div(class = "row",
                      fluidRow(div(class = "col-xs-6",
                                   column(4,tags$a(icon(icon1, "fa-2x"),href=web1))
                      ),
                      div(class = "col-xs-4",
                          column(1,tags$a(icon(icon2, "fa-2x"),href=web2)))
                      ))
              ))
      )
  )
  
}
