fluidRow(
  column(2,
         
         h4('Choose Variables'), 
         selectInput('calc_vars', 
                     label = "",
                     list(), 
                     multiple=TRUE, 
                     selectize=TRUE),
         h4('Choose Data Transformation'),
         selectizeInput('data_agg', 
                        label = "",
                        list("sum",
                             "average",
                             "weighted average"),
                        options = list(maxItems = 1,
                                       placeholder = "select data transformation",
                                       onInitialize = I('function() { this.setValue(0); }')) 
         ),
         h4('Choose Probability Mapping'),
         selectizeInput('prob_map',
                        label = "",
                        list("expit", 
                             "tanh", 
                             "arctan"),
                        options = list(maxItems = 1,
                                       placeholder = "select probability generation",
                                       onInitialize = I('function() { this.setValue(0); }')) 
         ),
         actionButton("apply_aggs",label = "Apply Sequence")
  ), # end of column 1
  column(1, 
         offset = 0,
         style='padding:20px;',
         conditionalPanel(condition = "input.data_agg == 'weighted average'",
                          uiOutput("weighted_avg")
         )
  ),
  column(5,
         fluidRow(box(title="Probability generation ",
                      tags$table(border = 2, 
                                 tags$thead(
                                   tags$tr(
                                     tags$th(colspan = 4, height = 50, width = 600, 
                                             "Data Transformation Steps", style = 'text-align: center')
                                   )
                                 ), 
                                 tags$tbody(
                                   tags$tr(id = "agg-table-body",
                                           tags$td(align = "center", strong("Variables")),
                                           tags$td(align = "center", strong("1st Method")),
                                           tags$td(align = "center", strong("2nd Method")),
                                           tags$td(align = "center", strong("Variable Name"))
                                   )
                                 )
                      )
                      ,
                      set_html_breaks(2),
                      actionButton("get_prob",label = "Assign Treatment") 
                      
         )
         )
  ),
  column(3,
         tags$div(id = "prb-plt", 
                  class = "asn-plt",
                  plotOutput("prob_plot", width = "auto"),
                  tags$script(HTML("$('#apply_aggs').click(function(){
                                            $('#prb-plt').css('visibility', 'visible')  ;
                                        }) "
                  ))
         ),
         tags$div(id = "treat-plt", 
                  class = "asn-plt",
                  plotOutput("assignment_plot", width = "auto"),
                  tags$script(HTML("$('#get_prob').click(function(){
                                        $('#treat-plt').css('visibility', 'visible')  ;
                                    })"
                  ))
         ),
         actionButton("reset",label = "Reset"),
         tags$script(HTML("$('#reset').click(function(){
                                $('table tbody').find('.agg-seq').remove() ;
                                
                             }) ;
                            
                            $('#reset').click(function(){
                            $('.asn-plt').css('visibility', 'hidden') ;
                            });
                            "
         ))
  ),
  column(1)
)
