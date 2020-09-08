#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(straw)
library(shinyAce)
library(readr)

# TODO: on click cyto set ace cursor position
#       highlight node on parse graph

find_code <- function (enhanced_ast, line1, col1, line2, col2) {
  code <- enhanced_ast[-1,]
  indx <- (
    ( (code$line1 < line1) | (code$line1 == line1 & code$col1 <= col1) ) &
    ( (code$line2 > line2) | (code$line2 == line2 & code$col2 >= col2) )
    )
  rects <- code[indx,]

  if (nrow(rects) == 1)
    return(rects)

  # find smallest bounding rectangle
  min_indx <- with(
    rects,
    which.min((abs(line1 - line2) + 1) * (abs(col1 - col2) + 1))
    )

  rects[min_indx,]
}





default_code = "if (1) 2 else {3 + 4}"
# Define UI for application that draws a histogram
ui <- fluidPage(
   tags$head(
     tags$script(read_file("ace_mark_handler.js"))
   ),
   # Application title
   titlePanel("Old Faithful Geyser Data"),

   # Sidebar with a slider input for number of bins
   # sidebarLayout(
   #    sidebarPanel(
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30)
   #    ),

      # Show a plot of the generated distribution
      mainPanel(
        aceEditor(
          "ace_code",
          value = default_code,
          selectionId = "ace_selection",
          cursorId = "ace_cursor",
          mode = "r",
          height = '200px'
          ),
        fluidRow(
          column(
            cytoscape_dagreOutput("parseGraph"),
            width = 6
          ),
          column(
            cytoscape_dagreOutput("astGraph"),
            width = 6
          )
        )
      )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  crnt_ast <- reactive({
    req(input$ace_code)
    enhance_ast(input$ace_code)
  })

  selected_row <- reactive({
    cursor <- req(input$ace_cursor)
    row <- cursor$row + 1
    col <- cursor$column + 1
    find_code(crnt_ast(), row, col, row, col)
  })

  output$parseGraph <- renderCytoscape_dagre({
    req(input$ace_code)
    pd <- getParseData(parse(text = input$ace_code))
    cytoscape_dagre(tbl_to_cyto(pd))
  })

  output$astGraph <- renderCytoscape_dagre({
    #ast <- create_parse_graph(exp, type = "ast")
    ast <- crnt_ast()
    ast$highlight <- FALSE
    sel <- selected_row()
    ast[sel$row_num, 'highlight'] <- TRUE
    session$sendCustomMessage(
      "ace-mark", c(sel$line1 - 1, sel$col1 - 1, sel$line2 - 1, sel$col2)
      )
    print(selected_row())
    cytoscape_dagre(tbl_to_cyto(ast))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

