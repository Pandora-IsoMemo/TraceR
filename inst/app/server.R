shinyServer(function(input, output, session) {
  graph <- reactiveVal()
  observeEvent(input$generate_flowchart, {
    graph(create_example_graph())
    output$flowchart <- renderGrViz({
      render_graph(graph(), layout = "nicely", as_svg = TRUE)
    })
    # Custom js to add panzoom after the svg was created
    shinyjs::runjs('
    setTimeout(function(){ // 500ms timeout as svg is not immediately available
     var element = document.querySelector("#flowchart svg");
     panzoom(element, {
      bounds: true,
      boundsPadding: 0.1
    });
}, 500);')
  })

  output$clickMessage <- renderText({
    req(input$flowchart_click)
    paste0("You clicked the node, which has the ID ", input$flowchart_click$id[[1]], ".")
  })
})
