#' Render flowchart
#'
#' @param graph reactive graph object
#' @return rendered flowchart
#' @export
renderFlowchart <- function(graph) {
  req(graph())
  tryCatch(
    {
      render_graph(graph(), as_svg = TRUE)
    },
    error = function(e) {
      message <- paste("An error occurred while creating the graph:", e$message, sep = "\n")
      shinyalert(
        title = "Error",
        text = message,
        type = "error"
      )
      return(NULL)
    }
  )
}

#' Initialize panzoom
#'
#' @param outputId Graph output ID, including any Shiny module namespace
#' @export
initialize_graph_zooming <- function(outputId = "flowchart") {
  selector <- jsonlite::toJSON(paste0("#", outputId), auto_unbox = TRUE)
  shinyjs::runjs(paste0('
  var graphSelector = ', selector, ';
  function initializePanzoom() {
  var element = document.querySelector(graphSelector + " svg");

  if (element && element.tagName.toLowerCase() === "svg") {
    // Check if panzoom is already initialized
    if (!element.__panzoomInitialized) {
      panzoom(element, {
        bounds: true,
        boundsPadding: 0.1
      });
      element.__panzoomInitialized = true; // Mark this SVG as initialized
    }
  }
}

function observeGraphContainer(targetNode) {
  var observer = new MutationObserver(function (mutationsList) {
    for (var mutation of mutationsList) {
      if (mutation.type === "childList") {
        initializePanzoom();
      }
    }
  });

  observer.observe(targetNode, {
    childList: true,
    subtree: true
  });
}

function setupGraphObserver() {
  var targetNode = document.querySelector(graphSelector);
  if (targetNode) {
    observeGraphContainer(targetNode);
    initializePanzoom();
    return;
  }

  var bodyObserver = new MutationObserver(function () {
    var targetNode = document.querySelector(graphSelector);
    if (!targetNode) return;
    bodyObserver.disconnect();
    observeGraphContainer(targetNode);
    initializePanzoom();
  });
  bodyObserver.observe(document.body, {
    childList: true,
    subtree: true
  });
}

setupGraphObserver();
'))
}
