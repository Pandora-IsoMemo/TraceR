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
#' @export
initialize_graph_zooming <- function() {
  shinyjs::runjs('
  function initializePanzoom() {
  var element = document.querySelector("#flowchart svg");

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

// Function to observe changes in the graph container
function setupGraphObserver() {
  var targetNode = document.querySelector("#flowchart");
  if (!targetNode) {
    console.error("Flowchart container not found.");
    return;
  }

  // Create a mutation observer
  var observer = new MutationObserver(function (mutationsList) {
    for (var mutation of mutationsList) {
      if (mutation.type === "childList") {
        // Re-run initializePanzoom when new elements are added
        initializePanzoom();
      }
    }
  });

  // Start observing the container for child changes
  observer.observe(targetNode, {
    childList: true,
    subtree: false // Only observe direct children
  });
}

// Initial call to setup observer and panzoom
setupGraphObserver();
initializePanzoom();
')
}
