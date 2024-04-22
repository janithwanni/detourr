
#' Shiny bindings for detourr
#'
#' Output and render functions for using detourr with shiny. The output
#' function used must match both the display method and tour dim used,
#' or it will lead to strange behavour.
#'
#' @inheritParams htmlwidgets::shinyRenderWidget
#' @inheritParams htmlwidgets::shinyWidgetOutput
#'
#' @param output_id output variable to read from
#' @param expr an expression that generates a {detourr} widget
#'
#' @return An output or render function that enables the use of the widget
#' within shiny applications
#' 
#' @name detour-shiny
#' @export
displayScatter3dOutput <- function(output_id,
                                   width = "100%",
                                   height = "400px") {
  htmltools::attachDependencies(
    shiny::tagList(
      htmlwidgets::shinyWidgetOutput(output_id, "show_scatter_3d",
        width, height,
        package = "detourr"
      )
    ),
    crosstalk::crosstalkLibs()
  )
}

#' @rdname detour-shiny
#' @export
displayScatter2dOutput <- function(output_id,
                                   width = "100%",
                                   height = "400px") {
  htmltools::attachDependencies(
    shiny::tagList(
      htmlwidgets::shinyWidgetOutput(output_id, "show_scatter_2d",
        width, height,
        package = "detourr"
      )
    ),
    crosstalk::crosstalkLibs()
  )
}

#' @rdname detour-shiny
#' @export
shinyRenderDisplayScatter2d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, displayScatter2dOutput, quoted = TRUE, env = env)
}

#' @rdname detour-shiny
#' @export
shinyRenderDisplayScatter3d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, displayScatter3dOutput, quoted = TRUE, env = env)
}

#' Send commands to a detourr instance in a Shiny app
#'
#' Creates a proxy object that can be used to add
#' or remove points to a detour instance that has
#' already being rendered using \code{\link{shinyRenderDisplayScatter3d}}.
#' To be used in Shiny apps only.
#' #TODO: Check if namespaced modules can work as well
#' @param id output id of the detourr instance
#' @param session the Shiny session object used in the app.
#' Default should work for most cases
#'
#' @rdname detour-shiny
#' @export
display_scatter_proxy <- function(id, session = shiny::getDefaultReactiveDomain()) { #nolint
  structure(list(id = id, session = session), class = "detourr_proxy")
}

#' Function to add a bunch of points to existing shiny instance
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @param data dataframe of points
#' @param scale_attr result of `attributes(scale(<original_dataset>))`
#'  `1 / max(sqrt(rowSums(scale(<original_dataset>)^2)))`
#' @rdname detour-shiny
#' @export
add_points <- function(
  proxy,
  data,
  scale_attr = NULL,
  scale_factor = NULL,
  colour = "black",
  size =  1,
  alpha = 1
) {
  data <- unname(as.matrix(data)) |> 
    scale(
      center = scale_attr[["scaled:center"]],
      scale = FALSE
    )
  data <- data * scale_factor
  message <- list(
    id = proxy$id,
    data = apply(data, 1, as.list),
    config = list(
      colour = colour,
      size = size,
      alpha = alpha
    )
  )
  proxy$message <- message
  proxy$session$sendCustomMessage("add-points", proxy$message)
  return(proxy)
}

#' Function to add a bunch of lines to existing shiny instance
#'
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @param edge_list data.frame with columns `from` and `to`.
#' The indexing of points starts with the original dataset.
#' If \code{\link{add_points}} has been called before hand,
#' the indexing of these points starts from the end of the original dataset.
#' @rdname detour-shiny
#' @export
add_edges <- function(proxy, edge_list) {
  edge_list <- edge_list |> as.matrix() |> unname()
  edges <- apply(edge_list, 1, as.list)
  if (!is.null(proxy$message)) {
    proxy$message$edges <- edges
  }
  proxy$session$sendCustomMessage("add-edges", proxy$message)
  return(proxy)
}

#' Function to highlight a given set of points
#'
#' The given points will have the original opacity while the other points
#' will have reduced opacity
#'
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @param point_list Numeric vector. indexes to highlight in the prinary dataset
#' @param alpha The transparency value of the points outside of the point_list
#' @rdname detour-shiny
#' @export
highlight_points <- function(proxy, point_list, alpha = 0.3) {
  if (length(point_list) == 1) {
    point_list <- list(point_list)
  }
  proxy$message$point_list <- point_list
  proxy$session$sendCustomMessage("highlight-points", proxy$message)
  return(proxy)
}

#' Function to enlarge a given set of points
#'
#' The given points will have a larger size while the rest
#' remains the same
#'
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @param point_list Numeric vector. indexes to enlarge in the prinary dataset
#' @param shape the size of the points to be enlarged
#' @rdname detour-shiny
#' @export
enlarge_points <- function(proxy, point_list, size = 2) {
  if (length(point_list) == 1) {
    point_list <- list(point_list)
  }
  proxy$message$enlarge_point_list <- point_list
  proxy$message$size <- size
  proxy$session$sendCustomMessage("enlarge-points", proxy$message)
}


#' Function to clear added points
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @rdname detour-shiny
#' @export
clear_points <- function(proxy) {
  proxy$session$sendCustomMessage("clear_points")
}

#' Function to clear added points
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @rdname detour-shiny
#' @export
clear_edges <- function(proxy) {
  proxy$session$sendCustomMessage("clear_edges")
}

#' Function to clear added points
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @rdname detour-shiny
#' @export
clear_highlight <- function(proxy) {
  proxy$session$sendCustomMessage("clear_highlight")
}

#' Function to clear added points
#' @param proxy proxy object created by \code{\link{display_scatter_proxy}}
#' @rdname detour-shiny
#' @export
clear_enlarge <- function(proxy) {
  proxy$session$sendCustomMessage("clear_enlarge")
}