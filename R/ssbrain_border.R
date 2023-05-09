#' @title Set a Border
#'
#' @description This function sets a border data file on the surface of the brain
#'
#' @param filename A string with the full filepath to a border file
#' @param hemisphere The hemisphere of the border file, either "left" or "right
#' @param borders A single border, either an integer corresponding to the border number in the file (e.g. 5) or the name in the file (e.g. "LANG"). Can also be a vector of integers/names if multiple borders should be displayed. If omitted, all borders in the file will be plotted.
#' @param width The width of the borders to be plotted. Defaults to 5.
#' @param colors A list of colors the same length as \code{borders}; colors can be R color names (e.g. "red"), hex codes (e.g. "#FF0000"), or RGB triples (e.g. c(255,0,0)). If omitted, the default colors of the file will be used.
#' @param offset Whether the border should be slightly raised up (offset) above the brain (TRUE) or not (FALSE). Defaults to TRUE.
#'
#' @import grDevices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Here, all borders will be plotted and the file's colorscheme will be used
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_border(filename = "/path/to/my/datafile_lh.border", hemisphere = "left")
#'
#' # Here, only borders 1 and 5 from the file will be used, and they are colored "red" and "green"
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain_new = my_brain +
#'   ss_border(filename = "/path/to/my/datafile_lh.border",
#'             hemisphere = "left",
#'             borders = c(1,5),
#'             border_colors = list("red", "green"))
#'
#' # Here, labels 8 through 10 will be plotted, with different
#' # color specifications used for each
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_border(filename = "/path/to/my/datafile_lh.border",
#'             hemisphere = "left",
#'             borders = 8:10,
#'             border_colors = list("#FF00FF", c(212,118,97), "palegreen"))
#'
#' # Here, three borders are plotted by name, and all are black
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_border(filename = "/path/to/my/datafile_lh.border",
#'             hemisphere = "left",
#'             borders = c("LANG", "VIS", "AUD"),
#'             border_colors = "black")
#' }
#'
#' @details
#' Note: Adding multiple \code{ss_border} items to the same object will cause overlapping brain maps, with the most recently-added on top.


ss_border = function(filename,
                  hemisphere,
                  borders=NULL,
                  width=5,
                  border_colors=NULL,
                  offset = TRUE) {

  if (missing(filename)) {
    stop("ERROR in `ss_border`: You must provide a border filename.")
  }
  if (!file.exists(filename)) {
    stop(paste0("ERROR in `ss_border`: Your file ", filename, " doesn't exist."))
  }
  if (!grepl("*\\.border$",filename)) {
    cat(paste0("\nWarning for `border`: your filename ", filename, " doesn't end in `.border` - check to make sure this is the right filetype.\n"))
  }

  if (missing(hemisphere)) {
    stop("ERROR in `ss_border`: You must provide a hemisphere ('left' or 'right').")
  }
  if (! hemisphere %in% c("left", "right")) {
    stop("ERROR in `ss_border`: The argument `hemisphere` must be 'left' or 'right'.")
  }

  if (!is.null(borders)) {
    if (!is.numeric(borders) & !is.character(borders)) {
      stop("ERROR in `ss_border`: The `borders` argument must be a number (or list of numbers) or a border name (or list of border names).")
    }
  }

  if (!is.numeric(width)) {
    stop("ERROR in `ss_border`: The argument `width` must be a number.")
  }
  if (! width >= 0) {
    stop("ERROR in `ss_border`: The argument `width` must be a positive number.")
  }

  for (border_color in border_colors) {
    if (is.character(border_color)) {
      if (!tryCatch(is.matrix(col2rgb(border_color)),error=function(e) FALSE)) {
        stop("ERROR in `ss_border`: The argument `border_color` must be a valid color string (R color-name or HEX value) or RBG color triple (e.g. `c(255,255,255)`).")
      }
    } else if (any(border_color < 0) | any(border_color > 255) | length(border_color) != 3) {
      stop("ERROR in `ss_border`: The argument `border_color` must be a valid color string (R color-name or HEX value) or RBG color triple (e.g. `c(255,255,255)`).")
    }
  }

  if (!is.logical(offset)) {
    stop("ERROR in `ss_border`: The `offset` argument must be TRUE or FALSE.")
  }

  output = list(filename = filename,
                hemisphere = hemisphere,
                borders = borders,
                width = width,
                border_colors = border_colors,
                offset = offset)
  class(output) = c("ssbrain", "border")
  return(output)
}

#' @title Check if Border
#'
#' @description This function checks if an object is of the class \code{ssborder}.
#'
#' @param x The object to check
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_border = ss_border(filename = "/path/to/my/datafile.dlabel.nii", hemisphere = "left")
#' is.border(new_border)
#' }

is.border = function(x) {
  inherits(x, "border")
}

#' @title (Internal) Set a Border
#'
#' @description The internal function that calculates the result for \code{\link[ssbrain]{ss_border}}
#'
#' @param obj1 The existing \code{ssbrain} object
#' @param obj2 The new \code{ssborder} object to add
#'
#' @import grDevices
#' @import xml2

add_border = function(obj1, obj2) {
  filename = obj2$filename
  hemisphere = obj2$hemisphere
  borders = obj2$borders
  width = obj2$width
  border_colors = obj2$border_colors
  offset = obj2$offset

  if (offset & is.null(obj1$surf_info$border_vertices[[hemisphere]])) {
    cat("\nWarning for `border`: Currently, border offset is only supported with standard fsaverage6/7 meshes. Borders will be set to non-offset.\n")
    offset = FALSE
  }

  for (c in 1:length(border_colors)) {
    border_color = border_colors[c]
    if (!is.character(border_color) & !is.null(border_color)) {
      border_colors[c] = rgb(border_color[1], border_color[2], border_color[3], 255, maxColorValue = 255)
    }
  }

  if (length(borders) > 1 & length(border_colors) == 1 & !is.null(border_colors)) {
    border_colors = rep(border_colors, length(borders))
  }

  verts = obj1$surf_info[[hemisphere]]$num_verts

  xml_border = read_xml(filename)
  border_info = xml_children(xml_border)
  border_data = xml_children(border_info[2]) # here, 2 is the data
  if (is.numeric(borders)) {
    if(any(borders > length(border_data))) {
      stop("ERROR in `ss_border`: You have provided border numbers in the argument `borders` that exceed the number of borders in the provided file.")
    }
  } else {
    border_names = unlist(lapply(border_data, xml_attr, attr="Name"))
    if(! all(borders %in% border_names)) {
      stop("ERROR in `ss_border`: You have one or more border names in the argument `borders` that are not in the provided file.")
    }
  }

  if (is.null(borders)) {
    borders = 1:length(border_data)
  }

  if (is.null(border_colors)) {
    border_colors = c()
    for (i in 1:length(border_data)) {
      red = as.numeric(xml_attr(border_data[i], attr="Red"))
      green = as.numeric(xml_attr(border_data[i], attr="Green"))
      blue = as.numeric(xml_attr(border_data[i], attr="Blue"))
      border_colors[i] = rgb(red, green, blue, 1)
    }

  }

  border_meshes = list()
  for (j in 1:length(borders)) {
    if (is.numeric(borders)) {
      border = xml_children(border_data[borders[j]])
    } else {
      choice_index = which(border_names == borders[j])
      border = xml_children(border_data[choice_index])
    }

    border_parts = list()

    border_vertex_list = list()
    border_index_list = list()
    start_vertex = 1

    for (i in 0:length(border)) {
      # RGL draws the first border piece weirdly; so "draw" a fake non-existent border first
      if (i==0) {
        border_mesh = rgl::mesh3d(
          vertices=as.matrix(cbind(c(0,0,0,1),c(0,0,0,1))), segments=as.matrix(c(1,2)), material = material3d(lwd = width, col=border_colors[j])
        )
        border_parts[[i+1]] = border_mesh
        next
      }
      border_part = xml_children(border[i])
      vertices = unlist(strsplit(xml_text(border_part[1]), split="\n")) # here, 1 is the vertices
      split_vertices = lapply(vertices, function(x) as.numeric(strsplit(x, split=" ")[[1]]))
      vertex_matrix = t(do.call(rbind, split_vertices))
      vertex_matrix = vertex_matrix + 1 # border files use vertices, not row numbers

      if (offset) {
        border_vertices = apply(vertex_matrix, 2, function(x) as.matrix(c(rowMeans(obj1$surf_info$border_vertices[[hemisphere]][,x]),1)))
      } else {
        border_vertices = apply(vertex_matrix, 2, function(x) as.matrix(c(rowMeans(obj1$surf_info[[hemisphere]]$vertices[,x][1:3,]),1)))
      }


      border_indices = as.matrix(rbind(
        start_vertex:(start_vertex + ncol(vertex_matrix) - 1),
        c((start_vertex+1):(start_vertex + ncol(vertex_matrix) - 1),start_vertex)
      ))

      start_vertex = start_vertex + ncol(vertex_matrix)

      border_vertex_list[[i+1]] = border_vertices
      border_index_list[[i+1]] = border_indices

      border_mesh = rgl::mesh3d(
        vertices=border_vertices, segments=border_indices, material = material3d(color = border_colors[j], lwd = width)
      )
      border_parts[[i+1]] = border_mesh
    }

    all_border_vertices = do.call(cbind, border_vertex_list)
    all_border_indices = do.call(cbind, border_index_list)

    border_mesh = rgl::mesh3d(
      vertices = all_border_vertices, segments = all_border_indices, material = material3d(color = border_colors[j], lwd = width)
    )

    border_meshes[[j]] = border_mesh
  }

  if (is.null(obj1$borders_info[[hemisphere]]$mesh_list)) {
    obj1$borders_info[[hemisphere]]$mesh_list = border_meshes
  } else {
    obj1$borders_info[[hemisphere]]$mesh_list = append(obj1$borders_info[[hemisphere]]$mesh_list, border_meshes)
  }
  return(obj1)
}
