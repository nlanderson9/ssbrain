ss_border = function(border_filename,
                  hemisphere,
                  borders=NULL,
                  border_width=5,
                  border_colors=NULL,
                  offset = TRUE) {

  if (missing(border_filename)) {
    stop("ERROR in `ss_border`: You must provide a border filename.")
  }
  if (!file.exists(border_filename)) {
    stop(paste0("ERROR in `ss_border`: Your file ", border_filename, " doesn't exist."))
  }
  if (!grepl("*\\.border$",border_filename)) {
    cat(paste0("\nWarning for `border`: your filename ", border_filename, " doesn't end in `.border` - check to make sure this is the right filetype.\n"))
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

  if (!is.numeric(border_width)) {
    stop("ERROR in `ss_border`: The argument `border_width` must be a number.")
  }
  if (! border_width >= 0) {
    stop("ERROR in `ss_border`: The argument `border_width` must be a positive number.")
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

  output = list(border_filename = border_filename,
                hemisphere = hemisphere,
                borders = borders,
                border_width = border_width,
                border_colors = border_colors,
                offset = offset)
  class(output) = c("ssbrain", "border")
  return(output)
}

is.border = function(x) {
  inherits(x, "border")
}

add_border = function(obj1, obj2) {
  border_filename = obj2$border_filename
  hemisphere = obj2$hemisphere
  borders = obj2$borders
  border_width = obj2$border_width
  border_colors = obj2$border_colors
  offset = obj2$offset

  if (offset & is.null(brain$surf_info$border_vertices[[hemisphere]])) {
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

  xml_border = read_xml(border_filename)
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
          vertices=as.matrix(cbind(c(0,0,0,1),c(0,0,0,1))), segments=as.matrix(c(1,2)), material = material3d(lwd = border_width, col=border_colors[j])
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
        border_vertices = apply(vertex_matrix, 2, function(x) as.matrix(c(rowMeans(brain$surf_info$border_vertices[[hemisphere]][,x]),1)))
      } else {
        border_vertices = apply(vertex_matrix, 2, function(x) as.matrix(c(rowMeans(brain$surf_info[[hemisphere]]$vertices[,x][1:3,]),1)))
      }


      border_indices = as.matrix(rbind(
        start_vertex:(start_vertex + ncol(vertex_matrix) - 1),
        c((start_vertex+1):(start_vertex + ncol(vertex_matrix) - 1),start_vertex)
      ))

      start_vertex = start_vertex + ncol(vertex_matrix)

      border_vertex_list[[i+1]] = border_vertices
      border_index_list[[i+1]] = border_indices

      border_mesh = rgl::mesh3d(
        vertices=border_vertices, segments=border_indices, material = material3d(color = border_colors[j], lwd = border_width)
      )
      border_parts[[i+1]] = border_mesh
    }

    all_border_vertices = do.call(cbind, border_vertex_list)
    all_border_indices = do.call(cbind, border_index_list)

    border_mesh = rgl::mesh3d(
      vertices = all_border_vertices, segments = all_border_indices, material = material3d(color = border_colors[j], lwd = border_width)
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
