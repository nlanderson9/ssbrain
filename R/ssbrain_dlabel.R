ss_dlabel = function(dlabel_filename,
                  labels=NULL,
                  colors=NULL) {

  if (missing(dlabel_filename)) {
    stop("ERROR in `ss_dlabel`: You must provide a dlabel filename.")
  }
  if (!file.exists(dlabel_filename)) {
    stop(paste0("ERROR in `ss_dlabel`: Your file ", dlabel_filename, " doesn't exist."))
  }
  if (!grepl("*\\.dlabel\\.nii*",dlabel_filename)) {
    cat(paste0("\nWarning for `dlabel`: your filename ", dlabel_filename, " doesn't end in `dlabel.nii` - check to make sure this is the right filetype.\n"))
  }

  if (!is.null(labels)) {
    if (!is.numeric(labels) & !is.character(labels)) {
      stop("ERROR in `ss_dlabel`: The `labels` argument must be a number (or list of numbers) or a label name (or list of label names).")
    }
  }


  for (color in colors) {
    if (is.character(color)) {
      if (!tryCatch(is.matrix(col2rgb(color)),error=function(e) FALSE)) {
        stop("ERROR in `ss_dlabel`: The argument `colors` must include valid color strings (R color-name or HEX value) or RBG color triples (e.g. `c(255,255,255)`).")
      }
    } else if (any(color < 0) | any(color > 255) | length(color) != 3) {
      stop("ERROR in `ss_dlabel`: The argument `colors` must include valid color strings (R color-name or HEX value) or RBG color triples (e.g. `c(255,255,255)`).")
    }
  }

  if (is.null(labels) & !is.null(colors)) {
    stop("ERROR in `ss_dlabel`: You cannot provide colors without providing labels.")
  }

  if (length(labels) != length(colors) & !is.null(colors)) {
    stop("ERROR in `ss_dlabel`: ERROR: The number of labels must match the number of colors")
  }

  output = list(dlabel_filename = dlabel_filename,
                labels = labels,
                colors = colors)
  class(output) = c("ssbrain", "dlabel")
  return(output)
}

is.dlabel = function(x) {
  inherits(x, "dlabel")
}

add_dlabel = function(obj1, obj2) {
  dlabel_filename = obj2$dlabel_filename
  labels = obj2$labels
  label_colors = obj2$colors

  l_verts = obj1$surf_info$left$num_verts
  r_verts = obj1$surf_info$right$num_verts
  total_verts = sum(l_verts, r_verts)

  for (c in 1:length(label_colors)) {
    color = label_colors[c]
    if (!is.character(color) & !is.null(color)) {
      label_colors[c] = rgb(color[1], color[2], color[3], 255, maxColorValue = 255)
    }
  }


  dlabel = importCifti(dlabel_filename)
  all_data = dlabel$data$normal

  dlabel_meta = read_xml(dlabel$data_meta[[1]][2])
  dlabel_meta1 = xml_children(dlabel_meta)
  dlabel_meta2 = xml_children(dlabel_meta1)
  dlabel_meta3 = xml_children(dlabel_meta2[2])
  dlabel_meta4 = xml_children(dlabel_meta3)
  dlabel_meta5 = xml_children(dlabel_meta4[2])
  dlabel_labelnames = xml_text(dlabel_meta5)
  dlabel_labelnames = dlabel_labelnames[-1]
  label_red = xml_attr(dlabel_meta5, "Red")
  label_green = xml_attr(dlabel_meta5, "Green")
  label_blue = xml_attr(dlabel_meta5, "Blue")
  label_red = as.numeric(label_red[-1])
  label_green = as.numeric(label_green[-1])
  label_blue = as.numeric(label_blue[-1])
  label_alpha = rep(1,length(label_red))
  rgb_matrix = cbind(label_red, label_green, label_blue, label_alpha)
  rgb_fun = function(x) {
    rgb(x[1], x[2], x[3], x[4])
  }
  dlabel_colors = apply(rgb_matrix, 1, rgb_fun)

  if (is.character(labels)) {
    if(! all(labels %in% dlabel_labelnames)) {
      stop("ERROR in `ss_dlabel`: One or more labels do not appear in the dlabel file.")
    }
  }

  if (is.character(labels)) {
    labels = match(labels, dlabel_labelnames)
  } else if (is.null(labels)) {
    labels = 1:(length(dlabel_labelnames))
  }

  colors = rep("#D3D3D3", length(all_data))

  for (i in 1:length(labels)) {
    indices = which(all_data == labels[i])
    if (is.null(label_colors)) {
      colors[indices] = dlabel_colors[labels[i]]
    } else {
      if (is.list(label_colors)) {
        this_color = label_colors[[i]]
      } else {
        this_color = label_colors
      }
      if (is.numeric(this_color)) {
        this_color = rgb(this_color[1], this_color[2], this_color[3], 255, maxColorValue = 255)
      } else if (is.character(this_color) & ! grepl("^#*", this_color)) {
        this_color = col2hex(this_color)
      }
      colors[indices] = this_color
    }
  }

  dlabel_colors_left = colors[1:l_verts]
  dlabel_colors_right = colors[(l_verts+1):total_verts]

  # Get existing colors; don't overwrite existing data with blank vertices
  current_colors_left = obj1$color_info$left
  current_colors_right = obj1$color_info$right

  set_colors_left = dlabel_colors_left
  set_colors_right = dlabel_colors_right
  set_colors_left[which(dlabel_colors_left == "#D3D3D3")] = current_colors_left[which(dlabel_colors_left == "#D3D3D3")]
  set_colors_right[which(dlabel_colors_right == "#D3D3D3")] = current_colors_right[which(dlabel_colors_right == "#D3D3D3")]

  obj1$dlabel_info = list(dlabel_info = list(left = all_data[1:l_verts],
                                              right = all_data[(l_verts+1):total_verts]),
                           colors = colors)
  obj1$color_info = list(left = set_colors_left, right = set_colors_right)

  return(obj1)
}
