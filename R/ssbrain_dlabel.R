#' @title Set a Dlabel
#'
#' @description This function sets a dlabel data file on the surface of the brain
#'
#' @param filename A string with the full filepath to a dlabel file
#' @param labels A single label, either an integer corresponding to the label number in the file (e.g. 5) or the name in the file (e.g. "LANG"). Can also be a vector of integers/names if multiple labels should be displayed. If omitted, all labels in the file will be plotted.
#' @param colors A list of colors the same length as \code{labels}; colors can be R color names (e.g. "red"), hex codes (e.g. "#FF0000"), or RGB triples (e.g. c(255,0,0)). If omitted, the default colors of the file will be used.
#' @param dlabel_data (Optional argument) Rather than passing a dlabel file, you can directly pass in an R vector containing the data. This overrides \code{filename}.
#'
#' @import grDevices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Here, all labels will be plotted and the file's colorscheme will be used
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dlabel(filename = "/path/to/my/datafile.dlabel.nii")
#'
#' # Here, only labels 1 and 5 from the file will be used, and they are colored "red" and "green"
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain_new = my_brain +
#'   ss_dlabel(filename = "/path/to/my/datafile.dlabel.nii",
#'              labels = c(1,5),
#'              colors = list("red", "green"))
#'
#' # Here, labels 8 through 10 will be plotted, with different
#' # color specifications used for each
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dlabel(filename = "/path/to/my/datafile.dlabel.nii",
#'   labels = 8:10,
#'   colors = list("#FF00FF", c(212,118,97), "palegreen"))
#' }
#'
#' @details
#' Note: Adding multiple \code{ss_dlabel} items to the same object will cause overlapping brain maps, with the most recently-added on top.

ss_dlabel = function(filename,
                  labels=NULL,
                  colors=NULL,
                  dlabel_data) {

  if (missing(filename)) {
    stop("ERROR in `ss_dlabel`: You must provide a dlabel filename.")
  }
  if (!file.exists(filename)) {
    stop(paste0("ERROR in `ss_dlabel`: Your file ", filename, " doesn't exist."))
  }
  if (!grepl("*\\.dlabel\\.nii*",filename)) {
    cat(paste0("\nWarning for `dlabel`: your filename ", filename, " doesn't end in `dlabel.nii` - check to make sure this is the right filetype.\n"))
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

  if (missing(dlabel_data)) {
    dlabel_data = NULL
  }

  output = list(filename = filename,
                labels = labels,
                colors = colors,
                dlabel_data = dlabel_data)
  class(output) = c("ssbrain", "dlabel")
  return(output)
}

#' @title Check if Dlabel
#'
#' @description This function checks if an object is of the class \code{ssdlabel}.
#'
#' @param x The object to check
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_dlabel = ss_dlabel(filename = "/path/to/my/datafile.dlabel.nii")
#' is.dlabel(new_dlabel)
#' }

is.dlabel = function(x) {
  inherits(x, "dlabel")
}

#' @title (Internal) Set a Dlabel
#'
#' @description The internal function that calculates the result for \code{\link[ssbrain]{ss_dlabel}}
#'
#' @param obj1 The existing \code{ssbrain} object
#' @param obj2 The new \code{ssdlabel} object to add
#'
#' @import grDevices
#' @import xml2
#' @import gplots

add_dlabel = function(obj1, obj2) {
  filename = obj2$filename
  labels = obj2$labels
  label_colors = obj2$colors
  dlabel_data = obj2$dlabel_data

  l_verts = obj1$surf_info$left$num_verts
  r_verts = obj1$surf_info$right$num_verts
  total_verts = sum(l_verts, r_verts)

  if (length(label_colors) == 1) {
    if (!is.character(label_colors)) {
      label_colors = rgb(label_colors[1], label_colors[2], label_colors[3], 255, maxColorValue = 255)
    } else if (is.character(label_colors) & ! grepl("^#.*", label_colors)) {
      label_colors = col2hex(label_colors)
    }
  } else if (is.null(label_colors)) {
    label_colors = NULL
  } else {
    for (c in 1:length(label_colors)) {
      color = label_colors[[c]]
      if (!is.character(color) & !is.null(color)) {
        label_colors[[c]] = rgb(color[1], color[2], color[3], 255, maxColorValue = 255)
      } else if (!is.null(color)) {
        if (is.character(color) & ! grepl("^#.*", color)) {
          label_colors[[c]] = col2hex(color)
        }
      }
    }
  }

  if (is.null(dlabel_data)) {
    dlabel = importCifti(filename)
    all_data = dlabel$data$normal
  } else {
    all_data = dlabel_data
  }

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
