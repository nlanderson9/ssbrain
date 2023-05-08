#' @title Set a Seed
#'
#' @description This function sets a seed (based on a vertex row number) and then creates a map from a corresponding dconn file containing a functional connectivity matrix.
#'
#' @param dconn_filename A string with the full filepath to a dconn file
#' @param seed_value The number of the seed (row number, not vertex number)
#' @param colorbar A string containing the name of a colorbar to use (same names as Connectome Workbench)
#' @param thresh The option above which to hide data. Defaults to 0.2.
#' @param colorrange The range in which to spread the \code{colorbar}'s colors. Defaults to c(0.2, 0.6)
#' @param show_seed_sphere Whether or not to draw a sphere where the seed is (TRUE) or not (FALSE). Defaults to TRUE.
#' @param seed_sphere_size If show_seed_sphere is TRUE, the radius of the sphere. Defaults to 2.
#' @param seed_sphere_color If show_seed_sphere is TRUE, the color of the sphere. Defaults to "white".
#' @param seed_data (Optional argument) Rather than passing a dconn file, you can directly pass in an R matrix containing the functional connectivity data. This can save time if you want to draw multiple seeds from the same dconn matrix. This overrides \code{dconne_filename}.
#' @param palette (Experimental) A color palette created by \code{colorRampPalette}; overrides \code{colorbar}.
#'
#' @import grDevices
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_seed(dconn_filename = "/path/to/my/datafile.dconn.nii", seed_value = 31272)
#'
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain_new = my_brain +
#'   ss_dscalar(dconn_filename = "/path/to/my/datafile.dconn.nii",
#'              seed_value = 2213,
#'              colorbar = "TURBO",
#'              thresh = 0.3,
#'              colorrange = c(0.3, 0.8),
#'              seed_sphere_size = 4,
#'              seed_sphere_color = "red")
#'
#' # Warning: this may take a while (especially for fsaverage7),
#' # and will likely require a lot of memory!
#' dconn_data = importCifti(cifti_name = "/path/to/my/datafile.dconn.nii", data_only = TRUE)
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_seed(seed_data = dconn_data, seed_value = 31272)
#' }
#'
#' @details
#' Note: Adding multiple \code{ss_seed} items to the same object will cause overlapping brain maps, with the most recently-added on top.

ss_seed = function(dconn_filename,
                seed_value,
                colorbar="JET256",
                thresh = .2,
                colorrange = c(.2,.6),
                show_seed_sphere=TRUE,
                seed_sphere_size=2,
                seed_sphere_color="white",
                seed_data,
                palette) {

  if (missing(dconn_filename) & missing(seed_data)) {
    stop("ERROR in `ss_seed`: You must provide a dconn filename (`dconn_filename`) or dconn-style data (`seed_data`).")
  }
  if (!missing(dconn_filename) & !missing(seed_data)) {
    stop("Warning for `seed`: You've provided both a dconn filename and seed data; the seed data will override the dconn file.")
  }

  if (!missing(dconn_filename)) {
    if (!file.exists(dconn_filename)) {
      stop(paste0("ERROR in `ss_seed`: Your file ", dconn_filename, " doesn't exist."))
    }
    if (!grepl("*\\.dconn\\.nii*",dconn_filename)) {
      cat(paste0("\nWarning for `seed`: your filename ", dconn_filename, " doesn't end in `.dconn.nii` - check to make sure this is the right filetype.\n"))
    }
  }

  if (!is.numeric(seed_value)) {
    stop("ERROR in `ss_seed`: The `seed_value` must be a number.")
  }

  if (! colorbar %in% c("ROY-BIG-BL",
                        "videen_style",
                        "Gray_Interp_Positive",
                        "PSYCH-FIXED",
                        "RBGYR20",
                        "RBGYR20P",
                        "RYGBR4_positive",
                        "RGRBR_mirror90_pos",
                        "Orange-Yellow",
                        "POS_NEG_ZERO",
                        "red-yellow",
                        "blue-lightblue",
                        "FSL",
                        "power_surf",
                        "black-red",
                        "black-green",
                        "black-blue",
                        "black-red-positive",
                        "black-green-positive",
                        "black-blue-positive",
                        "blue-black-green",
                        "blue-black-red",
                        "red-black-green",
                        "fsl_red",
                        "fsl_green",
                        "fsl_blue",
                        "fsl_yellow",
                        "RedWhiteBlue",
                        "cool-warm",
                        "spectral",
                        "RY-BC-BL",
                        "magma",
                        "JET256",
                        "TURBO",
                        "TURBO",
                        "VIRIDIS",
                        "INFERNO",
                        "MAGMA",
                        "PLASMA",
                        "ROCKET",
                        "MAKO",
                        "CIVIDIS")) {
    stop("ERROR in `dlabel`: Available colorbars are:\nROY-BIG-BL
videen_style
Gray_Interp_Positive
PSYCH-FIXED
RBGYR20
RBGYR20P
RYGBR4_positive
RGRBR_mirror90_pos
Orange-Yellow
POS_NEG_ZERO
red-yellow
blue-lightblue
FSL
power_surf
black-red
black-green
black-blue
black-red-positive
black-green-positive
black-blue-positive
blue-black-green
blue-black-red
red-black-green
fsl_red
fsl_green
fsl_blue
fsl_yellow
RedWhiteBlue
cool-warm
spectral
RY-BC-BL
magma   [workbench version]
JET256
TURBO
VIRIDIS
INFERNO
MAGMA   [viridis version]
PLASMA
ROCKET
MAKO
CIVIDIS")
  }

  if (!is.numeric(thresh)) {
    stop("ERROR in `ss_seed`: The `thresh` argument must be a number.")
  } else if (! thresh >= 0) {
    stop("ERROR in `ss_seed`: The `thresh` argument must be a number >= 0.")
  }

  if (length(colorrange) != 2) {
    stop("ERROR in `ss_seed`: The `colorrange` argument must be a two-number vector (e.g. `c(1, 3)`)")
  }
  if (!is.numeric(colorrange)) {
    stop("ERROR in `ss_seed`: The `colorrange` argument must be a two-number vector (e.g. `c(1, 3)`)")
  }
  if (!all(colorrange >=0)) {
    stop("ERROR in `ss_seed`: The `colorrange` argument must be two numbers >= 0 (e.g. `c(1, 3)`)")
  }

  if (!is.logical(show_seed_sphere)) {
    stop("ERROR in `ss_seed`: The `show_seed_sphere` argument must be TRUE or FALSE.")
  }

  if (!is.numeric(seed_sphere_size)) {
    stop("ERROR in `ss_seed`: The `seed_sphere_size` must be a number.")
  }

  if (is.character(seed_sphere_color)) {
    if (!tryCatch(is.matrix(col2rgb(seed_sphere_color)),error=function(e) FALSE)) {
      stop("ERROR in `ss_seed`: The argument `seed_sphere_color` must be a valid color string (R color-name or HEX value) or RBG color triple (e.g. `c(255,255,255)`).")
    }
  } else if (any(seed_sphere_color < 0) | any(seed_sphere_color > 255) | length(seed_sphere_color) != 3) {
    stop("ERROR in `ss_seed`: The argument `seed_sphere_color` must be a valid color string (R color-name or HEX value) or RBG color triple (e.g. `c(255,255,255)`).")
  }

  if (!missing(seed_data)) {
    if (!is.matrix(seed_data)) {
      stop("ERROR in `ss_seed`: The `seed_data` argument must be a matrix.")
    }
    if (ncol(seed_data) != nrow(seed_data)) {
      stop(paste0("ERROR in `ss_seed`: The `seed_data` argument must be a symmetrical matrix; your number of rows (",nrow(seed_data),") is not equal to the number of columns (",ncol(seed_data),")."))
    }
    if (any(seed_data)>1 | any(seed_data) < 0) {
      stop("ERROR in `ss_seed`: The `seed_data` argument currently only supports values between 0 and 1; your matrix contains values outside that range.")
    }
  }

  if (missing(dconn_filename)) {
    dconn_filename = NULL
  }
  if (missing(seed_data)) {
    seed_data = NULL
  }

  if (!missing(palette)) {
    if (!is.function(palette)) {
      stop("ERROR in `seed: The `palette` argument must be a palette function created by colorRampPalette().")
    }
  } else {
    palette = NULL
  }

  output = list(dconn_filename = dconn_filename,
                seed_value = seed_value,
                colorbar = colorbar,
                thresh = thresh,
                colorrange = colorrange,
                show_seed_sphere = show_seed_sphere,
                seed_sphere_size = seed_sphere_size,
                seed_sphere_color = seed_sphere_color,
                seed_data = seed_data,
                palette = palette)
  class(output) = c("ssbrain", "dconn")
  return(output)
}

#' @title Check if Dconn
#'
#' @description This function checks if an object is of the class \code{ssdconn}.
#'
#' @param x The object to check
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_dconn = ss_seed(dconn_filename = "/path/to/my/datafile.dconn.nii", seed_value = 2213)
#' is.dconn(new_dconn)
#' }

is.dconn = function(x) {
  inherits(x, "dconn")
}

#' @title (Internal) Set a Seed
#'
#' @description The internal function that calculates the result for \code{\link[ssbrain]{ss_seed}}
#'
#' @param obj1 The existing \code{ssbrain} object
#' @param obj2 The new \code{ssdconn} object to add

add_dconn = function(obj1, obj2) {
  dconn_filename = obj2$dconn_filename
  seed_value = obj2$seed_value
  colorbar = obj2$colorbar
  thresh = obj2$thresh
  colorrange = obj2$colorrange
  show_seed_sphere = obj2$show_seed_sphere
  seed_sphere_size = obj2$seed_sphere_size
  seed_sphere_color = obj2$seed_sphere_color
  seed_data = obj2$seed_data
  palette = obj2$palette

  l_verts = obj1$surf_info$left$num_verts
  r_verts = obj1$surf_info$right$num_verts
  total_verts = sum(l_verts, r_verts)

  if (!is.character(seed_sphere_color)) {
    seed_sphere_color = rgb(seed_sphere_color[1], seed_sphere_color[2], seed_sphere_color[3], 255, maxColorValue = 255)
  }

  if (!is.null(palette)) {
    colorbar = NULL
  }

  if (!is.null(colorbar)) {
    color_data = colorbarGenerator(colorbar_name = colorbar)
  } else {
    color_data = colorbarGenerator(pos_colorpalette = palette)
  }


  low = color_data$pos_low
  high = color_data$pos_high
  palette = color_data$pos_palette

  if (is.null(seed_data)) {
    dconn_data = distill_dconn(dconn_filename, seed_value, total_verts)
  }

  if (is.null(seed_data)) {
    all_data = dconn_data$data$normal
  } else {
    all_data = seed_data
  }
  pos_indices = which(all_data>0)

  bins_labeled = cut(all_data[pos_indices], 100)

  pos_bins = as.numeric(bins_labeled)

  binlist = rep(1, length(all_data))
  binlist[pos_indices] = pos_bins + 1

  low_bin = which(unlist(lapply(levels(bins_labeled), checkBin, y=colorrange[1])))
  high_bin = which(unlist(lapply(levels(bins_labeled), checkBin, y=colorrange[2])))

  num_colors = high_bin - low_bin + 1
  num_lowcolors = low_bin - 1
  num_highcolors = 100 - high_bin


  palette_colors = palette(num_colors)

  allcolors = c("#D3D3D3", rep(low, num_lowcolors), palette_colors, rep(high, num_highcolors))

  colors = allcolors[binlist]

  below_thresh = which(all_data < thresh)
  colors[below_thresh] = "#D3D3D3"

  dconn_colors_left = colors[1:l_verts]
  dconn_colors_right = colors[(l_verts+1):total_verts]

  # Get existing colors; don't overwrite existing data with blank vertices
  current_colors_left = obj1$color_info$left
  current_colors_right = obj1$color_info$right

  set_colors_left = dconn_colors_left
  set_colors_right = dconn_colors_right
  set_colors_left[which(dconn_colors_left == "#D3D3D3")] = current_colors_left[which(dconn_colors_left == "#D3D3D3")]
  set_colors_right[which(dconn_colors_right == "#D3D3D3")] = current_colors_right[which(dconn_colors_right == "#D3D3D3")]

  if (show_seed_sphere) {
    if (seed_value <= l_verts) {
      sphere_coords = obj1$surf_info$left$vertices[,seed_value]
      seed_hemi = "left"
    } else {
      sphere_coords = obj1$surf_info$right$vertices[,(seed_value-l_verts)]
      seed_hemi = "right"
    }
    seed_sphere_info = list(coords = sphere_coords, size = seed_sphere_size, color = seed_sphere_color)

    if (is.null(obj1$sphere_info[[seed_hemi]]$sphere_list)) {
      obj1$sphere_info[[seed_hemi]]$sphere_list = list(seed_sphere_info)
    } else {
      current_num_spheres = length(obj1$sphere_info[[seed_hemi]]$sphere_list)
      obj1$sphere_info[[seed_hemi]]$sphere_list[[current_num_spheres+1]] = seed_sphere_info
    }
  }  else {
    seed_sphere_info = NULL
  }

  obj1$dconn_info = list(dconn_data = list(left = as.matrix(all_data[1:l_verts]),
                                            right = as.matrix(all_data[(l_verts+1):total_verts])),
                          colorbar = colorbar,
                          thresh = thresh,
                          colorrange = colorrange,
                          colors = colors,
                          seed_value = seed_value,
                          seed_sphere_info = seed_sphere_info)

  obj1$color_info = list(left = set_colors_left, right = set_colors_right)

  return(obj1)
}
