#' @title Check data bins
#'
#' @description Check to see which bin contains the value
#'
#' @param x The levels (bins) of the output of the \code{cut} function
#' @param y The value to check
#'
#' @return TRUE or FALSE

checkBin = function(x, y) {
  bin_bounds = strsplit(as.character(x[1]),split=",")[[1]]
  bin_bound1 = as.numeric(gsub("\\(", "", bin_bounds[1]))
  bin_bound2 = as.numeric(gsub("]", "", bin_bounds[2]))
  if (y > bin_bound1 & y <= bin_bound2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Set a Dscalar
#'
#' @description This function sets a dscalar data file on the surface of the brain
#'
#' @param filename A string with the full filepath to a dscalar file
#' @param colorbar A string containing the name of a colorbar to use (same names as Connectome Workbench)
#' @param show Whether to show positive data, negative data, or all data. Options are "all", "pos", or "neg". Defaults to "all".
#' @param pos_thresh The threshold below which to hide positive data. Defaults to NULL (no threshold).
#' @param neg_thresh The option above which to hide negative data. Defaults to NULL (no threshold).
#' @param pos_colorrange The positive range in which to spread the \code{colorbar}'s colors. Defaults to NULL (full data range).
#' @param neg_colorrange The negative range in which to spread the \code{colorbar}'s colors. Defaults to NULL (full data range).
#' @param dscalar_data (Optional argument) Rather than passing a dscalar file, you can directly pass in an R vector containing the data. This overrides \code{filename}.
#' @param pos_palette (Experimental) A color palette created by \code{colorRampPalette}; overrides \code{colorbar} for positive colors.
#' @param neg_palette (Experimental) A color palette created by \code{colorRampPalette}; overrides \code{colorbar} for negative colors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(filename = "/path/to/my/datafile.dscalar.nii")
#'
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain_new = my_brain +
#'   ss_dscalar(filename = "/path/to/my/datafile.dscalar.nii",
#'              colorbar = "ROY-BIG-BL",
#'              show= "pos",
#'              pos_thresh = 10,
#'              pos_colorrange = c(10,50))
#' }
#'
#' @details
#' Note: Adding multiple \code{ss_dscalar} items to the same object will cause overlapping brain maps, with the most recently-added on top.

ss_dscalar = function(filename,
                   colorbar="FSL",
                   show = "all",
                   pos_thresh = 1,
                   neg_thresh = -1,
                   pos_colorrange = c(1,3),
                   neg_colorrange = c(-1,-3),
                   dscalar_data,
                   pos_palette,
                   neg_palette) {

  if (missing(filename) & missing(dscalar_data)) {
    stop("ERROR in `ss_dscalar`: You must provide a dscalar filename.")
  }
  if (!missing(filename) & !missing(dscalar_data)) {
    stop("Warning for `ss_dscalar`: You've provided both a dscalar filename and dscalar data; the dscalar data will override the dscalar file.")
  }

  if(missing(filename)) {
    filename = NULL
  }

  if (!is.null(filename)) {
    if (!file.exists(filename)) {
      stop(paste0("ERROR in `ss_dscalar`: Your file ", filename, " doesn't exist."))
    }
    if (!grepl("*\\.dscalar\\.nii*",filename)) {
      cat(paste0("\nWarning for `dscalar`: your filename ", filename, " doesn't end in `.dscalar.nii` - check to make sure this is the right filetype.\n"))
    }
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
    stop("ERROR in `ss_dscalar`: Available colorbars are:\nROY-BIG-BL
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

  if (! show %in% c("pos", "neg", "all")) {
    stop("ERROR in `ss_dscalar`: Accepatable values for `show` are:\npos\nneg\nall")
  }

  if (!is.null(pos_thresh)) {
    if (!is.numeric(pos_thresh)) {
      stop("ERROR in `ss_dscalar`: The `pos_thresh` argument must be a number.")
    } else if (! pos_thresh >= 0) {
      stop("ERROR in `ss_dscalar`: The `pos_thresh` argument must be a number >= 0.")
    }
  }

  if (!is.null(neg_thresh)) {
    if (!is.numeric(neg_thresh)) {
      stop("ERROR in `ss_dscalar`: The `neg_thresh` argument must be a number.")
    } else if (! neg_thresh <= 0) {
      stop("ERROR in `ss_dscalar`: The `neg_thresh` argument must be a number <= 0.")
    }
  }

  if (!is.null(pos_colorrange)) {
    if (length(pos_colorrange) != 2) {
      stop("ERROR in `ss_dscalar`: The `pos_colorrange` argument must be a two-number vector (e.g. `c(1, 3)`)")
    }
    if (!is.numeric(pos_colorrange)) {
      stop("ERROR in `ss_dscalar`: The `pos_colorrange` argument must be a two-number vector (e.g. `c(1, 3)`)")
    }
    if (!all(pos_colorrange >=0)) {
      stop("ERROR in `ss_dscalar`: The `pos_colorrange` argument must be two numbers >= 0 (e.g. `c(1, 3)`)")
    }
  }

  if (!is.null(neg_colorrange)) {
    if (length(neg_colorrange) != 2) {
      stop("ERROR in `ss_dscalar`: The `neg_colorrange` argument must be a two-number vector (e.g. `c(-1, -3)`)")
    }
    if (!is.numeric(neg_colorrange)) {
      stop("ERROR in `ss_dscalar`: The `neg_colorrange` argument must be a two-number vector (e.g. `c(-1, -3)`)")
    }
    if (!all(neg_colorrange <=0)) {
      stop("ERROR in `ss_dscalar`: The `neg_colorrange` argument must be two numbers <= 0 (e.g. `c(-1, -3)`)")
    }
  }

  if (!is.null(pos_colorrange)) {
    if (pos_colorrange[1] > pos_colorrange[2]) {
      pos_colorrange = c(pos_colorrange[2], pos_colorrange[1])
    }
  }

  if (!is.null(neg_colorrange)) {
    if (neg_colorrange[1] > neg_colorrange[2]) {
      neg_colorrange = c(neg_colorrange[2], neg_colorrange[1])
    }
  }

  if (missing(dscalar_data)) {
    dscalar_data = NULL
  }

  if (!missing(pos_palette)) {
    if (!is.function(pos_palette)) {
      stop("ERROR in `dscalar: The `pos_palette` argument must be a palette function created by colorRampPalette().")
    }
  } else {
    pos_palette = NULL
  }
  if (!missing(neg_palette)) {
    if (!is.function(neg_palette)) {
      stop("ERROR in `dscalar: The `neg_palette` argument must be a palette function created by colorRampPalette().")
    }
  } else {
    neg_palette = NULL
  }

  output = list(filename = filename,
                colorbar = colorbar,
                show = show,
                pos_thresh = pos_thresh,
                neg_thresh = neg_thresh,
                pos_colorrange = pos_colorrange,
                neg_colorrange = neg_colorrange,
                dscalar_data = dscalar_data,
                pos_palette = pos_palette,
                neg_palette = neg_palette)
  class(output) = c("ssbrain", "dscalar")
  return(output)
}

#' @title Check if Dscalar
#'
#' @description This function checks if an object is of the class \code{ssdscalar}.
#'
#' @param x The object to check
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_dscalar = ss_dscalar(filename = "/path/to/my/datafile.dscalar.nii")
#' is.dscalar(new_dscalar)
#' }

is.dscalar = function(x) {
  inherits(x, "dscalar")
}

#' @title (Internal) Set a Dscalar
#'
#' @description The internal function that calculates the result for \code{\link[ssbrain]{ss_dscalar}}
#'
#' @param obj1 The existing \code{ssbrain} object
#' @param obj2 The new \code{ssdscalar} object to add

add_dscalar = function(obj1, obj2){
  filename = obj2$filename
  colorbar = obj2$colorbar
  show = obj2$show
  pos_thresh = obj2$pos_thresh
  neg_thresh = obj2$neg_thresh
  pos_colorrange = obj2$pos_colorrange
  neg_colorrange = obj2$neg_colorrange
  dscalar_data = obj2$dscalar_data
  pos_palette = obj2$pos_palette
  neg_palette = obj2$neg_palette

  if (is.null(dscalar_data)) {
    dscalar_data = importCifti(filename)
    all_data = dscalar_data$data$normal
  } else {
    all_data = as.matrix(dscalar_data)
  }

  l_verts = obj1$surf_info$left$num_verts
  r_verts = obj1$surf_info$right$num_verts
  total_verts = sum(l_verts, r_verts)

  if (nrow(all_data) != total_verts) {
    stop(paste0("ERROR in `ss_dscalar`: the number of vertices in your dscalar file ",filename," (",nrow(all_data),") does not match the number of vertices on your surface (",total_verts,")."))
  }


  if (!is.null(pos_palette) | !is.null(neg_palette)) {
    colorbar = NULL
  }

  if (!is.null(colorbar)) {
    color_data = colorbarGenerator(colorbar_name = colorbar)
  } else if (!is.null(pos_palette) & is.null(neg_palette)) {
    color_data = colorbarGenerator(pos_colorpalette = pos_palette)
  } else if (is.null(pos_palette) & !is.null(neg_palette)) {
    color_data = colorbarGenerator(neg_colorpalette = neg_palette)
  } else if (!is.null(pos_palette) & !is.null(neg_palette)) {
    color_data = colorbarGenerator(pos_colorpalette = pos_palette, neg_colorpalette = neg_palette)
  }

  neg_low = color_data$neg_low
  neg_high = color_data$neg_high
  pos_low = color_data$pos_low
  pos_high = color_data$pos_high
  neg_palette = color_data$neg_palette
  pos_palette = color_data$pos_palette

  data_min = min(all_data)
  data_max = max(all_data)

  if (is.null(pos_thresh)) {
    pos_thresh = 0
  }
  if (is.null(neg_thresh)) {
    neg_thresh = 0
  }
  if (is.null(pos_colorrange)) {
    pos_colorrange = c(0,data_max)
  }
  if (is.null(neg_colorrange)) {
    neg_colorrange = c(data_min, 0)
  }

  prop_pos_data = round((abs(data_max) / (abs(data_min)+abs(data_max)))*100)
  prop_neg_data = 100 - prop_pos_data

  if (prop_pos_data == 0) {
    pos_indices = which(all_data>0)
    neg_indices = which(all_data<0)
    pos_bins_labeled = as.factor(c(0))
    neg_bins_labeled = cut(all_data[neg_indices], prop_neg_data)
    pos_bins = as.numeric(pos_bins_labeled) + prop_neg_data
    neg_bins = as.numeric(neg_bins_labeled)

    binlist = rep(1, length(all_data))
    binlist[neg_indices] = neg_bins + 1
  } else if (prop_neg_data == 0) {
    pos_indices = which(all_data>0)
    neg_indices = which(all_data<0)
    pos_bins_labeled = cut(all_data[pos_indices], prop_pos_data)
    neg_bins_labeled = as.factor(c(0))
    pos_bins = as.numeric(pos_bins_labeled) + prop_neg_data
    neg_bins = as.numeric(neg_bins_labeled)

    binlist = rep(1, length(all_data))
    binlist[pos_indices] = pos_bins + 1
  } else {
    pos_indices = which(all_data>0)
    neg_indices = which(all_data<0)
    pos_bins_labeled = cut(all_data[pos_indices], prop_pos_data)
    neg_bins_labeled = cut(all_data[neg_indices], prop_neg_data)
    pos_bins = as.numeric(pos_bins_labeled) + prop_neg_data
    neg_bins = as.numeric(neg_bins_labeled)

    binlist = rep(1, length(all_data))
    binlist[pos_indices] = pos_bins + 1
    binlist[neg_indices] = neg_bins + 1
  }

  if (show %in% c("pos", "all")) {
    low_pos_bin = which(unlist(lapply(levels(pos_bins_labeled), checkBin, y=pos_colorrange[1])))
    if (length(low_pos_bin) == 0) {
      if (pos_colorrange[1] < min(all_data[pos_indices])) {
        low_pos_bin = 1
      } else {
        stop("ERROR: You've chosen a minimum colorrange value that is higher than your highest data value.")
      }
    }
    high_pos_bin = which(unlist(lapply(levels(pos_bins_labeled), checkBin, y=pos_colorrange[2])))
    if (length(high_pos_bin) == 0) {
      if (pos_colorrange[2] > max(all_data[pos_indices])) {
        high_pos_bin = length(levels(pos_bins_labeled))
      } else {
        stop("ERROR: You've chosen a maximum colorrange value that is lower than your lowest data value.")
      }
    }
  } else {
    low_pos_bin = 1
    high_pos_bin = length(levels(pos_bins_labeled))
  }

  if (show %in% c("neg", "all")) {
    low_neg_bin = which(unlist(lapply(levels(neg_bins_labeled), checkBin, y=neg_colorrange[2])))
    if (length(low_neg_bin) == 0) {
      if (neg_colorrange[2] < min(all_data[neg_indices])) {
        low_neg_bin = length(levels(neg_bins_labeled))
      } else {
        stop("ERROR: You've chosen a minimum colorrange value that is higher than ")
      }
    }
    high_neg_bin = which(unlist(lapply(levels(neg_bins_labeled), checkBin, y=neg_colorrange[1])))
    if (length(high_neg_bin) == 0) {
      if (neg_colorrange[1] < max(all_data[neg_indices])) {
        high_neg_bin = 1
      } else {
        stop("ERROR: You've chosen a maximum colorrange value that is lower than ")
      }
    }
  } else {
    low_neg_bin = length(levels(neg_bins_labeled))
    high_neg_bin = 1
  }

  if (prop_pos_data == 0) {
    num_poscolors = 0
    num_pos_lowcolors = 0
    num_pos_highcolors = 0
  } else {
    num_poscolors = high_pos_bin - low_pos_bin + 1
    num_pos_lowcolors = low_pos_bin - 1
    num_pos_highcolors = prop_pos_data - high_pos_bin
  }
  if (prop_neg_data == 0) {
    num_negcolors = 0
    num_neg_lowcolors = 0
    num_neg_highcolors = 0
  } else {
    num_negcolors = low_neg_bin - high_neg_bin + 1
    num_neg_lowcolors = high_neg_bin - 1
    num_neg_highcolors = prop_neg_data - low_neg_bin
  }


  pos_colors = pos_palette(num_poscolors)
  neg_colors = neg_palette(num_negcolors)

  allcolors = c("#D3D3D3", rep(neg_high, num_neg_lowcolors), neg_colors, rep(neg_low, num_neg_highcolors), rep(pos_low, num_pos_lowcolors), pos_colors, rep(pos_high, num_pos_highcolors))

  colors = allcolors[binlist]

  below_thresh = which(all_data < pos_thresh & all_data > neg_thresh)
  colors[below_thresh] = "#D3D3D3"
  if (show == "pos") {
    colors[which(all_data < 0)] = "#D3D3D3"
  }
  if (show == "neg") {
    colors[which(all_data > 0)] = "#D3D3D3"
  }

  dscalar_colors_left = colors[1:l_verts]
  dscalar_colors_right = colors[(l_verts+1):total_verts]

  # Get existing colors; don't overwrite existing data with blank vertices
  current_colors_left = obj1$color_info$left
  current_colors_right = obj1$color_info$right

  set_colors_left = dscalar_colors_left
  set_colors_right = dscalar_colors_right
  set_colors_left[which(dscalar_colors_left == "#D3D3D3")] = current_colors_left[which(dscalar_colors_left == "#D3D3D3")]
  set_colors_right[which(dscalar_colors_right == "#D3D3D3")] = current_colors_right[which(dscalar_colors_right == "#D3D3D3")]


  obj1$dscalar_info = list(dscalar_data = list(left = as.matrix(all_data[1:l_verts]),
                                                right = as.matrix(all_data[(l_verts+1):total_verts])),
                            colorbar = colorbar,
                            show = show,
                            pos_thresh = pos_thresh,
                            neg_thresh = neg_thresh,
                            pos_colorrange = pos_colorrange,
                            neg_colorrange = neg_colorrange,
                            colors = colors)
  obj1$color_info = list(left = set_colors_left, right = set_colors_right)

  return(obj1)

}
