#' @title Create a Color Code
#'
#' @description Create a color HEX code from an RGB triple in a vector
#'
#' @param x A vector with three RGB values
#'
#' @import grDevices

rgb_fun1 = function(x) {
  rgb(x[1], x[2], x[3], 255, maxColorValue = 255)
}

#' @title Create Color Maps
#'
#' @description Creates the necessary outputs for colormaps used by ss_dscalar and ss_seed
#'
#' @param colorbar_name A string with the name of the color bar
#' @param pos_list The list of positive color values to create the palette
#' @param neg_list The list of negative color values to create the palette
#'
#' @import grDevices
#' @import viridisLite

createOutputList = function(colorbar_name, pos_list, neg_list) {

  if (colorbar_name %in% c("TURBO", "VIRIDIS", "INFERNO", "MAGMA", "PLASMA", "ROCKET", "MAKO", "CIVIDIS")) {
    pos_color_order = viridis(n=100, option = tolower(colorbar_name))
    neg_color_order = c(pos_color_order[1], pos_color_order[1])

    neg_low = neg_color_order[1]
    neg_high = neg_color_order[length(neg_color_order)]
    pos_low = pos_color_order[1]
    pos_high = pos_color_order[length(pos_color_order)]
  } else {
    neg_color_order = unlist(lapply(neg_list, rgb_fun1))
    # neg_color_order = rev(neg_color_order)
    pos_color_order = unlist(lapply(pos_list, rgb_fun1))

    neg_high = rgb(neg_list[[1]][1], neg_list[[1]][2], neg_list[[1]][3], 1, maxColorValue = 255)
    neg_low = rgb(neg_list[[length(neg_list)]][1], neg_list[[length(neg_list)]][2], neg_list[[length(neg_list)]][3], 1, maxColorValue = 255)
    pos_low = rgb(pos_list[[1]][1], pos_list[[1]][2], pos_list[[1]][3], 1, maxColorValue = 255)
    pos_high = rgb(pos_list[[length(pos_list)]][1], pos_list[[length(pos_list)]][2], pos_list[[length(pos_list)]][3], 1, maxColorValue = 255)
  }

  neg_palette = colorRampPalette(neg_color_order)
  pos_palette = colorRampPalette(pos_color_order)

  return(list(neg_low=neg_low, neg_high=neg_high, pos_low=pos_low, pos_high=pos_high, neg_palette=neg_palette, pos_palette=pos_palette))
}

#' @title Generate Color Bar
#'
#' @description Given a name or color palette(s), generate the necessary information to use them in brain maps
#'
#' @param colorbar_name A string with the name of the color bar
#' @param neg_colorpalette (Optional argument) A custom color palette created by colorRampPalette, to be used for negative values
#' @param pos_colorpalette (Optional argument) A custom color palette created by colorRampPalette, to be used for positive values
#'
#' @import grDevices

colorbarGenerator = function(colorbar_name, neg_colorpalette, pos_colorpalette) {
  if (!missing(colorbar_name)) {
    if (colorbar_name == "ROY-BIG-BL") {
      pos_color_order_list = list(c(0,0,0),
                                  c(60,0,0),
                                  c(100,0,0),
                                  c(150,0,0),
                                  c(200,0,0),
                                  c(255,0,0),
                                  c(255,120,0),
                                  c(255,200,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(0,255,255),
                                  c(0 ,55,0),
                                  c(0,200,0),
                                  c(75,125,0),
                                  c(125,0,160),
                                  c(75,0,125),
                                  c(0,0,170),
                                  c(0,0,80),
                                  c(0,0,0))
    } else if (colorbar_name == "videen_style") {
      pos_color_order_list = list(c(0,0,0),
                                  c(102,0,51),
                                  c(51,51,76),
                                  c(76,76,127),
                                  c(127,127,204),
                                  c(0,255,0),
                                  c(16,176,16),
                                  c(255,255,0),
                                  c(255,153,0),
                                  c(255,105,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,0),
                                  c(187,187,187),
                                  c(221,221,221),
                                  c(255,255,255),
                                  c(255,56,141),
                                  c(226,81,226),
                                  c(16,176,16),
                                  c(0,255,0),
                                  c(0,255,255),
                                  c(0,0,0))
    } else if (colorbar_name == "Gray_Interp_Positive") {
      pos_color_order_list = list(c(0,0,0),
                                  c(255,255,255))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "PSYCH-FIXED") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,68,0),
                                  c(255,105,0),
                                  c(255,204,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(0,255,255),
                                  c(0,204,255),
                                  c(0,153,255),
                                  c(0,105,255),
                                  c(0,58,255))
    } else if (colorbar_name == "RBGYR20") {
      pos_color_order_list = list(c(51,255,0),
                                  c(0,255,0),
                                  c(0,220,51),
                                  c(0,185,102),
                                  c(0,151,153),
                                  c(0,116,204),
                                  c(0,81,255),
                                  c(52,65,204),
                                  c(102,49,153),
                                  c(153,32,102),
                                  c(204,16,51))
      neg_color_order_list = list(c(255,0,0),
                                  c(255,51,0),
                                  c(255,102,0),
                                  c(255,153,0),
                                  c(255,204,0),
                                  c(255,255,0),
                                  c(204,255,0),
                                  c(153,255,0),
                                  c(102,255,0))
    } else if (colorbar_name == "RBGYR20P") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,51,0),
                                  c(255,102,0),
                                  c(255,153,0),
                                  c(255,204,0),
                                  c(255,255,0),
                                  c(204,255,0),
                                  c(153,255,0),
                                  c(102,255,0),
                                  c(51,255,0),
                                  c(0,255,0),
                                  c(0,220,51),
                                  c(0,185,102),
                                  c(0,151,153),
                                  c(0,116,204),
                                  c(0,81,255),
                                  c(52,65,204),
                                  c(102,49,153),
                                  c(153,32,102),
                                  c(204,16,51))
      neg_color_order_list = list(c(211,211,211),
                                  c(211,211,211))
    } else if (colorbar_name == "RYGBR4_positive") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,255,0),
                                  c(0,180,0),
                                  c(0,0,255),
                                  c(255,0,0))
      neg_color_order_list = list(c(255,0,0),
                                  c(255,0,0))
    } else if (colorbar_name == "RGRBR_mirror90_pos") {
      pos_color_order_list = list(c(220,0,0),
                                  c(255,255,0),
                                  c(0,180,0),
                                  c(255,255,0),
                                  c(220,0,0),
                                  c(255,100,255),
                                  c(0,0,255),
                                  c(255,100,255),
                                  c(220,0,0))
      neg_color_order_list = list(c(220,0,0),
                                  c(220,0,0))
    } else if (colorbar_name == "Orange-Yellow") {
      pos_color_order_list = list(c(254,130,2),
                                  c(254,254,126),
                                  c(254,254,254))
      neg_color_order_list = list(c(0,0,0),
                                  c(130,2,0),
                                  c(254,130,2))
    } else if (colorbar_name == "POS_NEG_ZERO") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,255),
                                  c(0,0,255))
    } else if (colorbar_name == "red-yellow") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(255,0,0),
                                  c(255,0,0))
    } else if (colorbar_name == "blue-lightblue") {
      pos_color_order_list = list(c(0,0,255),
                                  c(0,255,255))
      neg_color_order_list = list(c(0,0,255),
                                  c(0,0,255))
    } else if (colorbar_name == "FSL") {
      pos_color_order_list = list(c(255,0,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(0,255,255),
                                  c(0,0,255))
    } else if (colorbar_name == "power_surf") {
      pos_color_order_list = list(c(255,0,0),
                                  c(0,0,0.6*255),
                                  c(255,255,0),
                                  c(255,0.7*255,0.4*255),
                                  c(0,0.8*255,0),
                                  c(255,0.6*255,255),
                                  c(0,0.6*255,0.6*255),
                                  c(0,0,0),
                                  c(0.3*255,0,0.6*255),
                                  c(0.2*255,255,255),
                                  c(255,0.5*255,0),
                                  c(0.6*255,0.2*255,255),
                                  c(0,0.2*255,0.4*255),
                                  c(0.2*255,255,0.2*255),
                                  c(0,0,255),
                                  c(255,255,0.8*255),
                                  c(0,0.4*255,0),
                                  c(0.25*255,0.25*255,0.25*255))
      neg_color_order_list = list(c(255,0,0),
                                  c(255,0,0))
    } else if (colorbar_name == "black-red") {
      pos_color_order_list = list(c(127,0,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,0),
                                  c(127,0,0))
    } else if (colorbar_name == "black-green") {
      pos_color_order_list = list(c(0,127,0),
                                  c(0,255,0))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,127,0))
    } else if (colorbar_name == "black-blue") {
      pos_color_order_list = list(c(0,0,127),
                                  c(0,0,255))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,127))
    } else if (colorbar_name == "black-red-positive") {
      pos_color_order_list = list(c(0,0,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "black-green-positive") {
      pos_color_order_list = list(c(0,0,0),
                                  c(0,255,0))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "black-blue-positive") {
      pos_color_order_list = list(c(0,0,0),
                                  c(0,0,255))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "blue-black-green") {
      pos_color_order_list = list(c(0,0,0),
                                  c(0,255,0))
      neg_color_order_list = list(c(0,0,255),
                                  c(0,0,0))
    } else if (colorbar_name == "blue-black-red") {
      pos_color_order_list = list(c(0,0,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,255),
                                  c(0,0,0))
    } else if (colorbar_name == "red-black-green") {
      pos_color_order_list = list(c(0,0,0),
                                  c(0,255,0))
      neg_color_order_list = list(c(255,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "fsl_red") {
      pos_color_order_list = list(c(100,0,0),
                                  c(255,0,0))
      neg_color_order_list = list(c(100,0,0),
                                  c(100,0,0))
    } else if (colorbar_name == "fsl_green") {
      pos_color_order_list = list(c(0,100,0),
                                  c(0,255,0))
      neg_color_order_list = list(c(0,100,0),
                                  c(0,100,0))
    } else if (colorbar_name == "fsl_blue") {
      pos_color_order_list = list(c(0,0,100),
                                  c(0,0,255))
      neg_color_order_list = list(c(0,0,100),
                                  c(0,0,100))
    } else if (colorbar_name == "fsl_yellow") {
      pos_color_order_list = list(c(100,100,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(100,100,0),
                                  c(100,100,0))
    } else if (colorbar_name == "RedWhiteBlue") {
      pos_color_order_list = list(c(255,255,255),
                                  c(255,127,127),
                                  c(255,0,0))
      neg_color_order_list = list(c(0,0,255),
                                  c(127,127,255),
                                  c(255,255,255))
    } else if (colorbar_name == "cool-warm") {
      pos_color_order_list = list(c(221,221,221),
                                  c(229,216,209),
                                  c(236,211,197),
                                  c(241,204,185),
                                  c(245,196,173),
                                  c(247,187,160),
                                  c(247,177,148),
                                  c(247,166,135),
                                  c(244,154,123),
                                  c(241,141,111),
                                  c(236,127,99),
                                  c(229,112,88),
                                  c(222,96,77),
                                  c(213,80,66),
                                  c(203,62,56),
                                  c(192,40,47),
                                  c(180,4,38))
      neg_color_order_list = list(c(59,76,192),
                                  c(68,90,204),
                                  c(77,104,215),
                                  c(87,117,225),
                                  c(98,130,234),
                                  c(108,142,241),
                                  c(119,154,247),
                                  c(130,165,251),
                                  c(141,176,254),
                                  c(152,185,255),
                                  c(163,194,255),
                                  c(174,201,253),
                                  c(184,208,249),
                                  c(194,213,244),
                                  c(204,217,238),
                                  c(213,219,230),
                                  c(221,221,221))
    } else if (colorbar_name == "spectral") {
      pos_color_order_list = list(c(255,255,191),
                                  c(254,224,139),
                                  c(253,174,97),
                                  c(244,109,67),
                                  c(213,62,79),
                                  c(158,1,66))
      neg_color_order_list = list(c(94,79,162),
                                  c(50,136,189),
                                  c(102,194,165),
                                  c(171,221,164),
                                  c(230,245,152),
                                  c(255,255,191))
    } else if (colorbar_name == "RY-BC-BL") {
      pos_color_order_list = list(c(0,0,0),
                                  c(255,0,0),
                                  c(255,255,0))
      neg_color_order_list = list(c(0,255,255),
                                  c(0,0,255),
                                  c(0,0,0))
    } else if (colorbar_name == "magma") {
      pos_color_order_list = list(c(0, 0, 0),
                                  c(3, 3, 15),
                                  c(9, 7, 32),
                                  c(18, 13, 49),
                                  c(28, 16, 68),
                                  c(39, 18, 88),
                                  c(52, 16, 105),
                                  c(66, 15, 117),
                                  c(79, 18, 123),
                                  c(92, 22, 127),
                                  c(104, 28, 129),
                                  c(117, 33, 129),
                                  c(129, 37, 129),
                                  c(142, 42, 129),
                                  c(155, 46, 127),
                                  c(168, 50, 125),
                                  c(181, 54, 122),
                                  c(194, 59, 117),
                                  c(207, 64, 112),
                                  c(219, 71, 106),
                                  c(229, 80, 100),
                                  c(238, 91, 94),
                                  c(244, 105, 92),
                                  c(249, 120, 93),
                                  c(251, 135, 97),
                                  c(253, 150, 104),
                                  c(254, 165, 113),
                                  c(254, 180, 123),
                                  c(254, 194, 135),
                                  c(254, 209, 148),
                                  c(253, 224, 161),
                                  c(252, 238, 176),
                                  c(252, 253, 191))
      neg_color_order_list = list(c(0,0,0),
                                  c(0,0,0))
    } else if (colorbar_name == "JET256") {
      pos_color_order_list = list(c(0,0,127),
                                  c(0,0,255),
                                  c(0,255,255),
                                  c(255,255,0),
                                  c(255,0,0),
                                  c(127,0,0))
      neg_color_order_list = list(c(0,0,127),
                                  c(0,0,127))
    } else {
      pos_color_order_list = list()
      neg_color_order_list = list()
    }

    output_list = createOutputList(colorbar_name, pos_color_order_list, neg_color_order_list)
    return(output_list)
  } else {
    if (missing(neg_palette)) {
      pos_palette_colors = pos_palette(100)
      pos_low = pos_palette_colors[1]
      pos_high = pos_palette_colors[length(pos_palette_colors)]

      neg_palette = colorRampPalette(c(pos_low, pos_low))
      neg_low = pos_low
      neg_high = pos_low
    } else if (missing(pos_palette)) {
      neg_palette_colors = neg_palette(100)
      neg_low = neg_palette_colors[1]
      neg_high = neg_palette_colors[length(neg_palette_colors)]

      pos_palette = colorRampPalette(c(neg_low, neg_low))
      pos_low = neg_low
      pos_high = neg_low
    } else {
      pos_palette_colors = pos_palette(100)
      pos_low = pos_palette_colors[1]
      pos_high = pos_palette_colors[length(pos_palette_colors)]

      neg_palette_colors = neg_palette(100)
      neg_low = neg_palette_colors[1]
      neg_high = neg_palette_colors[length(neg_palette_colors)]
    }

    return(list(neg_low=neg_low, neg_high=neg_high, pos_low=pos_low, pos_high=pos_high, neg_palette=neg_palette, pos_palette=pos_palette))
  }

}
