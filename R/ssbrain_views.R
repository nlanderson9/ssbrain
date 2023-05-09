#' @title List Available Rotations
#'
#' @description This function lists the pre-set rotations available in the \code{ssbrain} package for use with the \code{\link[ssbrain]{ss_view}} function.
#'
#' @export

listRotations = function() {
  cat("Available rotations are:\n
orbitofrontal
inferior_temporal")
}



#' @title Set a Viewpoint
#'
#' @description This function sets the view of the brain in the viewer window. It determines if a lateral or medial view is used, and the x/y/z axis rotation of the brain.
#'
#' @param side Which side of the brain to show, either "lateral" or "medial". Defaults to "lateral".
#' @param rotation The rotation applied to the \code{side} chosen. Either a pre-defined rotation (e.g. "orbitofrontal"), or a vector of x/y/z angles (e.g. c(90,120,90)).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_view(side="lateral", rotation = "inferior_temporal")
#'
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain_new = my_brain +
#'   ss_view(side="medial", rotation = c(30,90,90))
#' }
#'
#' @details
#' Note: \code{ss_view} is the only ss_* function that \strong{overwrites} any prexisting view, rather than adding on additional elements.

ss_view = function(side="lateral",
                rotation=c(0,90,90)) {
  if (!is.character(side)) {
    stop("ERROR in `ss_view`: The argument `side` must be either 'lateral' or 'medial'.")
  }
  if (! side %in% c("lateral", "medial")) {
    stop("ERROR in `ss_view`: The argument `side` must be either 'lateral' or 'medial'.")
  }

  if (is.character(rotation)) {
    if (! rotation %in% c("orbitofrontal", "inferior_temporal")) {
      stop("ERROR in `ss_view`: The available rotations are:\norbitofrontal\ninferior_temporal")
    }
  } else if (is.numeric(rotation)) {
    if ((any(rotation > 360)) | any(rotation < -360)) {
      stop("ERROR in `ss_view`: All rotation values for `rotation` must be between -360 and 360.")
    }
  } else {
    stop("ERROR in `ss_view`: The `rotation` argument must either be a three-number vector (e.g. `c(0, 90, 90)`) or a pre-set rotation (e.g. 'orbitofrontal').")
  }

  output = list(side=side, rotation=rotation)
  class(output) = c("ssbrain", "view")
  return(output)
}

#' @title Check if View
#'
#' @description This function checks if an object is of the class \code{ssview}.
#'
#' @param x The object to check
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_view = ss_view(side="lateral", rotation = "inferior_temporal")
#' is.view(new_view)
#' }

is.view = function(x) {
  inherits(x, "view")
}

#' @title (Internal) Set a Viewpoint
#'
#' @description The internal function that calculates the result for \code{\link[ssbrain]{ss_view}}
#'
#' @param obj1 The existing \code{ssbrain} object
#' @param obj2 The new \code{ssview} object to add


add_view = function(obj1, obj2) {
  rotation = obj2$rotation
  side = obj2$side

  if (is.character(rotation)) {
    if (rotation == "orbitofrontal") {
      rotation = c(-0.93, 61.0, 89.64)
    } else if (rotation == "inferior_temporal") {
      rotation = c(-32.95, 154.80, 86.78)
    }
  }

  obj1$view_info = list(rotation = rotation, side = side)
  return(obj1)
}
