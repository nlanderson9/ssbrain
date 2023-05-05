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

is.view = function(x) {
  inherits(x, "view")
}

add_view = function(obj1, obj2) {
  rotation = obj2$rotation
  side = obj2$side

  if (is.character(rotation)) {
    if (rotation == "orbitofrontal") {
      rotation = c(-0.93, 61.0, 89.64)
    } else if (view == "inferior_temporal") {
      rotation = c(-32.95, 154.80, 86.78)
    }
  }

  obj1$view_info = list(rotation = rotation, side = side)
  return(obj1)
}
