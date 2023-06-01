#' @title Set a Surface Mesh
#'
#' @description This is the core function of the \code{ssbrain} package. It creates an object that will store all of the surface information you want to display. It sets which surface mesh you plan to use.
#'
#' @param surf A string with the surface mesh you wish to use. Options are "fsaverage6" and "fsaverage7".
#' @param surfL (Optional argument) If you don't want to use fs6/fs7, you can provide the full path to a surface GIFTI file for the left hemisphere.
#' @param surfR (Optional argument) If you don't want to use fs6/fs7, you can provide the full path to a surface GIFTI file for the right hemisphere.
#'
#' @return An \code{ssbrain} object with the specified surface mesh(es).
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6")
#'
#' my_brain = ss_surf(surfL = "/path/to/surface/lh.pial_infl2.surf.gii")
#' }

ss_surf = function(surf=NULL, surfL = NULL, surfR = NULL, showMedialWall = TRUE, medialWallColor = "#858585") {
  brain = list(
    view_info=NULL,
    surf_info=NULL,
    dscalar_info=NULL,
    dlabel_info=NULL,
    dconn_info=NULL,
    borders_info=NULL,
    sphere_info=NULL,
    color_info=NULL)
  class(brain) = "ssbrain"

  if(!is.null(surf) & !is.null(surfL) & !is.null(surfR)) {
    stop("ERROR in `ss_surf`: You must set a surface `surf` or `surfL`/`surfR`.")
  }

  if (!is.null(surf) & (!is.null(surfL) | !is.null(surfR))) {
    stop("ERROR in `ss_surf`: Using the `surf` argument automatically sets the `surfL` and `surfR` arguments; you cannot use both.")
  }
  if (!is.null(surf)) {
    if (! surf %in% c("fsaverage6", "fsaverage7")) {
      stop("ERROR in `ss_surf`: The `surf` argument must be 'fsaverage6' or 'fsaverage7'.")
    }
  }
  if (!is.null(surfL)) {
    if (!file.exists(surfL)) {
      stop(paste0("ERROR in `ss_surf`: The file ",surfL," can't be found."))
    }
  }
  if (!is.null(surfR)) {
    if (!file.exists(surfR)) {
      stop(paste0("ERROR in `ss_surf`: The file ",surfR," can't be found."))
    }
  }

  if (!is.null(surf)) {
    if (surf == "fsaverage6") {
      surfL = system.file("extdata", "fs6", "lh.pial_infl2.surf.gii", package="ssbrain")
      surfR = system.file("extdata", "fs6", "rh.pial_infl2.surf.gii", package="ssbrain")

      surfL = importSurface(surfL)
      surfR = importSurface(surfR)
      if (showMedialWall) {
        brain$surf_info = list(left = surfL, right = surfR, border_vertices = border_vertex_coords_fs6, medial_wall = medial_wall_fs6, medial_wall_color = medialWallColor)
      } else {
        brain$surf_info = list(left = surfL, right = surfR, border_vertices = border_vertex_coords_fs6, medial_wall = NULL, medial_wall_color = NULL)
      }
    }
    else if (surf == "fsaverage7") {
      surfL = system.file("extdata", "fs7", "lh.pial_infl4.surf.gii", package="ssbrain")
      surfR = system.file("extdata", "fs7", "rh.pial_infl4.surf.gii", package="ssbrain")

      surfL = importSurface(surfL)
      surfR = importSurface(surfR)
      if (showMedialWall) {
        brain$surf_info = list(left = surfL, right = surfR, border_vertices = border_vertex_coords_fs7, medial_wall = medial_wall_fs7, medial_wall_color = medialWallColor)
      } else {
        brain$surf_info = list(left = surfL, right = surfR, border_vertices = border_vertex_coords_fs7, medial_wall = NULL, medial_wall_color = NULL)
      }
    }
  }
  else {
    if (!is.null(surfL)) {
      surfL = importSurface(surfL)
    }
    if (!is.null(surfR)) {
      surfR = importSurface(surfR)
    }
    brain$surf_info = list(left = surfL, right = surfR, border_vertices = list(left = NULL, right = NULL))
  }

  colors_left = rep("#D3D3D3", brain$surf_info$left$num_verts)
  colors_right = rep("#D3D3D3", brain$surf_info$right$num_verts)
  brain$color_info = list(left = colors_left, right = colors_right)

  return(brain)
}
