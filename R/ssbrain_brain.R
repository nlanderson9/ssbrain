ss_brain = function(surf=NULL, surfL = NULL, surfR = NULL) {
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
    stop("ERROR in `ss_brain`: You must set a surface `surf` or `surfL`/`surfR`.")
  }

  if (!is.null(surf) & (!is.null(surfL) | !is.null(surfR))) {
    stop("ERROR in `ss_brain`: Using the `surf` argument automatically sets the `surfL` and `surfR` arguments; you cannot use both.")
  }
  if (!is.null(surf)) {
    if (! surf %in% c("fsaverage6", "fsaverage7")) {
      stop("ERROR in `ss_brain`: The `surf` argument must be 'fsaverage6' or 'fsaverage7'.")
    }
  }
  if (!is.null(surfL)) {
    if (!file.exists(surfL)) {
      stop(paste0("ERROR in `ss_brain`: The file ",surfL," can't be found."))
    }
  }
  if (!is.null(surfR)) {
    if (!file.exists(surfR)) {
      stop(paste0("ERROR in `ss_brain`: The file ",surfR," can't be found."))
    }
  }

  if (!is.null(surf)) {
    if (surf == "fsaverage6") {
      surfL = "/projects/b1134/tools/workbench_tools/fsaverage6/surf/lh.pial_infl2.surf.gii"
      surfR = "/projects/b1134/tools/workbench_tools/fsaverage6/surf/rh.pial_infl2.surf.gii"

      surfL = importSurface(surfL)
      surfR = importSurface(surfR)

      brain$surf_info = list(left = surfL, right = surfR, border_vertices = readRDS("/projects/b1134/tools/brain_screenshot/border_vertex_coords_fs6.RDS"))
    }
    else if (surf == "fsaverage7") {
      surfL = "/projects/b1134/tools/workbench_tools/fsaverage7/surf/lh.pial_infl4.surf.gii"
      surfR = "/projects/b1134/tools/workbench_tools/fsaverage7/surf/rh.pial_infl4.surf.gii"

      surfL = importSurface(surfL)
      surfR = importSurface(surfR)

      brain$surf_info = list(left = surfL, right = surfR, border_vertices = readRDS("/projects/b1134/tools/brain_screenshot/border_vertex_coords_fs7.RDS"))
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
