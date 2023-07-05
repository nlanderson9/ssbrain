#' @title Calculation of View Matrix
#'
#' @description This function calculates the coordinates that \code{rgl} needs to rotate a brain.
#'
#' @param hemisphere "left" or "right"
#' @param side "lateral" or "medial"
#' @param wb_X The x-axis rotation as shown in Connectome Workbench
#' @param wb_Y The y-axis rotation as shown in Connectome Workbench
#' @param wb_Z The z-axis rotation as shown in Connectome Workbench
#'
#' @return A view matrix that can be passed to rgl::view3d as a userMatrix.
#'
#' @import rgl

get_viewmatrix = function(hemisphere, side, wb_X, wb_Y, wb_Z) {
  if (side == "lateral") {
    if (hemisphere == "left") {
      lateral_matrix = rbind(
        c( 0,-1, 0, 0),
        c( 0, 0, 1, 0),
        c(-1, 0, 0, 0),
        c( 0, 0, 0, 1)
      )
    } else {
      lateral_matrix = rbind(
        c( 0, 1, 0, 0),
        c( 0, 0, 1, 0),
        c( 1, 0, 0, 0),
        c( 0, 0, 0, 1)
      )
    }
    wb_X_transform = wb_X
    wb_Y_transform = (90 - wb_Y)
    wb_Z_transform = (90 - wb_Z)
    rgl_X_transform = wb_Z_transform
    if (hemisphere == "left") {
      rgl_Y_transform = -wb_Y_transform
      rgl_Z_transform = wb_X_transform
    } else {
      rgl_Y_transform = wb_Y_transform
      rgl_Z_transform = -wb_X_transform
    }
    viewmatrix = lateral_matrix
    viewmatrix = rotate3d(viewmatrix, rgl_X_transform*(pi/180),1,0,0)
    viewmatrix = rotate3d(viewmatrix, rgl_Z_transform*(pi/180),0,0,1)
    viewmatrix = rotate3d(viewmatrix, rgl_Y_transform*(pi/180),0,1,0)
    viewmatrix
  } else if (side == "medial") {
    if (hemisphere == "left") {
      medial_matrix = rbind(
        c( 0, 1, 0, 0),
        c( 0, 0, 1, 0),
        c( 1, 0, 0, 0),
        c( 0, 0, 0, 1)
      )
    } else {
      medial_matrix = rbind(
        c( 0,-1, 0, 0),
        c( 0, 0, 1, 0),
        c(-1, 0, 0, 0),
        c( 0, 0, 0, 1)
      )
    }
    wb_X_transform = wb_X
    wb_Y_transform = (90 - wb_Y)
    wb_Z_transform = (90 - wb_Z)
    rgl_X_transform = wb_Z_transform
    if (hemisphere == "left") {
      rgl_Y_transform = -wb_Y_transform
      rgl_Z_transform = wb_X_transform
    } else {
      rgl_Y_transform = wb_Y_transform
      rgl_Z_transform = -wb_X_transform
    }
    viewmatrix = medial_matrix
    viewmatrix = rotate3d(viewmatrix, rgl_X_transform*(pi/180),1,0,0)
    viewmatrix = rotate3d(viewmatrix, rgl_Z_transform*(pi/180),0,0,1)
    viewmatrix = rotate3d(viewmatrix, rgl_Y_transform*(pi/180),0,1,0)
    viewmatrix
  }
}

#' @title Visually Display Brain Surface
#'
#' @description This function opens an rgl window and displays a brain surface.
#'
#' @param brain An \code{ssbrain} object created by \link{ss_surf} (and potentially modified by other functions)
#' @param hemisphere Which brain hemisphere to display (either "left" or "right")
#' @param width The width of the window. Default is 800.
#' @param height The height of the window. Default is 500.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(dscalar_filename = "/path/to/my_file.dscalar.nii")
#'
#' showBrain(my_brain, hemisphere = "left")
#' }


showBrain = function(brain,
                     hemisphere,
                     width=800,
                     height=500,
                     ambient_color = "gray12") {

  if (missing(brain)) {
    stop("ERROR: You must provide a 'brain' object to show.")
  }
  if (class(brain) != "ssbrain") {
    stop("ERROR: You must provide a 'brain' object to show (use function `ssbrain` to create one).")
  }

  if (! hemisphere %in% c("left", "right")) {
    stop("ERROR: The `hemisphere` argument must be 'left' or 'right'.")
  }

  if (! hemisphere %in% names(brain$surf_info)) {
    stop(paste0("ERROR: You must include a ",hemisphere," brain surface before showing a ",hemisphere," brain. Use the `setSurface` or `updateSurface` functions."))
  }

  if (is.null(brain$surf_info[[hemisphere]])) {
    stop(paste0("ERROR: You must include a ",hemisphere," brain surface before showing a ",hemisphere," brain. Use the `setSurface` or `updateSurface` functions."))
  }

  if (length(rgl.dev.list()) == 0) {
    rgl::open3d(windowRect=c(0,0,width,height))
  } else {
    clear3d()
  }

  brain_mesh = tmesh3d(
    brain$surf_info[[hemisphere]]$vertices,
    brain$surf_info[[hemisphere]]$faces,
    meshColor = "vertices",
    material=NULL
  )

  if (is.null(brain$view_info)) {
    if (hemisphere == "left") {
      brainview = rbind(
        c( 0, -1,  0,  0),
        c( 0,  0,  1,  0),
        c(-1,  0,  0,  0),
        c( 0,  0,  0,  1)
      )
    } else {
      brainview = rbind(
        c( 0, 1,  0,  0),
        c( 0,  0,  1,  0),
        c(1,  0,  0,  0),
        c( 0,  0,  0,  1)
      )
    }
  } else {
    rotation_coords = brain$view_info$rotation
    side = brain$view_info$side
    brainview = get_viewmatrix(hemisphere, side, rotation_coords[1], rotation_coords[2], rotation_coords[3])
  }

  rgl::view3d(userMatrix=brainview, fov=0, zoom=.6)

  colors = brain$color_info[[hemisphere]]

  if (!is.null(brain$surf_info$medial_wall)) {
    medial_wall_vertices = brain$surf_info$medial_wall[[hemisphere]]
    colors[medial_wall_vertices] = brain$surf_info$medial_wall_color
  }

  mesh_col <- rgl::shade3d(
    rgl::addNormals(brain_mesh),
    color=colors,
    specular="black",
    ambient=ambient_color,
    emission="black",
    shininess=50,
    alpha=1.0,
    legend=TRUE
  )

  # wire3d(brain_mesh)

  if (!is.null(brain$borders_info[[hemisphere]])) {
    for (i in 1:length(brain$borders_info[[hemisphere]]$mesh_list)) {
      this_border = brain$borders_info[[hemisphere]]$mesh_list[[i]]
      wire3d(this_border)
    }
  }

  if (!is.null(brain$sphere_info[[hemisphere]]$sphere_list)) {
    for (i in 1:length(brain$sphere_info[[hemisphere]]$sphere_list)) {
      this_sphere_info = brain$sphere_info[[hemisphere]]$sphere_list[[i]]
      coords = this_sphere_info$coords
      size = this_sphere_info$size
      color = this_sphere_info$color
      spheres3d(x=coords[1], y=coords[2], z=coords[3], radius=size, color=color)
    }
  }
  # rglwidget()
}

#' @title Capture a Visually-displayed Brain Surface
#'
#' @description This function opens an rgl window and displays a brain surface, then saves a screenshot of it.
#'
#' @param brain An \code{ssbrain} object created by \link{ss_surf} (and potentially modified by other functions)
#' @param hemisphere Which brain hemisphere to display (either "left" or "right")
#' @param filename A string with the full path to the PNG file you'd like to create
#' @param width The width of the window. Default is 800.
#' @param height The height of the window. Default is 500.
#' @param crop Whether or not to crop the resulting image. Default is TRUE.
#' @param cropmargin The margin of whitespace to leave around the brain when cropping. Default is 10.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(dscalar_filename = "/path/to/my_file.dscalar.nii")
#'
#' captureBrain(my_brain, hemisphere = "left", filename = "/path/to/my/output_image.png")
#' }

captureBrain = function(brain,
                        hemisphere,
                        filename,
                        width=800,
                        height=500,
                        crop=TRUE,
                        cropmargin=10,
                        ambient_color = "gray12") {
  showBrain(brain,hemisphere, width, height, ambient_color)
  snapshot3d(filename, width=width, height=height,webshot=FALSE)
  # save_info = suppressWarnings(rglwidget(snapshot=TRUE))
  temp_path = save_info[1]
  file.copy(temp_path, filename)
  unlink(temp_path)


  # rgl.postscript(paste0(filename,".svg"), fmt="svg")
  # rsvg_png(paste0(filename,".svg"), filename, width=pixwidth, height=pixheight)

  if (crop) {
    img = magick::image_read(filename)
    img = magick::image_trim(img)
    img = magick::image_border(img, color="white", geometry = paste(cropmargin,"x",cropmargin))
    img = magick::image_convert(img, colorspace = "sRGB")
    magick::image_write(image=img, path = filename)
  } else {
    img = magick::image_read(filename)
    img = magick::image_convert(img, colorspace = "sRGB")
    magick::image_write(image=img, path = filename)
  }
}

#' @title Open a New Brain Window
#'
#' @description Forces a new brain window to open. All new instances of \code{\link[ssbrain]{showBrain}} and \code{\link[ssbrain]{captureBrain}} will use this window. This can be useful if you've already opened a brain window and don't want to overwrite/close it (e.g. to look at two brains at once).
#'
#' @param width The width of the window. Default is 800.
#' @param height The height of the window. Default is 500.
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain1 = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(dscalar_filename = "/path/to/my_file1.dscalar.nii")
#'
#' my_brain2 = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(dscalar_filename = "/path/to/my_file2.dscalar.nii")
#'
#' # To show my_brain1, then replace it with my_brain2:
#' showBrain(my_brain1, hemisphere = "left")
#' showBrain(my_brain2, hemisphere = "left")
#'
#' # To show my_brain1, then to open my_brain2 in a second window:
#' showBrain(my_brain1, hemisphere = "left")
#' openNewBrainViewer()
#' showBrain(my_brain2, hemisphere = "left")
#' }

openNewBrainViewer = function(width=800,
                              height=500) {
  rgl::open3d(windowRect=c(0,0,width,height))
}

#' @title Close All Open Brain Windows
#'
#' @description Closes all currently-open brain windows
#'
#' @import rgl
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_brain = ss_surf(surf="fsaverage6") +
#'   ss_dscalar(dscalar_filename = "/path/to/my_file.dscalar.nii")
#'
#' showBrain(my_brain, hemisphere = "left")
#'
#' closeBrainViewers()
#' }

closeBrainViewers = function() {
  close3d(rgl.dev.list())
}
