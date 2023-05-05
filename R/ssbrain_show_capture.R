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


showBrain = function(brain,
                     hemisphere,
                     width=800,
                     height=500) {
  
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
  
  mesh_col <- rgl::shade3d(
    rgl::addNormals(brain_mesh),
    color=colors,
    specular="black",
    alpha=1.0,
    legend=TRUE
  )
  
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
      spheres3d(x=coords[1], y=coords[2], z=coords[3], r=size, color=color)
    }
  }
}

captureBrain = function(brain,
                        hemisphere,
                        filename,
                        width=800,
                        height=500,
                        crop=TRUE,
                        cropmargin=10) {
  showBrain(brain,hemisphere, width, height)
  snapshot3d(filename, width=width, height=height,webshot=FALSE)
  # rgl.postscript(paste0(filename,".svg"), fmt="svg")
  # rsvg_png(paste0(filename,".svg"), filename, width=pixwidth, height=pixheight)
  
  if (crop) {
    img = image_read(filename)
    img = image_trim(img)
    img = image_border(img, color="white", geometry = paste(cropmargin,"x",cropmargin))
    image_write(image=img, path = filename)
  }
}

closeBrainViewers = function() {
  close3d(rgl.dev.list())
}