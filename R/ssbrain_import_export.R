#' @title Import a GIFTI Surface File
#'
#' @description This function imports a GIFTI surface file into R, providing information for vertex coordinates in Euclidean space and the vertices used to draw triangular faces.
#'
#' @param surface_name A string with the full path to a GIFTI surface file
#'
#' @return A list with three items: vertices (Euclidean coordinates for each vertex), faces (the 3 vertex indices that make up each face), and num_verts (the total number of vertices in the mesh)
#'
#' @import gifti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' surface_data = importSurface("/path/to/surface/lh.pial_infl2.surf.gii")
#' }

importSurface = function(surface_name) {
  surf = readgii(surface_name)
  surf = surf$data
  surf = list(vertices = surf$pointset, faces = surf$triangle)
  if (min(surf$faces) == 0) {
    surf$faces = surf$faces + 1
  }
  surf$vertices = t(surf$vertices)
  surf$vertices = rbind(surf$vertices, 1)
  surf$faces = t(surf$faces)
  surf$num_verts = ncol(surf$vertices)

  return(surf)
}

#' @title Import a CIFTI Data File
#'
#' @description This function imports a CIFTI data file into R, providing information for each surface vertex (depending on the filetype).
#'
#' @param cifti_name A string with the full path to a CIFTI data file
#' @param data_only If TRUE, only provides a vector of the data. If FALSE, provides a full \code{gifti} object, including metadata. Default is FALSE.
#'
#' @return A gifti object (data_only = FALSE) or a vector of data (data_only = TRUE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cifti = importCifti("/path/to/cifti/my_file.dlabel.nii")
#'
#' # If `data_only` is FALSE (the default), the data inside the resulting
#' # object is located in `$data$normal`. For example:
#'
#' cifti_object = importCifti("/path/to/cifti/my_cifti.dscalar.nii")
#' cifti_data = cifti_object$data$normal
#'
#' # is equivalent to
#'
#' cifti_data = importCifti("/path/to/cifti/my_cifti.dscalar.nii",
#'                           data_only = TRUE)
#' }
#'
#' @details
#' This function currently only supports \code{.dscalar.nii}, \code{.dlabel.nii}, and \code{.dconn.nii} files.

importCifti = function(cifti_name, data_only=FALSE) {
  stopifnot("`cifti_name` provided does not exist" = file.exists(cifti_name))
  stopifnot("`cifti_name` doesn't have a valid CIFTI file extension (.dscalar.nii, .dlabel.nii, or .dconn.nii" = grepl("\\.dscalar\\.nii$|\\.dlabel\\.nii$|\\.dconn\\.nii$", cifti_name))
  cifti_basename = basename(cifti_name)
  cifti_basename_noext = tools::file_path_sans_ext(cifti_basename)

  cifti_dirname = dirname(cifti_name)
  gifti_name = file.path(cifti_dirname, paste0(cifti_basename_noext, ".gii"))

  if (is.null(getOption("ssbrain_wbpath"))) {
    stop("ERROR: please set your workbench directory path using:
         set_wbpath('/path/to/wb')")
  }

  cmd = sprintf("%s -cifti-convert -to-gifti-ext %s %s",
                getOption("ssbrain_wbpath"),
                cifti_name,
                gifti_name)

  system(cmd, intern=TRUE, ignore.stdout = TRUE)
  output = readgii(gifti_name)
  unlink(gifti_name)
  unlink(paste0(gifti_name,".data"))
  if(data_only) {
    output$data$normal
  } else {
    output
  }
}

#' @title Export a CIFTI Data File or Border File
#'
#' @description This function exports a CIFTI data file (or border file) from R, using a vector of data
#'
#' @param cifti_filename A string with the full path to the CIFTI data file you'd like to create
#' @param data A vector of data values.
#' @param luttpath (Optional argument) A path to a lutt color file; used for dlabel and border files. Default is the Yeo 17-parcel color scheme.
#'
#' @import dplyr
#' @import tidyr
#' @import gifti
#'
#' @import gifti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_data = runif(81924, 1.0, 100.0) # a vector of 81,924 random values between 1 and 100
#' exportCifti("/path/to/my/output.dscalar.nii", my_data)
#'
#' my_data = sample(1:15, 81924) # a vector of 81,924 random integers between 1 and 15
#' exportCifti("/path/to/my/output.dlabel.nii", my_data)
#' }
#'
#' @details
#' This function currently only supports \code{.dscalar.nii}, \code{.dlabel.nii}, and \code{.border} files.
#' For border files, outputs should be named filename.border. These will automatically be changed into left/right hemisphere files called filename_lh.border and filename_rh.border


exportCifti = function(cifti_filename, data, luttpath) {
  data = data %>% replace_na(0)
  if (length(data) == 81924) {
    mesh='fsaverage6'
    template=system.file("extdata","/fs6/fsaverage6_cifti_template.dscalar.nii", package = "ssbrain")
    surf_l=system.file("extdata","/fs6/lh.pial_infl2.surf.gii", package = "ssbrain")
    surf_r=system.file("extdata","/fs6/rh.pial_infl2.surf.gii", package = "ssbrain")
  } else if (length(data) == 327684) {
    mesh='fsaverage'
    template=system.file("extdata","/fs7/fsaverage7_cifti_template.dscalar.nii", package = "ssbrain")
    surf_l=system.file("extdata","/fs7/lh.pial_infl4.surf.gii", package = "ssbrain")
    surf_r=system.file("extdata","/fs7/rh.pial_infl4.surf.gii", package = "ssbrain")
  } else {
    warning("This function formally supports `fsaverage6` and `fsaverage`. Your `data` argument does not contain 81924 or 327684 vertices; proceed with caution.")
    template=system.file("extdata","/fs6/fsaverage6_cifti_template.dscalar.nii", package = "ssbrain")
  }

  if (grepl("dscalar",cifti_filename)) {
    filetype="dscalar"
  } else if (grepl("dlabel", cifti_filename)) {
    filetype="dlabel"
  } else if (grepl("border", cifti_filename)) {
    filetype="border"
  } else {
    stop("`cifti_filename` must end in either `.dscalar.nii`, `.dlabel.nii`, or `.border`")
  }

  if (missing(luttpath)) {
    luttpath = system.file("extdata", "Yeo17_Colours.txt", package="ssbrain")
    }

  if (is.null(getOption("ssbrain_wbpath"))) {
    stop("ERROR: please set your workbench directory path using:
         set_wbpath('/path/to/wb')")
  }

  cifti_basename = basename(cifti_filename)
  cifti_basename_noext = tools::file_path_sans_ext(cifti_basename)
  cifti_basename_noext = tools::file_path_sans_ext(cifti_basename_noext) # has to be done twice, because CIFTIs have two extensions (e.g. .dscalar.nii)

  cifti_dirname = dirname(cifti_filename)
  gifti_name_l = file.path(cifti_dirname, paste0(cifti_basename_noext, "_l.gii"))
  gifti_name_r = file.path(cifti_dirname, paste0(cifti_basename_noext, "_r.gii"))
  if (filetype == "dscalar") {
    dscalar_name = cifti_filename
  } else {
    dscalar_name = file.path(cifti_dirname, paste0(cifti_basename_noext, "_TEMP.dscalar.nii"))
  }


  template = importCifti(template)
  template$data_info$Dim0 = template$data_info$Dim0/2
  template$data_info$n = template$data_info$n/2
  template$data_info$Encoding = "Base64Binary"
  template_l = template
  template_r = template
  template_l$data$normal = data[1:template$data_info$Dim0]
  template_r$data$normal = data[(template$data_info$Dim0+1):(template$data_info$Dim0*2)]


  writegii(template_l, gifti_name_l)
  writegii(template_r, gifti_name_r)
  system(paste0(getOption("ssbrain_wbpath"), " -cifti-create-dense-scalar ",dscalar_name," -left-metric ",gifti_name_l," -right-metric ",gifti_name_r), intern=TRUE, ignore.stdout = TRUE)
  unlink(gifti_name_l)
  unlink(gifti_name_r)


  if (filetype %in% c("dlabel", "border")) {
    if (filetype == "dlabel") {
      dlabel_name = cifti_filename
    } else {
      dlabel_name = file.path(cifti_dirname, paste0(cifti_basename_noext, "_TEMP.dlabel.nii"))
    }
    system(paste0(getOption("ssbrain_wbpath"), " -cifti-label-import ",dscalar_name," ", luttpath," ", dlabel_name))
    unlink(dscalar_name)
  }

  if (filetype == "border") {
    label_name_l = file.path(cifti_dirname, paste0(cifti_basename_noext, "_lh.label.gii"))
    label_name_r = file.path(cifti_dirname, paste0(cifti_basename_noext, "_rh.label.gii"))
    border_name_l = file.path(cifti_dirname, paste0(cifti_basename_noext, "_lh.border"))
    border_name_r = file.path(cifti_dirname, paste0(cifti_basename_noext, "_rh.border"))

    system(paste0(getOption("ssbrain_wbpath"), " -cifti-separate ",dlabel_name," COLUMN -label CORTEX_LEFT ",label_name_l," -label CORTEX_RIGHT ",label_name_r));
    system(paste0(getOption("ssbrain_wbpath"), " -label-to-border ",surf_l," ",label_name_l," ",border_name_l," -placement 0.5"));
    system(paste0(getOption("ssbrain_wbpath"), " -label-to-border ",surf_r," ",label_name_r," ",border_name_r," -placement 0.5"));

    unlink(dlabel_name)
    unlink(label_name_l)
    unlink(label_name_r)
  }
}

#' @title Import a Border File
#'
#' @description This function imports a border file into R, providing information on vertices (and their ordering) used to form borders.
#'
#' @param border_name A string with the full path to a border file
#' @param border_surfname A string with the full path to the GIFTI surface used to create the border
#'
#' @return A gifti file containing border information
#'
#' @import gifti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' border_data = importBorder("/path/to/border/my_borderfile_lh.border",
#'                            border_surfname = "/path/to/surface/lh.pial_infl2.surf.gii")
#' }

importBorder = function(border_name, border_surfname) {
  border_basename = basename(border_name)
  border_extension = ".border"
  border_dirname = dirname(border_name)
  gifti_name = file.path(border_dirname, gsub(border_extension, ".border.func.gii", border_basename))

  if (is.null(getOption("ssbrain_wbpath"))) {
    stop("ERROR: please set your workbench directory path using:
         set_wbpath('/path/to/wb')")
  }

  cmd <- paste(
    getOption("ssbrain_wbpath"),
    "-border-to-vertices",
    border_surfname,
    border_name,
    gifti_name
  )
  system(cmd, intern=TRUE, ignore.stdout = TRUE)
  output = readgii(gifti_name)
  unlink(gifti_name)
  unlink(paste0(gifti_name,".data"))
  output
}

#' @title 'Distill' a Connectivity Dconn File
#'
#' @description This function "distills" a symmetrical .dconn.nii file containing connectivity information. It exports the correlation between each vertex and the specified vertex, resulting in a vector the length of the number of vertices. Functionally, it converts the dconn into a dscalar.
#'
#' @param dconn_filepath A string with the full path to the .dconn.nii file
#' @param seed_value A value for the seed (the row number, not vertex number)
#' @param num_verts The number of vertices (row/columns) in your dconn file
#'
#' @return An object of the same type returned by \link{importCifti} when used on a dscalar file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dconn_seed_data = distill_dconn(dconn_filepath = "/path/to/my/dconn/my_data.dconn.nii",
#'                                 seed_value = 2214, num_verts = 81924)
#' }

distill_dconn = function(dconn_filepath, seed_value, num_verts) {
  dconn_basename = basename(dconn_filepath)
  dconn_basename_noext = tools::file_path_sans_ext(dconn_basename)
  dconn_basename_noext = tools::file_path_sans_ext(dconn_basename_noext)

  if (is.null(getOption("ssbrain_wbpath"))) {
    stop("ERROR: please set your workbench directory path using:
         set_wbpath('/path/to/wb')")
  }

  dconn_dirname = dirname(dconn_filepath)
  reduced_dconn_name = file.path(dconn_dirname, paste0(dconn_basename_noext, "_TEMP.dconn.nii"))
  dscalar_mask = file.path(dconn_dirname, paste0(dconn_basename_noext, "_MASK_TEMP.dscalar.nii"))
  distilled_dscalar = file.path(dconn_dirname, paste0(dconn_basename_noext, "_TEMP.dscalar.nii"))

  exportCifti(dscalar_mask, c(rep(0,seed_value-1),1,rep(0,num_verts-seed_value)))

  cmd = sprintf("%s -cifti-restrict-dense-map %s ROW %s -cifti-roi %s",
                getOption("ssbrain_wbpath"),
                dconn_filepath,
                reduced_dconn_name,
                dscalar_mask)
  system(cmd, intern=TRUE, ignore.stdout = TRUE)

  cmd = sprintf("%s -cifti-change-mapping %s ROW %s -scalar",
                getOption("ssbrain_wbpath"),
                reduced_dconn_name,
                distilled_dscalar)
  system(cmd, intern=TRUE, ignore.stdout = TRUE)

  output = importCifti(distilled_dscalar)
  unlink(reduced_dconn_name)
  unlink(dscalar_mask)
  unlink(distilled_dscalar)
  output
}
