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

exportCifti = function(cifti_filename, data, luttpath="/projects/b1134/tools/workbench_tools/Label_color_codes/Yeo17_Colours.txt") {
  data = data %>% replace_na(0)
  if (length(data) == 81924) {
    mesh='fsaverage6'
    template="/projects/b1134/tools/workbench_tools/fsaverage6/surf/fsaverage6_cifti_template.dscalar.nii"
    surf_l="/projects/b1134/tools/workbench_tools/fsaverage6/surf/lh.pial_infl2.surf.gii"
    surf_r="/projects/b1134/tools/workbench_tools/fsaverage6/surf/rh.pial_infl2.surf.gii"
  } else if (length(data) == 327684) {
    mesh='fsaverage'
    template="/projects/b1134/tools/workbench_tools/fsaverage7/surf/fsaverage7_cifti_template.dscalar.nii"
    surf_l="/projects/b1134/tools/workbench_tools/fsaverage7/surf/lh.pial_infl4.surf.gii"
    surf_r="/projects/b1134/tools/workbench_tools/fsaverage7/surf/rh.pial_infl4.surf.gii"
  } else {
    stop("This function only supports `fsaverage6` and `fsaverage`. Make sure that your `data` argument contains either 81924 or 327684 vertices.")
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