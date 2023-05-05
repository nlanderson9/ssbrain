getSeedVector = function(brain, seed_start, seed_end, dlabel) {
  
  if(!missing(dlabel)) {
    dlabel_data = importCifti(dlabel)
    dlabel_data = dlabel_data$data$normal
  }
  
  seed_list = c()
  vertex_list = as.data.frame(t(brain$surf_info$left$vertices))
  faces_list = as.data.frame(t(brain$surf_info$left$faces))
  colnames(vertex_list) = c("x", "y", "z", "w")
  vertex_list$index = 1:nrow(vertex_list)
  
  seed1 = seed_start
  seed2 = seed_end
  curr_seed = seed1
  seed_list = c()
  go=TRUE
  while(go){
    all_faces = faces_list %>%
      filter(if_any(everything(), ~ .x==curr_seed))
    options = unique(unlist(all_faces))[unique(unlist(all_faces)) != curr_seed]
    shortest_dist = 10000000
    closest_seed = NA
    for (i in 1:length(options)) {
      option_seed = options[i]
      dist = sqrt((vertex_list$x[seed2]-vertex_list$x[option_seed])^2 + (vertex_list$y[seed2]-vertex_list$y[option_seed])^2 + (vertex_list$z[seed2]-vertex_list$z[option_seed])^2)
      if (dist < shortest_dist) {
        shortest_dist = dist
        closest_seed = option_seed
      }
    }
    if (closest_seed == seed2) {
      go=FALSE
    } else {
      seed_list = c(seed_list, closest_seed)
      curr_seed = closest_seed
    }
  }
  
  seed_list = c(seed1, seed_list, seed2)
  
  if (missing(dlabel)) {
    return(seed_list)
  } else {
    output_frame = data.frame(seed = seed_list, label = dlabel_data[seed_list])
    return(output_frame)
  }
}