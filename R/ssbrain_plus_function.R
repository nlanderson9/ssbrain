`+.ssbrain` = function(obj1, obj2) {
  if (is.dscalar(obj2)) output = add_dscalar(obj1, obj2)
  if (is.dlabel(obj2)) output = add_dlabel(obj1, obj2)
  if (is.dconn(obj2)) output = add_dconn(obj1, obj2)
  if (is.border(obj2)) output = add_border(obj1, obj2)
  if (is.view(obj2)) output = add_view(obj1, obj2)
  return(output)
}