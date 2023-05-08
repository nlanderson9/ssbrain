ssbrain
================

***[PDF manual
here](https://github.com/nlanderson9/ssbrain/blob/main/docs/ssbrain_0.1.0.pdf)***

Welcome to the ssbrain package for R! This package allows you to use GIFTI, CIFTI, and Border files to capture brain surface images similar to the ones you would see in Connectome workbench.

To install:

``` r
# install.packages("devtools")
devtools::install_github("nlanderson9/ssbrain")
```

``` r
library(ssbrain)
```

This package allows you to "build" a brain out of all your desired elements (an underlying surface mesh, activation maps, label files, seed-based connectivity maps, borders) and then display it in a viewer window. You can then capture this window and save it as an image file. Specifying these files, and the parameters that modify how they are displayed, can all be altered programatically.

All of the builder functions in this package begin with the `ss_` prefix:

``` r
ss_surf()
ss_dscalar()
ss_dlabel()
ss_seed()
ss_border()
ss_view()
```

Each brain starts with a surface mesh - you can specify `fsaverage6`, `fsaverage7`, or provide a path to your own left/right hemisphere meshes.

``` r
my_brain = ss_surf(surf = "fsaverage6")

my_brain = ss_surf(surfL = "my_custom_surface_lh.surf.gii", surfR = "my_custom_surface_rh.surf.gii")
```

(You only need to provide the surface for the hemisphere that you want to display; choosing `fsaverage6`/`fsaverage7` automatically sets the correct meshes for both hemispheres)


Brains are built by adding function calls together (very similar to the syntax of `ggplot`) - this can be done all-at-once, or in multiple steps:

``` r 
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii")
  
# is equivalent to

my_brain = ss_surf(surf = "fsaverage6")
my_brain = my_brain +
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii")
  
# is equivalent to

initial_brain = ss_surf(surf = "fsaverage6")
my_brain = initial_brain + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii")
  ```
