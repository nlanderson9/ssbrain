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

## Starting Your Brain

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

You can add multiple items to the same brain, of different kinds or more than one of the same kind:

``` r 
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity1.dscalar.nii") + 
  ss_dlabel(dlabel_filename = "regions.dlabel.nii") + 
  ss_dscalar(dscalar_filename = "task_activity2.dscalar.nii") +
  ss_border(border_filename = "network_parcellation.border", hemisphere = "left")
```

Adding items that overlap will layer them on top of each other on the brain. Each item is added in the order provided (i.e. the last items will appear on top, layed above the previous items). So, in the example above, the `task_activity2` dscalar will appear on top of the `regions` label file, which will appear on top of the `task_activity1` dscalar (borders are always on top of all other filetypes).

## Adding Layers

There are four primary functions that add data on top of a surface mesh:

1. `ss_dscalar`

This uses a dscalar file (`.dscalar.nii`) to add an activation map to the brain. You can use its arguments to adjust things like the color palette, threshold ranges, etc. Use the command `?ss_dscalar` in R to see all of the arguments, their explanations, and examples.

2. `ss_dlabel`

This uses a dlabel file (`.dlabel.nii`) to add a label map to the brain. You can use its arguments to adjust things like the which labels are displayed, their colors, etc. Use the command `?ss_dlabel` in R to see all of the arguments, their explanations, and examples.

3. `ss_seed`

This uses a symmetrical dconn file (`.dconn.nii`) to add a connectivity map to the brain. You can use its arguments to adjust things like the color palette, threshold ranges, etc. Use the command `?ss_seed` in R to see all of the arguments, their explanations, and examples.

4. `ss_border`

This uses a border file (`.border`) to add borders to the brain. You can use its arguments to adjust things like the which borders are displayed, their colors, etc. Use the command `?ss_border` in R to see all of the arguments, their explanations, and examples.


Additionally, you can set the view of your brain using `ss_view`. This is similar to Connectome Workbench, in that you can specify a side (lateral or medial) as well as an x/y/z rotation.

``` r
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii") + 
  ss_view(side = "lateral", rotation = "orbitofrontal")
  
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii") + 
  ss_view(side = "lateral", rotation = c(30,90,90)
```

You can use one of the pre-programmed views, or set your x/y/z rotation angles like you would in Workbench. See `?ss_view` for options.

Note: Unlike the other `ss_*` builder functions, `ss_view` instances do NOT stack, but rather overwrite. So

``` r
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii") + 
  ss_view(side = "lateral", rotation = "orbitofrontal") +
  ss_view(side = "lateral", rotation = "inferior_temporal")
```
will ONLY give you an inferior temporal view of the brain, as the orbitofrontal view was overwritten.

## Viewing and Capturing

Once you have built the brain you want, you can choose to either view or screenshot it.

Viewing uses the `showBrain` function, which opens an RGL viewer window and displays the brain:

``` r
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii") + 
  ss_view(side = "lateral", rotation = "orbitofrontal")

showBrain(my_brain, hemisphere = "left")
```

Further calls to `showBrain` will update the image in the existing window (if you closed the window, a new one will be opened).

You can also capture images using `captureBrain`:

``` r
my_brain = ss_surf(surf = "fsaverage6") + 
  ss_dscalar(dscalar_filename = "task_activity.dscalar.nii") + 
  ss_view(side = "lateral", rotation = "orbitofrontal")

captureBrain(my_brain, hemisphere = "left", filename = "my_brain_image.png")
```

Once you're finished looking at brains, you can either manually close the window(s), or use the following command to close all existing brain windows:
``` r
closeBrainViewers()
```

## Example

Now we can put this all together. Let's say you have 10 subjects, and each has two task activity maps. You have a pair of border files that you want to overlay on each task activity map, one-at-a-time. You want to capture each brain in 3 different views. If you were using Connectome Workbench, this would mean setting (and screenshotting) 10 x 2 x 2 x 3 = 120 different brain views. With `ssbrain`, all you need is:

``` r 
subjects = c("subject1", "subject2", "subject3", "subject4", "subject5",
             "subject6", "subject7", "subject8", "subject9", "subject10")
task_map_names = c("task1", "task2")
border_numbers = c(2,5)
views = list(c("lateral", "orbitofrontal"), c("lateral", "inferior_temporal"), c("medial", "orbitofrontal")

for (s in 1:length(subjects)) {
  subject = subjects[s]
  subject_brain = ss_surf(surf = "fsaverage6")
  for (t in 1:length(task_map_names)) {
    task_map = task_map_names[t]
    task_brain = subject_brain + 
      ss_dscalar(dscalar_filename = paste0("/my_directory/",subject,"/",task_map,".dscalar.nii"))
    for (b in 1:length(border_names)) {
      border = border_numbers[b]
      border_brain = task_brain +
        ss_border(border_filename = paste0("/my_directory/",subject,"/network_parcellation_lh.border",
                  hemisphere = "left",
                  borders = border)
      for (v in 1:length(views)) {
        view_side = views[[v]][1]
        view_rotation = views[[v]][2]
        border_brain = border_brain + 
          ss_view(side = view_side, rotation = view_rotation) # remember, ss_view overwrites previous views, so it's fine to keep updating the same object
        
        captureBrain(border_brain, hemisphere = "left",
                     filename = paste0("/my_output_directory/",subject,"_",task_map,"_",border,"_",view_side,"_",view_rotation,".png"))
      }
    }
  }
}
```
