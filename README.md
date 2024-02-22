FISH 6001 Lecture Activities
================
Matt Robertson
2024-02-12

# Activities

This repository contains lecture slides, shiny apps, simulation-based
markdown documents, and an assignment for FISH 6001 (Ecology,
Management, and Practice of North Atlantic Fisheries) at the Marine
Institute of Memorial University of Newfoundland.

## Lecture slides

The slides can be found as a pdf in the main folder, in the file titled,
`Fish 6001 - Population dynamics module`.

## Growth modeling shiny apps

I have included two shiny apps which show the basic exponential and
logistic growth functions. The parameters for these functions can be
modified to allow a visualization of exactly how the parameters shift
the model. Students should be able to run the shiny apps without needing
to directly access the github repo by using:

``` r
shiny::runGitHub( "FISH6001_apps","MatthewRobertson2452", subdir="/Exponential_growth/")
shiny::runGitHub( "FISH6001_apps","MatthewRobertson2452", subdir="/Logistic_growth/")
```

## Population dynamics simulations

I have also included two markdown documents which walk through
simulations for a surplus production model and an age-structured
population dynamics model. Students can just examine the output .md
files for these models or if they are more interested in understanding
how these models work, they can open the files themselves and modify
some of the parameters to see how they modify the simulations. If
parameters are modified, the files will need to be knit in order to see
the effects of those modifications.

## Assignment

The assignment for this module involves using an Rmarkdown simulation to
examine the theoretical population dynamics for your fish stock. This
will involve finding the necessary traits as inputs to run the
population dynamics model (8 points),identifying differences between
your stock and a pre-defined stock (5 points), and determining whether
your stock maintains a more sustainable fishery than a pre-defined stock
(2 points).

To access the assignment you will need to download
`Population_dynamics_module_assignment.Rmd` and `simulated_pop_dy.RData`
from the `Assignment` folder. Please make changes and write your answers
in the markdown file and then submit the markdown file and knitted pdf
output to my email (<matthew.robertson@mi.mun.ca>).
