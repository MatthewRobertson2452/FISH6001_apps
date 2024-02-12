FISH 6001 Lecture Activities
================
Matt Robertson
2024-02-12

# Activities

This respository contains shiny apps and simulation-based markdown
documents to accompany the population dynamics module lectures for FISH
6001 (Ecology, Management, and Practice of North Atlantic Fisheries) at
the Marine Institute of Memorial University of Newfoundland.

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
