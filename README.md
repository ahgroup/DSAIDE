# DSAIDE
Dynamical Systems Approach to Infectious Disease Epidemiology

## Description
This is an R package consisting of a set of Shiny Apps to teach infectious disease epidemiology from a dynamical system perspective.
By manipulating the models through the Shiny UI interface and working through the instructions provided within the Shiny UI, you can learn about some important concepts in infectious disease epidemiology. 
You will also learn how models can be used to study such concepts.

## Installing
1. Install the CRAN release in the usual way with install.packages()
2. The latest development version (potentially buggy) can be installed from github, using the devtools package function via: install_github("ahgroup/DSAIDE")

## Basic Use
After loading the package, run `dsaidemenu()` to start the main menu. 
From there choose the different apps corresponding to different infectious disease topics and scenarios.
Each app contains a description of the model and scenario that is implemented.
Each app also contains a list of recommeded tasks to work through in order to learn about a specific infectious topic.
Once done exploring the apps, close the main menu to exit back to the R console.

## Advanced Use
The paper and vignette mentioned below explain ways to interact with and modify the underlying models without the use of the Shiny GUI.

## Contributing to the package
DSAIDE is built in a way that makes it (hopefully) easy for others to contribute new simulations/apps. The package contains a sub-folder called _/docsfordevelopers_ (in the locally installed version of the package, this folder is in the main package folder, on Github it is inside the _/inst_ folder). The documentation.md file in this folder explains the overall structure of the package and gives detailed instructions on how to create new apps. The information provided is meant to be detailed and complete enough to allow fairly easy development and contribution of new apps (or other enhancements) to the package. For any further questions, feel free to contact me via email or github.

## Further information
* I published a paper describing the package and how to use it which you can find and read [here](https://doi.org/10.1371/journal.pcbi.1005642). Also use this paper if you want to cite the package.
* The vignette provides details about the different ways the package can be used. Currently the paper is probably better than the vignette, but in the future the vignette will be more up-to-date. 
* The `documentation.md' file described above contains more information about the package structure.
* For feedback, bug reports etc., use github at https://github.com/ahgroup/DSAIDE