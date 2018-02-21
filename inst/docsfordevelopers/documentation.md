# Documentation for working on the DSAIDE package and developing new Apps 

**The following information will hopefully help anyone (including the future self of the package author/maintainer) working on the package understand how things work and what to do.**

## Adding a new App 

There are 2 main ways one can add new apps:

1. The most seamless but also more complicated approach is to fork the whole package, add the app within the package, and push the updated package back. This requires becoming familiar with all parts of the package and some understanding how R packages work in general. If this is the first time you are contributing an app to this package, I suggest you use the simpler approach described next.

2. I provided a zip file in this folder (docsfordevelopers) which bundles all the material needed to create a new stand-alone app. You can use this to build and test your app. Once it works, you can send it to me and I'll test it and if it works and I think it's a suitable addition, I'll add it to the package.

If you plan to contribute new apps, it is best if you contact me (ahandel@uga.edu or via github.com/ahgroup/DSAIDE) and we can discuss beforehand, this will likely make for a smoother process and I can provide additional help as needed.

## Adding a new App using the stand-alone approach

The next steps below describe how you can add a new app using option 2, i.e. use the files provided in the zip file to create a new app. Further below I provide some additional information should you try to build new apps inside the package.

### Unpack everything
- Unzip the provided folder and place all files into a single directory. You should have several generate_XX.R helper functions, a simulate_XX.R template, the XX_Documentation.Rmd file and the main app.R file. There are also a few dsaide... files that are needed, you can just keep them the way they are. The only files you should need to modify are the simulate_ function, app.R and the XX_Documentation.Rmd file. 

### Write App code
- Write the main simulator function, **simluate_XXX.R**, following the structure of the existing functions. In the provided zip file, the simulator is that of the simple ID app. Replace contents as needed. You can also copy/paste/modify another one of the existing simulator functions that is closer to what yours will look like.
- Write the Shiny wrapper/App which calls the simulator you just wrote. Again, this is easiest by copying and modifying one of the existing apps. In the provided zip file, this is again the code for the basic ID app. There are a few additional statements, i.e. the source commands at the top, which call helper functions. If you use another app.R as starting point, make sure to include all those source commands, and modify them as described.

### Write App documentation
- Write the documentation for the app as R markdown file. Use the provided XX_Documentation.Rmd template. Add information to the template as described in the template. 
- Run the R script generate_HTML..R. Make sure you are in the right working directory. This function takes the Rmd file (all such files in the folder, make sure there is only the app doc) and turns it into HTML, then processes the HTML files and splits them. 
- The newly created files will be placed in a **/www** subfolder.
- The **/www** subfolder is deleted and re-created on each run, so don't edit manually
- you need to create the html documentation files before you can run/test your shiny app, so run it once even if you don't have much content for the documentation written yet


### Test and submit app
Once you have created the documentation, you should be able to call the app by just using the 'run app' button in Rstudio (or call it from the R command line). Now modify/test/debug until everything works and all the documentation is written.

Once all is done, you can email me the app, or send me an email telling me you have a new app and share it via Dropbox or through github or any other way. I'll check it and if it works and is suited for the package, I'll include it. Of course, I'll list you as contributor.


## Development of new app within the package

If you have already previously contributed an app and/or feel like you have a good understanding of R packages in general and the structure of this one in particular, you can contribute by forking the whole package and working within the package. Pretty much all the above steps apply, but certain files are in specific sub-folders and with specific naming conventions that you need to be aware of. To that end, read the rest of this document.

## To work on package through RStudio
- The following documentation assumes that Rstudio is used.
- Load DSAIDE.Rproj in RStudio. Edit files as needed.
- Optionally, use RStudio tie-in with github to sync project to github (the 'git' tab).

## Package structure 
- The main R functions, i.e. the main menu and the simulation scripts are in the **/R** folder
- Every script that runs one of the simulations underlying each app is called **simulate_XXX.R** and in the **/R** folder
- The **/R** folder also has a few helper/convenience functions
- Folder **/vignettes** contains the files needed for the vignette. This content is copied to **/inst/doc** at some point. Any edits to the vignette should happen in the **/vignettes** folder, not **/inst/doc**.
- Folder **/man** contains the documentation for the R functions in **/R**, automatically generated by roxygen, should not be edited
- Folder **/inst/docsfordevelopers** contains this file and a zip file to help someone develop new apps as described above
- Folder **/inst/simulatorfunctions** contains copies of all the **simulate_XXX.R** code files from **/R** for easy access and editing by users
- The shiny apps are in the **/inst/shinyapps** subfolder (which gets copied to **/shinyapps** in the deployed package). Each App has a corresponding **/www** subfolder which contains the documentation displayed inside each app. The **/www** folder and its content are automatically generated as described below.
- There are a few  folders in the **/inst/shinyapps** directory which do not correspond to shiny apps. Those are:
    - the **/allappdocumentation** folder contains Rmd files for the documentation content of all shiny apps. These files are processed by the **generate_HTML...R** script. Note that this is not part of the CRAN R package for size reasons, but if you fork the project from github it will be included.
    - The **/media** folder contains figures for each app/model as editable SVG source files and as png files which are the ones included in the apps. The folder also contains a bib file used as part of the documentation (i.e. inside the Rmd files). Note that this is not part of the CRAN R package for size reasons, but if you fork the project from github it will be included.
    - The **/styles** folder contains css styling for the apps and documentation.


## Further commments on documentation
See above for the basic description on how generation of the documentation for display inside the shiny app works.

Each section in the documentation html file that has a 'shinytabNNN' id will be extracted and placed into a separate html file. Any other sections are ignored. Those 'shinytabNNN' files are later loaded by the shiny UI and displayed inside the app.
- Naming for newly created html files is shinytab1.html, etc., as well as header.html/footer.html
- the source HTML files in **/shinyapps/allappdocumentation** will be deleted again.  
- see the comments in **generate_HTML...R** for more details


## Dependency packages for development
- roxygen
- devtools
- rmarkdown for vignette and shiny documentation
- packages needed by DSAIDE: see DESCRIPTION file


### Adding App to main menu
- Add your App to the main menu by modifying **dsaidemenu.R** (the script that runs the main menu) and the MainMenu Shiny app.R file (the graphical interface for the main menu).


## To update R documentation and vignette
- Edit documentation inside R functions. 
- Build documentation with Rstudio Build/More/Document or devtools::document()
- Edit vignette inside the **/vignettes** folder.
- To build new vignette, run devtools::build_vignettes()

## To build the DSAIDE package
- by hand" edit the DESCRIPTION file to make sure it's up to date
- in RStudio, use the functions in the 'build' tab to test and build the package.
- Run clean and rebuild, then build and reload using menu, or devtools::load_all()
- Run the check, fix any errors 

## To use package
- install either from CRAN or github
- for github install, install devtools package, load it
- then run install_github("ahgroup/DSAIDE")
- library('DSAIDE') 
- dsaidemenu()

## Notes
- All needed libraries should be loaded via the DESCRIPTION file and not in separate R files
- For more information on R package building (and the process followed for this package), see Hadley's R packages book: http://r-pkgs.had.co.nz/
