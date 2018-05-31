# Documentation for working on the DSAIDE package and developing new Apps 

**The following information will hopefully help anyone (including the future self of the package author/maintainer) working on the package understand how things work and what to do.**

## Adding new apps 

There are 2 main ways one can add new apps:

1. The easiest way is to use the provided material in the 'newappfiles.zip' file. Extract the zip file into some folder. You can then use these files to build and test your app, as described below. Once it works, you can send it to me and I'll test it and if it works and I think it's a suitable addition, I'll add it to the package.

2. A more seamless but also more complicated approach is to fork the whole package, add the app within the package, and push the updated package back. This requires becoming familiar with all parts of the package and some understanding how R packages work in general. If this is the first time you are contributing an app to this package, I suggest you use the simpler approach first.

If you plan to contribute new apps, it is best if you contact me (ahandel@uga.edu or via github.com/ahgroup/DSAIDE) and we can discuss beforehand, this will likely make for a smoother process and I can provide additional help as needed.

## Adding a new app using the stand-alone approach

The following sections provide a detailed description on how to create a new app using the files provided in the zip file. Further below I provide some additional information should you try to build new apps inside the package.

### Unpack everything
Unpack the zip file and place all files into a single directory. You should have several generate_XX.R helper functions, a simulate_XX.R template, the XX_Documentation.Rmd file and the main app.R file. There are also a few dsaide... files that are needed, you can just keep them the way they are. **The only files you should modify are the simulate_mynewapp.R, app.R and the mynewapp_Documentation.Rmd files.** 

### Write the simulator function
Write the main simulator function, **simluate_mynewapp.R**. The provided file contains code for one of the existing apps. Replace the code with yours, following the structure of the existing functions. You can also copy/paste/modify another one of the existing simulator functions that is closer to what yours will look like. Test that your function works by sourcing it and executing it on its own. Once you have confirmed your function works as expected, you can move to the next step.

### Write the shiny wrapper/app
The app.R file is a shiny app that contains the user interface (inside the 'ui' function) and the server side (inside the 'server' function). The easiest approach is to use one of the existing apps and modify it as needed. You could either use the provide app.R file, or the app.R file of the app that's closest to the one you want to write. If you use an app.R as starting point that's different than the one provided in the zip file, make sure to include all the `source`  commands at the top of the provided app.R, and modify them as described. You can test your app by running it through R studio. Note that for this to work, you have to go through the 'write app documentation' steps described next. You can start by just writing some dummy text or use the provided template and fill it later.

### Write app documentation
Write the documentation for the app as R markdown file. Use the provided mynewapp_Documentation.Rmd template. Add information to the template as described in the template. Next, you need to process the Rmd file. To that end, run the provided generate_HTMLfilesforapps.R script. Make sure you are in the right working directory. This function takes Rmd files (all such files in the folder, make sure there is only the app doc) and turns it into HTML, then processes the HTML files and splits them. The newly created files will be placed in a **/www** subfolder. The **/www** subfolder is deleted and re-created on each run, so don't edit manually. You need to create the html documentation files before you can run/test your shiny app, so run it once even if you don't have much content for the documentation written yet.

### Test and submit app
Once you have created the documentation, you should be able to call the app by just using the 'run app' button in Rstudio (or call it from the R command line). Now modify/test/debug until everything works and all the documentation is written. Once all is done, you can email me the app, or send me an email telling me you have a new app and share it via Dropbox or through github or any other way. I'll check it and if it works and is suited for the package, I'll include it. Of course, I'll list you as contributor.


## Development of new app within the package
If you have already previously contributed an app and/or feel like you have a good understanding of R coding and packages in general and the structure of this one in particular, you can contribute by forking the whole package and working within the package. Pretty much all the above steps apply, but certain files are in specific sub-folders and with specific naming conventions that you need to be aware of. To that end, read the rest of this document.

## To work on package through RStudio
- The following documentation assumes that Rstudio is used.
- Load DSAIDE.Rproj in RStudio. Edit files as needed.
- Optionally, use RStudio tie-in with github to sync project to github (the 'git' tab).

## Package structure 
- The main R functions, i.e. the main menu and the simulation scripts are in the **/R** folder
- Every script that runs one of the simulations underlying each app is called **simulate_XXX.R** and in the **/R** folder
- The **/R** folder also has a few helper/convenience functions
- Folder **/vignettes** contains the files needed for the vignette. This content is copied to **/inst/doc** at some point. Any edits to the vignette should happen in the **/vignettes** folder, not **/inst/doc**.
- Folder **/man** contains the documentation for the R functions in **/R**, automatically generated by roxygen, should not be edited.
- Folder **/inst/docsfordevelopers** contains this file and a zip file to help someone develop new apps as described above.
- Folder **/inst/simulatorfunctions** contains copies of all the **simulate_XXX.R** code files from **/R** for easy access and editing by users.
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
- Add your App to the main menu by modifying the MainMenu Shiny app.R file (the graphical interface for the main menu).

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