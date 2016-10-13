#Documentation for developing new Apps for the DSAIDE Package

##Package structure 
* The main simulator functions, as well as a few helper functions are in the R/ folder
* The shiny apps are in the shinyapps/ subfolder. Each App has a corresponding www/ subfolder which contains the documentation. This folder is automatically generated as described below.

##Adding Apps
* Write the main simulator function, following the structure of the existing functions.
* Write the Shiny wrapper/App. Easiest by copying and modifying one of the existing apps.
* Add the simulator function and Shiny app to appropriate directories.

##To prepare App documentation
* Write the documentation for the app. A template exists in the allappdocumentation/rmarkdowntemplate/ folder. Add information to the template as described there. 
* Turn your documentation file into an HTML file and copy to the allappdocumentation/ folder. 
* First part of file name must be the same as name for the Shiny app. You can have additional text following an underscore, will be stripped.  
* Run the R script processAllFiles, which will execute a function by the same name inside the script. This splits html files. Each section in the html file that has a 'shinytabNNN' id will be extracted and placed into a separate html file. Any other sections are ignored.
* The newly created files will be placed in the www subfolders for each shiny app in inst/shinyapps/
* The www subfolder for each shiny app is deleted and re-created on each run, so should not contain any other files
* Naming for newly created html files is shinytab1.html, etc. and header.html/footer.html
* see comments in processAllFiles.R for more details

##Integrating Apps
* Add your App to the main menu by modifying dsaidemenu.R and the MainMenu Shiny app.R file.

##Contributing your Apps
* The best way to add your apps to the DSAIDE package is if you fork the package from github, write your app, then send a pull request.
* Alternatively, if you are not familiar with that approach, you can email me your new App files and I can manually integrate them.