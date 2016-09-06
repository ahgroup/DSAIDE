#library("gdata")
library("XML")

#first, get all of the HTML files in this folder,
#for each file call the separate function to split into individual html files
#then move them to the corresponding shiny app folder
processAllFiles <- function(){
  files = list.files(".", pattern = "\\.html$")

  for (i in seq(length(files)))
  {
    #get name of file currently to be processed
    current_filename = files[i]
    #strip out the _4_Practice.html part to create the right folders for the shiny app
    #in general, the name before the first underscore needs to correspond to the anme of the app
    #all information past the underscore is stripped
    foldername = unlist(strsplit(current_filename,split='_'))[1]

    #Clean up and recreate any previous files/folders

    # Delete the previously created subfolder for the given file
    unlink(foldername, recursive = TRUE)

    # find the path to its folder and copy all of the separated files there
    shinyapp.path <- sprintf("../shinyapps/%s/www", foldername)

    #Clean the folder and create it again
    unlink(shinyapp.path, recursive = TRUE)
    dir.create(shinyapp.path)

    # Call the separate function (see below) to separate the HTML
    #separated HTML files are saved to subfolder below the current one
    pathToDir <- separate(current_filename,foldername)

    # Here we copy the files over into the www directory of each shiny app
    results.files <- list.files(pathToDir)
    for (j in seq(results.files))
      {
                file.copy(from=sprintf("%s/%s", pathToDir, results.files[j]), to=shinyapp.path,
                overwrite = TRUE, recursive = FALSE,
                copy.mode = TRUE)
      } #finish loop to copy files
  } #finish loop over all files in folder
} #end of function

# This function will read a R markdown file, find the Practice section and
#   separate all the sub-sections from that part on.
#
# Parameters,
#   input: The main HTML file that needs to be separated
#   foldername: The name of the folder where the new files should be stored
##
# Return values: Path to the folder consisting of the separated HTML files
separate <- function(input,foldername){
  # Read and parse raw HTML file
  html.raw <- htmlTreeParse(input, useInternalNodes = TRUE)

  # Static text to start an HTML document
  html.head.start <- '<!DOCTYPE html> <html xmlns="http://www.w3.org/1999/xhtml">'

  # Get the head content to use later on
  html.head.content <- xpathApply(html.raw, "//head", saveXML)

  # Static text to end a head TAG
  html.head.end <- '</head>';

  # Static text to start a body TAG
  html.body.start <- '<body>'

  # Get the label of the page
  #html.body.label <- xpathApply(html.raw, "//div[@id[starts-with(., 'mylabel')]]", saveXML)

  # Get the header of the page as a Node list
  html.body.header <- xpathApply(html.raw, "//div[@id[starts-with(., 'myheader')]]", saveXML)

  # Find all the DIV tags within the html structure which has shinytab* ID
  html.parse.shinytabs = getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]") # xmlValue or saveXML

  # Find the footer of the page to be used later on
  html.body.footer <- xpathApply(html.raw, "//div[@id[starts-with(., 'myfooter')]]", saveXML)

  # Find the scripts of the page to be used later on
  #html.body.script <- xpathApply(html.raw, "//script[@type[starts-with(., 'text/javascript')]]", saveXML)
  html.body.script <- xpathApply(html.raw, "//script", xmlValue) #there is some weird CDATA field coming along if I use saveXML. not sure how to get rid, so just extract inside and slap script tag on again later


  # Static text to end a Body TAG
  html.body.end <- '</body>'

  # Static text to end an HTML TAG
  html.html.end <- '</html>'


  script.start.tag <- '<script>'
  script.end.tag <- '</script>'


  # Create a directory to copy all of the separated files of this HTML here!
  dir.create(sprintf("%s/%s", dirname(input), foldername))


  # Now loop over the tabs and create an HTML file for each one
  # Files are named simply as shinytab1.html, shinytab2.html, etc.
  # That's done because names of specific tabs can change between apps
  for (i in seq(length(html.parse.shinytabs))){

    subDoc <- xmlDoc(html.parse.shinytabs[[i]])

    # Set the Title of the Shiny Tab, which will be the file name
    title <- paste('shinytab', i, sep='')

    #this can be used if we wanted to use the sections as file names instead
    #title <- xpathApply(subDoc, "//h2", xmlValue)

    # Get the content of the shinytab
    content <- xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", saveXML)
    #content <- xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]")

    # Structure all the text that needs to be saved in the file
    # for each Shinytab.html file, we need to include the MathJax script bit at the end to allow equations to render ok
    # that should be script #2 in the html.body.script list
    toWrite <- paste(#html.head.start,
                     #html.head.content[[1]],
                     #html.head.end,
                     html.body.start,
                     #html.body.label[[1]],
                     #html.body.header[[1]],
                     content[[1]],
                     #html.body.footer[[1]],
                     #script.start.tag,
                     #html.body.script[[2]], #including MathJax script here doesn't seem to work with shiny, so leave it out for now
                     #script.end.tag,
                     html.body.end
                     #html.html.end
    )

    # This is the file that will be written in
    fileName <- sprintf("%s/%s/%s.html", dirname(input), foldername, gsub("\\s", "_", title))

    # Create a connection to the file
    fileConn <- file( fileName )

    # Write the lines
    writeLines(con = fileConn, text = toWrite)

    # Close the connection
    close(fileConn)
    #browser()

  }

  # Now, Save the title and the footer HTML files
  # Note that if header and footer are not present, this script will currently fail to run

  # Structure all the text that needs to be saved in the header html file
  toWrite <- paste(#html.head.start,
                   #html.head.content[[1]],
                   #html.head.end,
                   html.body.start,
                   #html.body.label[[1]],
                   html.body.header[[1]],
                   #content[[1]],
                   #html.body.footer[[1]],
                   #html.body.script[[1]],
                   html.body.end
                   #html.html.end
  )
  fileName <- sprintf("%s/%s/header.html", dirname(input), foldername)
  fileConn <- file( fileName )
  writeLines(con = fileConn, text = toWrite)
  close(fileConn)

  # Structure all the text that needs to be saved in the footer html file
  toWrite <- paste(html.head.start,
                   #html.head.content[[1]],
                   #html.head.end,
                   html.body.start,
                   #html.body.label[[1]],
                   #html.body.header[[1]],
                   #content[[1]],
                   html.body.footer[[1]],
                   #html.body.script[[1]],
                   html.body.end,
                   html.html.end
  )
  fileName <- sprintf("%s/%s/footer.html", dirname(input), foldername)
  fileConn <- file( fileName )
  writeLines(con = fileConn, text = toWrite)
  close(fileConn)


  return (sprintf("%s/%s", dirname(input), foldername))
}

################################################
#main program
#run the functions above to process all files
processAllFiles()

