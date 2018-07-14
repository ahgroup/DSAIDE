#' @title A helper function for the UI part of the shiny apps
#'
#' @description This function take the documentation provided as Rmd file
#' If not present our outdated, it turns the Rmd file into an html file
#' it then takes the html file and extracts sections 
#' to generate the tabs with content for each Shiny app. 
#' This is a helper function and only useful for this package.
#' @return tablist a list of tabs for display in a shiny UI
#' @details This function is called by the shiny UIs to populate the documentation and information tabs
#' @author Andreas Handel
#' @export




generate_documentation <- function(currentdir)
{

    #first, check if no html file of documentation is available
    #or if it is outdated. In that case, use Rmd file to create
    #html file
    #for this to work, there can only be one Rmd and 0 or 1 html files
    #in the folder where the app is
    browser()
    htmlfile = list.files(path = currentdir, pattern = "\\.html$")
    rmdfile = list.files(path = currentdir, pattern = "\\.Rmd$")
    if (length(htmlfile)==0) #html file doesn't exist, create
    {
      rmarkdown::render(rmdfile)
    }      
    if (length(htmlfile)>0) #html file exists, check date
    {
      htmldate <- file.info(htmlfile)$mtime
      rmddate <- file.info(rmdfile)$mtime
      if ((rmddate-htmldate)>0) #if rmd file is newer, recreate html
      {
        rmarkdown::render(rmdfile)
      }
    }

    #now take HTML file and split it into components for each tab
    htmlfile = list.files(path = currentdir, pattern = "\\.html$")
    
    # Read and parse raw HTML file
    html.raw <- XML::htmlTreeParse(htmlfile, useInternalNodes = TRUE)
    
    # Static text to start an HTML document
    html.head.start <- '<!DOCTYPE html> <html xmlns="http://www.w3.org/1999/xhtml">'
    
    # Get the head content to use later on
    html.head.content <- XML::xpathApply(html.raw, "//head", XML::saveXML)
    
    # Static text to end a head TAG
    html.head.end <- '</head>';
    
    # Static text to start a body TAG
    html.body.start <- '<body>'
    
    # Get the header of the page as a Node list
    html.body.header <- XML::xpathApply(html.raw, "//div[@id[starts-with(., 'shinyheader')]]", XML::saveXML)
    
    # Find all the DIV tags within the html structure which has shinytab* ID
    html.parse.shinytabs = XML::getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]") # xmlValue or saveXML
    
    # Find the footer of the page to be used later on
    html.body.footer <- XML::xpathApply(html.raw, "//div[@id[starts-with(., 'myfooter')]]", XML::saveXML)
    
    # Find the scripts of the page to be used later on
    html.body.script <- XML::xpathApply(html.raw, "//script", XML::xmlValue) #there is some weird CDATA field coming along if I use saveXML. not sure how to get rid, so just extract inside and slap script tag on again later
    
    # Find the script in the footer
    html.footer.script <- XML::xpathApply(html.raw, "//script", XML::xmlValue) 

    # Static text to end a Body TAG
    html.body.end <- '</body>'
    
    # Static text to end an HTML TAG
    html.html.end <- '</html>'
    
    script.start.tag <- '<script>'
    script.end.tag <- '</script>'
    
    
    #list with content for tabs
    tablist = list(NULL)
    
    # Now loop over the shinytab sections and save them as a list
    for (i in seq(length(html.parse.shinytabs)))
    {
      
      subDoc <- XML::xmlDoc(html.parse.shinytabs[[i]])
      
      # Set the Title of the Shiny Tab, which will be the file name
      title <- paste('shinytab', i, sep='')
    
      # Get the content of the shinytab
      content <- XML::xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", XML::saveXML)

      tablist[[i]] = shiny::HTML(content[[1]])
      #browser()
    }
    
    return(tablist)
}

        #txtcontent = readLines(tabfilename) #read in as text to pull out title
        #mypattern = '<h2>([^<]*)</h2>' #next 3 lines pull out title of tab inside h2 tag
        #matches = grep(mypattern, txtcontent, value=TRUE)
        #tabtitle = gsub(mypattern,'\\1',matches)
        #tabtitle2 = gsub("&#13;", "", tabtitle) #this weird character remains for some reason, need to remove by hand
        #tablist[[n-2]] = shiny::tabPanel(tabtitle2, htmlcontent, icon = NULL) #save title and content as shiny tabPanel
#    }
