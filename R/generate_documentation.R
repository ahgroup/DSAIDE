#' @title A helper function for the UI part of the shiny apps
#'
#' @description This function take the documentation provided as html file
#' it then takes the html file and extracts sections 
#' to generate the tabs with content for each Shiny app. 
#' This is a helper function and only useful for this package.
#' @return tablist a list of tabs for display in a shiny UI
#' @details This function is called by the shiny UIs to populate the documentation and information tabs
#' @author Andreas Handel
#' @export

generate_documentation <- function()
{
    #it is better if a developer creates the html by knitting
    #their Rmd file instead of producing it on the fly
    #therefore this code bit is currently disabled,
    #but left in here for future reference
    #first, check if no html file of documentation is available
    #or if it is outdated. In that case, use Rmd file to create
    #html file
    #for this to work, there can only be one Rmd and 0 or 1 html files
    #in the folder where the app is
    # htmlfile = list.files(path = currentdir, pattern = "\\.html$")
    # rmdfile = list.files(path = currentdir, pattern = "\\.Rmd$")
    # if (length(htmlfile)==0) #html file doesn't exist, create
    # {
    #   rmarkdown::render(rmdfile)
    # }      
    # if (length(htmlfile)>0) #html file exists, check date
    # {
    #   htmldate <- file.info(htmlfile)$mtime
    #   rmddate <- file.info(rmdfile)$mtime
    #   if ((rmddate-htmldate)>0) #if rmd file is newer, recreate html
    #   {
    #     rmarkdown::render(rmdfile)
    #   }
    # }

    #now take HTML file and split it into components for each tab
    currentdir = getwd() 
    htmlfile = list.files(path = currentdir, pattern = "\\.html$")
    
    htmldoc = read_html(htmlfile)
    #test = rvest::html_nodes(htmldoc, "h2") 
    #titles =  html_text(test)  
    #browser()
    #test = rvest::html_nodes(htmldoc, 'div# :contains(shinytab)')
    html.parse.shinytabs = rvest::html_nodes(htmldoc, xpath = "//div[@id[starts-with(., 'shinytab')]]" )
    htmlcontent = html_text(html.parse.shinytabs)
    # Read and parse raw HTML file
    #html.raw <- XML::htmlTreeParse(htmlfile, useInternalNodes = TRUE)
    
    # Find all the DIV tags within the html structure which has shinytab* ID
    #html.parse.shinytabs = XML::getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]") # xmlValue or saveXML
    #browser()
    #list with content for tabs
    #tablist = NULL
    tabtitles = c('Overview','The Model','What to do','Further Information')    
    tablist = shiny::tabPanel(htmlcontent)
    
    browser()
    # Now loop over the shinytab sections and save them as a list
    #for (i in seq(length(html.parse.shinytabs)))
    #{
      
    #  subDoc <- XML::xmlDoc(html.parse.shinytabs[[i]])
      # Get the content of the shinytab
    #  content <- XML::xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", XML::saveXML)
    #  htmlcontent = shiny::HTML(content[[1]])
      #save title and content as shiny tabPanel
    #  tablist[[i]] = shiny::tabPanel(tabtitles[i], htmlcontent, icon = NULL) 
    #}
    return(tablist)
}

        
