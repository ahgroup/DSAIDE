#' @title A function that downloads simulator code
#'
#' @description This function is called when the
#' user clicks the "Download Code" button in
#' the simulator app. It creates an R file which
#' will run the simulation specified in the
#' app from which the button is clicked, and
#' after running the simulation, returns the
#' plot and text created by generate_plots()
#' and generate_text().
#'
#' @param modelsettings A list with model settings.
#' #' Needs to contain list elements with names and values for all inputs expected 
#' by simulation function. Required are:
#' Name of simulation function in variable modelsettings$simfunction
#' Also needs to contain an element plotscale 
#' to indicate which axis should be on a log scale (x, y or both), 
#' a list element nplots to indicate number of plots that should be produced
#' when calling the generate_plot function with the result, 
#' and a list element modeltype which specifies what kind of model should be run. 
#' Currently one of (_ode_, _discrete_, _stochastic_, _usanalysis_, _modelexploration_, _fit_ ). Stochastic models also need an nreps list entry to indicate numer of repeat simulations.
#' @param modelfunction The name of the simulator function being called. The name
#' must be one of the simulator functions in the DSAIDE package.
#' 
#' @return Creates an R script that runs the simulation specified in the app
#' and returns the text and plots created by the simulation.
#' @export

download_code <- function(modelsettings, modelfunction) {
  # Opening lines
  opening_lines <- paste("datall = NULL",
                         "finaltext = NULL",
                         "library(DSAIDE)",
                         paste0("modeltype <- ",
                                "\"",
                                modelsettings$modeltype,
                                "\""),
                         paste0("plotscale <- ",
                                "\"",
                                modelsettings$plotscale,
                                "\""),
                         paste0("nplots <- ",
                                "\"",
                                modelsettings$nplots,
                                "\""),
                         paste0("plottype <- ",
                                "\"",
                                modelsettings$plottype,
                                "\""),
                         paste0("rngseed <- ",
                                "\"",
                                modelsettings$rngseed,
                                "\""),
                         paste0("nreps <- ",
                                "\"",
                                modelsettings$nreps,
                                "\""),
                         paste0("tmax <- ",
                                "\"",
                                modelsettings$tmax,
                                "\""),
                         paste0("tfinal <- ",
                                "\"",
                                modelsettings$tfinal,
                                "\""),
                         sep = "\n")
  
  # Option if model is ODE
  if (grepl("_ode_", modelsettings$modeltype)) {
    currentmodel <- modelfunction[grep('_ode',modelfunction)] #list of model functions, get the ode function
    currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    args_in_order <- lapply(1:length(currentargs),
                            function(i) paste(names(currentargs[i]), "=",
                                              currentargs[[i]])) %>%
      unlist(.) %>%
      paste(., collapse = ", ")
  
    model_lines <- paste(paste0("simresult <- ",
                              modelfunction, "(", args_in_order, ")"),
                        "simresult <- simresult$ts",
                        "if (grepl('_and_',modeltype))",
                        "{",
                        "colnames(simresult) = paste0(colnames(simresult),'_ode')",
                        "}",
                        "colnames(simresult)[1] = 'xvals'",
                        "rawdat = as.data.frame(simresult)",
                        "dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = 'varnames', times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL",
                        "dat$IDvar = dat$varnames",
                        "dat$nreps = 1",
                        "datall = rbind(datall,dat)",
                        sep = "\n")
  }
  
  # Option if the model is stochastic
  if (grepl('_stochastic_',modelsettings$modeltype)) {
    modelsettings$currentmodel <- 'stochastic'
    currentmodel <- modelfunction[grep('_stochastic',modelfunction)]
    currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    args_in_order <- lapply(1:length(currentargs),
                            function(i) paste(names(currentargs[i]), "=",
                                              currentargs[[i]])) %>%
      unlist(.) %>%
      paste(., collapse = ", ")
    noutbreaks <- 0
    
    model_lines <- paste(
      "for (nn in 1:nreps)",
      "{",
      "if (is.null(tmax) & !is.null(tfinal) ) ",
      "{",
      "tmax = tfinal",
      "}",
      "currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))]",
      paste0("simresult <- ",
             modelfunction, "(", args_in_order, ")"),
      "simresult <- simresult$ts",
      "colnames(simresult)[1] = 'xvals'",
      "rawdat = as.data.frame(simresult)",
      "dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = 'varnames', times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL",
      "dat$IDvar = paste(dat$varnames,nn,sep='')",
      "dat$nreps = nn",
      "datall = rbind(datall, dat)",
      "rngseed = rngseed + 1",
      "S0=head(simresult[,2],1)",
      "Sfinal=tail(simresult[,2],1)",
      "if ( (S0-Sfinal)/S0>0.2 ) {noutbreaks = noutbreaks + 1}",
      "}",
      "finaltext = paste('For stochastic simulation scenarios, values shown are the mean over all simulations.', noutbreaks,' simulations produced an outbreak (susceptible/uninfected dropped by at least 20%)')",
      "}",
    sep = "\n")
  }
  
  # Final plotting stuff
  closing_lines <- paste(
    "listlength = nplots",
    "result = vector('list', listlength)",
    "result[[1]]$maketext = TRUE",
    "result[[1]]$showtext = NULL",
    "result[[1]]$finaltext = paste0('Numbers are rounded to 2 significant digits. ',finaltext)",
    "result[[1]]$dat = datall",
    "if (!is.null(datall))",
    "{",
    "result[[1]]$ymin = 0.1",
    "result[[1]]$ymax = max(datall$yvals)",
    "result[[1]]$xmin = 1e-12",
    "result[[1]]$xmax = max(datall$xvals)",
    "}",
    "result[[1]]$plottype = 'Lineplot'",
    "result[[1]]$xlab = 'Time'",
    "result[[1]]$ylab = 'Numbers'",
    "result[[1]]$legend = 'Compartments'",
    "result[[1]]$xscale = 'identity'",
    "result[[1]]$yscale = 'identity'",
    "if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}",
    "if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}",
    "generate_plots(result)",
    "generate_text(result)",
  sep = "\n")
  
  # Writing to file
  output_text <- paste(opening_lines, model_lines, closing_lines, sep = "\n")
  return(output_text)
  # fileConn <- file(paste0(getwd(), "/output.R"))
  # writeLines(output_text, fileConn)
  # close(fileConn)
}