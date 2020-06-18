#' @title A helper function that produces a call to a simulator function for specific settings
#'
#' @description This function takes a modelsettings structure and uses that information
#' to create an unevaluated function call that runs the simulator function with the specified settings
#'
#'
#' @param modelsettings a list with model settings. Required list elements are: \cr
#' List elements with names and values for all inputs expected by simulation function. \cr
#' modelsettings$simfunction - name of simulation function in variable  \cr
#' modelsettings$is_mbmodel - indicate of simulation function has mbmodel structure
#' @return A string containing an unevaluated function call with the specified settings
#' @details This function produces a function call for specific settings.
#' @export

generate_fctcall <- function(modelsettings)
{
    #extract modeslettings inputs needed for simulator function
    currentmodel = modelsettings$currentmodel
    #match values provided from UI with those expected by function
    settingsvec = unlist(modelsettings)

    if (modelsettings$is_mbmodel)
    {
      iplist = formals(currentmodel) #get all input arguments for function
      #turn pair-list into a vector of inputs
      ip = c(eval(iplist$vars),eval(iplist$pars),eval(iplist$times))
      #turn each input into a shiny numeric input
    } else #if it's not an mbmodel generated function
    {
      ip = unlist(formals(currentmodel)) #get all input arguments for function
    }


    currentargs = settingsvec[match(names(ip), names(settingsvec))]
    #get rid of NA that might happen because inputs are not supplied for certain function inputs.
    #in that case we use the function defaults
    currentargs <- currentargs[!is.na(currentargs)]
    #make a list, makes conversion to numeric easier
    arglist = as.list(currentargs)
    #convert arguments for function call to numeric if possible
    #preserve those that can't be converted
    numind = suppressWarnings(!is.na(as.numeric(arglist))) #find numeric values
    arglist[numind] = as.numeric(currentargs[numind])

    #mbmodels have a non-flat structure of arguments, not sure how to better re-create this for function call
    if (modelsettings$is_mbmodel)
    {
      varargs = settingsvec[match(names(iplist$vars[-1]), names(settingsvec))] #1st element is always empty for some reason
      parargs = ""
      timeargs = ""
      argstring = paste0("vars = c(",varargs,"), pars = c(",parargs,"), times = c(",timeargs,")")
      fctlist = append(parse(text = currentmodel), argstring)

    } else
    {
      #add function name as first element to list
      fctlist = append(parse(text = currentmodel), arglist)
    }

    browser()

    #make fct call
    fctcall <- as.call(fctlist)
    return(fctcall)
}


