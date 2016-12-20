
Things that need doing:


Now:
1. Finish vignette, check manual/documentation, improve as needed. 

Starting January:
2. Have John Rossow go through the whole package, try it and fix/flag things
    * #1 Test vignette
    * #2 Go through each app. Read all tabs, do all tasks. Record any bugs in code, any errors/unclear things in built-in/app documentation
    * Check R documentation for all functions, flag any errors/unclear things

3. Post package to CRAN, publicize a bit
4. Write DSAIDE paper

5. Write and publish DSAIDE book. Use bookdown, place on leanpub. Then decide if going with a real publisher makes sense.



Add following content/features to package:
* One more app/simulation
* add progress bar
* fix figure size adjustment 


- Remove progress bar. Instead, once a user hits 'run simulation' a 'simulation running' message should show up below the button. This message should go away as soon as the simulation has finished and output has been displayed. Depending on how many lines of code this is, maybe turn it into a stand-alone function. But only do that if it doesn't make the code more complex.

Note: I found a good source of shiny examples and tips and tricks that might be helpful. 
E.g. the ""Busy..." / "Done!" / "Error" example on that website might be useful for the progress bar task below.
https://github.com/daattali

- Modify the figure size adjustment functionality such that the width and height can both be adjusted and connect that to the plotOutput() setting in the new version of the UI   




