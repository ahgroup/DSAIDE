

# This function fills in what to record
# Prevents records being stored in the wrong index

fill_tasktable <- function(tablist,alltasks,appsettings,allrecords,tid,rc,nrec,reclist)
{
  for (n in 1:nrec)
  {
    tablist[[1]][rc,"QuizID"]= paste0("dsaide_",appsettings$appid)
    tablist[[1]][rc,"AppTitle"] = appsettings$apptitle
    tablist[[1]][rc,"AppID"] = appsettings$appid
    tablist[[1]][rc,"TaskID"] = tid
    tablist[[1]][rc,"TaskText"] = alltasks[tid,"TaskText"]

    tablist[[2]][rc,"TaskID"] = tid
    tablist[[2]][rc,"RecordID"] = n
    tablist[[2]][rc,"Record"] = reclist$rectext[n]
    tablist[[2]][rc,"Type"] = reclist$rectype[n]
    tablist[[2]][rc,"Note"] = reclist$recnote[n]
    tablist[[2]][rc,"Fuzzy"] = reclist$recfuzzy[n]
    rc = rc+1
  }
  return(tablist)
}
