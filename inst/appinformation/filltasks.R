

# This function fills in what to record
# Prevents records being stored in the wrong index

filltasks <- function(tid,rc,nrec,rectext,recnote,recfuzzy,allrecord)
{
  for (n in 1:nrec)
  {
    allrecord[rc,"TaskID"] = tid
    allrecord[rc,"RecordID"] = n
    allrecord[rc,"Record"] = rectext[n]
    allrecord[rc,"Note"] = recnote[n]
    allrecord[rc,"Fuzzy"] = recfuzzy[n]
    rc = rc+1
  }
  return(allrecord)
}
