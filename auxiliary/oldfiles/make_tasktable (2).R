#helper function that sets up task tables to be filled
make_tasktable <- function(appsettings)
{
  alltasks = data.frame(QuizID = paste0("dsaide_",appsettings$shorttitle),
                        AppTitle = appsettings$apptitle,
                        AppID = appsettings$appid,
                        TaskID = 1,
                        TaskText = "")

  allrecord = data.frame(TaskID = 1,
                         RecordID = 1,
                         Record = "",
                         Type = "",
                         Note = "",
                         Answer = "")
  tablist = list()
  tablist[[1]] = alltasks
  tablist[[2]] = allrecord
  return(tablist)

}
