#helper function that sets up task tables to be filled
make_tasktable <- function()
{
  alltasks = data.frame(QuizID = "",
                        AppTitle = "",
                        AppID = "",
                        TaskID = 0,
                        TaskText = "")

  allrecord = data.frame(TaskID = 0,
                         RecordID = 0,
                         Record = "",
                         Type = "",
                         Note = "",
                         Answer = "",
                         Fuzzy = 0)
  tablist = list()
  tablist[[1]] = alltasks
  tablist[[2]] = allrecord
  return(tablist)

}
