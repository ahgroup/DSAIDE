nmax = 24

x = data.frame(appid = rep(0,nmax), apptitle = "", shortitle = "", simfunction = "", nplots = 0, modeltype = "", otherinputs = "", docname = "")


#get path to Rmd files containing documentation
basepath = here::here()
files = list.files(path = paste0(basepath, "/inst/appinformation/oldfiles/"),  pattern = "\\_settings.R$", full.names = TRUE)
filesrmd = list.files(path = paste0(basepath, "/inst/appinformation/"),  pattern = "\\.Rmd$", full.names = FALSE)
filesrmdfull = list.files(path = paste0(basepath, "/inst/appinformation/"),  pattern = "\\.Rmd$", full.names = TRUE)


for (nn in 1:nmax)
{
  print(nn)
  source(files[nn])
  x[nn,1] = appsettings$appid
  x[nn,2] = appsettings$apptitle
  x[nn,3] = paste0(appsettings$appid,"_",strsplit(filesrmd[nn],"_")[[1]][1])
  x[nn,4] = paste0(appsettings$simfunction,collapse =", ")
  x[nn,5] = appsettings$nplots
  if (!is.null(appsettings$modeltype))
  {
    x[nn,6] = appsettings$modeltype
  }
  x[nn,7] = ""
  x[nn,8] = paste0(appsettings$appid,"_",filesrmd[nn])

  file.copy(filesrmdfull[nn], paste0(basepath, "/inst/appinformation/",appsettings$appid,"_",filesrmd[nn]), overwrite = TRUE)

}

write.table(x, here::here("modeltable2.tsv"), append = FALSE, sep = "\t", row.names = F, col.names = TRUE)
