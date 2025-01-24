cd.source<-function(path,pattern="\\.R$"){
  # source all 
  CurrentSourceWD<-getwd()
  setwd(path)
  files.sources = list.files(pattern = pattern)
  sapply(files.sources, source)
  setwd(CurrentSourceWD)
}