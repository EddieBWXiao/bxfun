wd.here<-function(){
  #stemmed from the MATLAB habits... 
  #in MATLAB, when a script is run, the directory is naturally its own directory, unless otherwise specified
  #putting wd.here() at the top of the script should achieve the same function
  #unlike here::here(), does not depend on the script being in an RProj, just that the person is using RStudio
  
  CurrentSourceWD<-dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(CurrentSourceWD)
  return(CurrentSourceWD)
}