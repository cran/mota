mota.progress<-function(value,max.value)
{
erase<-paste(rep("\b",2*nchar(max.value)+16),collapse="")
cat(erase,paste("progress: ",value, "of",max.value,""),sep="")
if(.Platform$OS.type=="windows")
  flush.console()
invisible(NULL)
}