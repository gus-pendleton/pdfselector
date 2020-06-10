#' A PDF Selecting Tool
#'
#' This function reads ALL pdfs from a specified directory and screens them with a RegEx expression. Matching files are copied to a new folder.
#' @param file_folder Directory to folder containing pdfs, as a string
#' @param new_folder Name of new folder containing matching pdfs, as a string
#' @param search_pattern RegEx expression for what you want to search for, string
#' @keywords pdf

 
pdf_selector<-function(file_folder,new_folder,search_pattern){
  library(pdftools)
  library(tidyverse)
  setwd(file_folder)
  dir.create(new_folder)
  filenames<-list.files(file_folder,pattern="*.pdf",full.names=TRUE)
  pdfs<-lapply(filenames,pdf_text)%>%
    lapply(paste,sep=" ",collapse=" ")
  l<-lapply(pdfs,str_which,pattern=search_pattern)%>%
    as.logical()%>%
    replace_na(FALSE)
  sapply(filenames[l],file.copy,to=new_folder)
}