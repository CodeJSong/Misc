##########################################
# Enrich Roster   ( Jun Song, 2016/05/06)
##########################################
# Add more information to PENN STATE ANGEL roster using crawling PSU SEARCH in R
# XML package required

###################################
# enrich.roster(rfile, wfile, rminstructer=TRUE, all=FALSE, xlist=TRUE)
###################################
### input
# rfile : roster file name exported from ANGEL  
# wfile : file name for new roster 
# rminstructer=TRUE : remove instructer/TA's name on the list  (depending on ANGEL setting)
# all=TRUE : crawl all the available data including address/nick name/phone number 
# xlist=TRUE : how students registered the course as STAT XXX or MATH XXX
### output 
# save the new roster 
# dataframe of the new roster
###################################

enrich.roster <- function(rfile, wfile, rminstructer=TRUE, all=FALSE, xlist=TRUE){
  require(XML)
  ##########
  # Export the roster from ANGEL and load it in R
  ##########
  roster <- read.delim(rfile)
  # reomve Instructor / Teaching Assistant // depends on ANGEL setting
  if(rminstructer) roster <- roster[-which(roster$USER_ROLE!=""),]
  
  ##########
  # Collect more infromation from Penn State Search for all students
  ##########
  idlist <- roster$SOURCE_ID
  n <- length(idlist)
  student.info <- NULL
  for(i in 1:n){
    url_name <- paste("http://www.psu.edu/cgi-bin/ldap/ldap_query.cgi?uid=",idlist[i], sep='')
    student.info[[i]] <- readHTMLTable(url_name)[[1]]
  }
  
  
  ##########
  # Add the information to the roster
  ##########
  title.list <- levels(unlist(lapply(student.info, function(x)x[,1])))
  ##########
  # Remove the information that you already have 
  # I didn't remove the name just to check if it works correclty
  donotneed <- c("E-mail:", "Mail ID:")  
  titles <- title.list[-which(title.list %in% donotneed)]  
  if(!all) titles <- c("Curriculum:", "Title:") # If you want to add just the department information / GRAD or UNDERGRAD
  
  p <- length(titles)
  newdata <- NULL
  for(i in 1:n){
    tmp<-student.info[[i]]
    rowinfo <- rep("NA",p)
    title.exist <- match(tmp[,1] , titles) 
    rowinfo[title.exist[!is.na(title.exist)]] <- as.vector(tmp[which(tmp[,1] %in% titles),2])  
    newdata <- rbind(newdata, rowinfo)
  }
  rownames(newdata) <- NULL
  newdata.frame <- data.frame(newdata) ; names(newdata.frame) <- titles
  
  ############
  # xlist : Check whether a student registered it as STAT 414 or MATH 414
  ############
  if(xlist){
    newdata.frame$Source_course <- unlist(lapply(roster$SOURCE_COURSE, function(x) if(grepl("STAT",x)){ return("STAT") }else return("MATH")))  
  }
  
  if(all){
    new.roster <- cbind.data.frame(roster, newdata.frame)
    write.csv(new.roster, wfile)  
  }
  if(!all){
    new.roster <- data.frame(cbind.data.frame(roster[,1:4],  newdata.frame))
    write.csv(new.roster, wfile)
  }
  return(new.roster)
}


################################################
# Example
################################################

rfile <- "d:/Downloads/roster.tsv"
wfile <- "d:/Downloads/roster2.csv"

new.roster <- enrich.roster(rfile=rfile, wfile=wfile)  # load data as well as save csv file

table(new.roster$Source)
table(new.roster$Cur)

