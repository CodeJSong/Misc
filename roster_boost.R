# Roster Boost  :  Add more information to PENN STATE ANGEL roster using crawling PSU SEARCH in R
#install.packages("XML")
library(XML)

###################################
# 1. Export the roster from ANGEL and load it in R
###################################
roster <- read.delim("~/Downloads/roster.tsv")
# reomve Instructor / Teaching Assistant // depends on ANGEL setting
roster <- roster[-which(roster$USER_ROLE!=""),]
###################################



###################################
# 2. Collect more infromation from Penn State Search for all students
###################################
idlist <- roster$SOURCE_ID
n <- length(idlist)
student.info <- NULL
for(i in 1:n){
  url_name <- paste("http://www.psu.edu/cgi-bin/ldap/ldap_query.cgi?uid=",idlist[i], sep='')
  student.info[[i]] <- readHTMLTable(url_name)[[1]]
}

####################################
# 3. Add the information to the roster
####################################
title.list <- levels(unlist(lapply(student.info, function(x)x[,1])))

##########
# Remove the information that you already have 
# I didn't remove the name just to check if it works correclty
donotneed <- c("E-mail:", "Mail ID:")  
titles <- title.list[-which(title.list %in% donotneed)]  
# If you want to add just the department information / GRAD or UNDERGRAD
titles <- c("Curriculum:", "Title:")
##########

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

write.csv(cbind.data.frame(roster, newdata.frame), "~/Downloads/roster2.csv")



########### Remove unncessary info and trim 
# sometimes, I want to know whether they register the course as STAT 414 or MATH 414  (COURSE must be merged)
newdata.frame$Source_course <- unlist(lapply(roster$SOURCE_COURSE, function(x) if(grepl("STAT",x)){ return("STAT") }else return("MATH")))

new.roster <- data.frame(cbind.data.frame(roster[,1:4],  newdata.frame))
write.csv(new.roster, "~/Downloads/roster3.csv")

table(new.roster$Source)
table(new.roster$Cur)
