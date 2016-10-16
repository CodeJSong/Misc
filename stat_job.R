################################################################
# stat.job   ( Jun Song, 2016/09/16)
################################################################
# collect stat phd job posting 
#    - from UFL-STAT job list(package : XML)   - 2016/05/17
#    - from mathjobs.org (pacakge : rvest, stringr) - 2016/09/16
#    - purdue : http://www.stat.purdue.edu/resources/jobs/listings/jobs/
#    - UW : http://www.stat.washington.edu/jobs/
#    - amstat : http://jobs.amstat.org/jobs
#    - IMS : http://jobs.imstat.org/jobseeker/search/results/
################################################################
stat.job <- function(where="ufl", maxpage=7, fromtoday=20){
  # collect data from "where" upto page no."maxpage" until the post "fromtoday" days ago
  today <- Sys.Date()
  crit.date <- today - fromtoday
  output <-NULL
  if(where=="ufl") {
    require(XML)
    job.url <- "http://www.stat.ufl.edu/jobs/?page="
    for(i in 1:maxpage){
      tmp <- readHTMLTable(paste(job.url,i,sep=''))
      ntable <- length(tmp)
      
      dd<-lapply(tmp,function(x) x[length(x)])[[ntable]]
      first.date <- as.Date(unlist(dd)[length(unlist(dd))], format="%m/%d/%Y")    # the earlist date of the page
      
      output <- rbind.data.frame(output,cbind.data.frame(tmp[[1]],Page=i))
      
      if(ntable>1){
        for(j in 1:ntable) output <- rbind.data.frame(output, cbind.data.frame(tmp[[j]],Page=i))
      }
      
      cat('Page',i,'is loaded\n')    
      
      if(as.Date(unlist(dd)[length(unlist(dd))-1], format="%m/%d/%Y") < crit.date) {
        cat('The post is outdated after page', i, '\n')
        break  # stop for-loop if the post is too old
      }
    }
  }
  if(where=="mathjobs"){
    require(rvest)
    require(stringr)
    
    aa<-read_html("https://www.mathjobs.org/jobs?action=joblist&id=statistics&send=Go&.cgifields=all")
    jobs <- html_nodes(aa,css="dt") %>% html_text()
    n.position <- str_count(jobs,"\\[")
    
    joblist <- NULL  # the final result
    
    for(i in 1:length(jobs)){
      tmp <- jobs[i]
      univ.name <- substr(tmp, 1,gregexpr("\\[",tmp)[[1]][1]-1 )
      dept.name <- "na"
      if(str_count(univ.name,",")>0) {
        dept.name <- substr(univ.name, 1, gregexpr(",",tmp)[[1]][1]-1 )
        univ.name <- substr(univ.name, gregexpr(",",tmp)[[1]][1]+2, nchar(univ.name) )
      }
      
      for(j in 1:(n.position[i])){
        joblist$dept <- c(joblist$dept,dept.name)
        joblist$univ <- c(joblist$univ,univ.name)
        if(j < n.position[i]){
          pos.tmp <- substr(tmp, gregexpr("\\[",tmp)[[1]][j], gregexpr("\\[",tmp)[[1]][j+1]-1)
          deadline <- "na"
          if(grepl("\\(2",pos.tmp)){
            deadline <- substr(pos.tmp,gregexpr("\\(2",pos.tmp)[[1]][1]+1, gregexpr("\\(2",pos.tmp)[[1]][1]+10)
            pos.tmp <-  substr(pos.tmp,1, gregexpr("\\(2",pos.tmp)[[1]][1]-2)
          }
          joblist$position <- c(joblist$position, pos.tmp)
          joblist$deadline <- c(joblist$deadline, deadline)
        }
        if(j == n.position[i]) {
          pos.tmp <- substr(tmp, gregexpr("\\[",tmp)[[1]][j], nchar(tmp))
          deadline <- "na"
          if(grepl("\\(2",pos.tmp)){
            deadline <- substr(pos.tmp,gregexpr("\\(2",pos.tmp)[[1]][1]+1, gregexpr("\\(2",pos.tmp)[[1]][1]+10)
            pos.tmp <-  substr(pos.tmp,1, gregexpr("\\(2",pos.tmp)[[1]][1]-2)
          }
          joblist$position <- c(joblist$position, pos.tmp)
          joblist$deadline <- c(joblist$deadline, deadline)
        }
        #joblist$deadline <- c(joblist$deadline, substr(tmp, gregexpr("\\(2",tmp)[[1]][j],gregexpr("\\)",tmp)[[1]][j]))
      }
    }
    output <- (data.frame(joblist))
  }
  return(output)
}

######################################################
# MATHJOBS.ORG
######################################################
job.list.data <- stat.job(where="mathjobs")

write.csv(job.list.data, "jobs.csv")


#####################################################
# UFL Job
#####################################################
aa<- stat.job(where="ufl", maxpage=10, fromtoday=60)

aa[grepl("rofessor", aa$Position),]      ## Professor list
aa[grepl("aulty", aa$Position),]      ## Professor list
aa[grepl("Post", aa$Position),]      ## Postdoctral list

write.csv(aa[grepl("rofessor", aa$Position),], "Professor_job.csv")
write.csv(aa[grepl("Post", aa$Position),] "PostDoc_job.csv")
