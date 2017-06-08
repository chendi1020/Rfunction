#source("H:/R setup/ODBC Connection.R")
#source("H:/R setup/OracleDbusing ROracle.R")

#library(sqldf)
#require(RJSONIO)



#cohort pickup
cohortfun <- function(type, num){
        if (type =='GRADUATING_COHORT' | type == 'COHORT' ){
                cohort <- sqlQuery(MSUEDW,  paste0("select distinct ",type , " as cohort, count(distinct PID) as count
                                                         from  OPB_PERS_FALL.PERSISTENCE_V
                                                         where ", type, " is not null and
                                                         student_level='UN' and level_entry_status='FRST' and (ENTRANT_SUMMER_FALL='Y' or substr(ENTRY_TERM_CODE,1,1)='F')
                                                         and GRAD6 is not null
                                                         group by ", type,
                                                         " order by ",type , " desc
                                                         ", sep="" ) )
                cohort$COHORT[1:num]
        }
        else {
                stop("type is not valid- need a character value of COHORT or GRADUATING_COHORT")
        }
        
}

#PAG data pull
pagfun <- function(type,num){
       
        cohortseq <- cohortfun(type,num)
        PAG <- data.frame()
        for (i in cohortseq){
                PAGds <- sqlQuery(MSUEDW, paste0("select distinct Pid, ",type,"  , COLLEGE_FIRST,  COLLEGE_DEGREE, 
                                    MAJOR_FIRST_SEMESTER, MAJOR_NAME_FIRST, MAJOR_DEGREE, MAJOR_NAME_DEGREE
                                 from OPB_PERS_FALL.PERSISTENCE_V
                                 where student_level='UN' and level_entry_status='FRST' and (ENTRANT_SUMMER_FALL='Y' or substr(ENTRY_TERM_CODE,1,1)='F')
                                 and  ",type ,"  = '", i,"'" ,sep="") ) 
               PAG<- rbind(PAG, PAGds)
        }
        PAG
        
}





#migrationJson function 
#type: entercohort/degree cohort
#num : number of most recent cohorts
migrationJson <- function(type, num){
        #for short name of college
        COLLNM <- sqlFetch(SISInfo, 'COLLEGE')
        PAG <- pagfun(type,num)
        PAG <- sqldf("select a.*, b.Short_Name as COLLEGE_FIRST_NAME, c.Short_Name as COLLEGE_DEGREE_NAME
             from PAG a 
             left join COLLNM b 
             on a.COLLEGE_FIRST=b.Coll_Code
             left join COLLNM c 
             on a.COLLEGE_DEGREE=c.Coll_Code")
        if(type=='GRADUATING_COHORT'){
                #build loop around the degree college
                degrcoll <- unique(PAG$COLLEGE_DEGREE)
                
                #loop through each degree college
                for (k in degrcoll){
                        data <- PAG[PAG$COLLEGE_DEGREE==k,]
                        
                        #concate to prevent same character
                        data$COLLEGE_FIRST_NAME <- paste(data$COLLEGE_FIRST, data$COLLEGE_FIRST_NAME, sep = "-")
                        data$MAJOR_NAME_FIRST <- paste(data$MAJOR_FIRST_SEMESTER, data$MAJOR_NAME_FIRST, sep = "-")
                        
                        data$COLLEGE_DEGREE_NAME <- paste(data$COLLEGE_DEGREE, data$COLLEGE_DEGREE_NAME, sep = "-")
                        data$MAJOR_NAME_DEGREE <- paste(data$MAJOR_DEGREE, data$MAJOR_NAME_DEGREE, sep = "-")
                        
                        #aggregation
                        library(dplyr)
                        
                        Agg1 <- data %>% group_by(GRADUATING_COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
                        Agg2 <- data %>% group_by(GRADUATING_COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME, MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg3 <- data %>% group_by(GRADUATING_COHORT,  MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg4 <- data %>% group_by(GRADUATING_COHORT, COLLEGE_FIRST_NAME,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg5 <- data %>% group_by(GRADUATING_COHORT, MAJOR_NAME_FIRST,  COLLEGE_DEGREE_NAME) %>% summarise(count=n())
                        
                        #gather all college and major from 2 time points
                        
                        DS1<- sqldf("select distinct   COLLEGE_DEGREE_NAME as coll,  MAJOR_NAME_DEGREE as mjr
                                    from Agg2
                                    union
                                    select distinct COLLEGE_FIRST_NAME , MAJOR_NAME_FIRST
                                    from Agg2")
                        
                        #for each college, build college following by majors within that college list
                        t<- split(DS1, DS1$coll)
                        listf <- function(x){ c(unique(x$coll),x$mjr)}
                        test <- sapply(t, listf)
                        listall <- as.data.frame(do.call(c,test))
                        
                        
                        colnames(listall) <- "Org"
                        listall$merge <-1
                        
                        #main structure  for building the square matrix
                        DS2 <- sqldf("select a.org, b.org as org1
                                     from listall a, listall b
                                     on 1=1
                                     ")
                        
                        
                        lvl <- unique(DS2$org1)
                        
                        levels(DS2$Org) <- lvl
                        #covert org1 from char to factor
                        DS2$org1 <- as.factor(DS2$org1)
                        levels(DS2$org1) <- lvl
                        
                        
                        
                        names(Agg3)<-names(Agg1)
                        names(Agg4) <- names(Agg1)
                        names(Agg5) <- names(Agg1)
                        mainds <- rbind(Agg1, Agg3, Agg4, Agg5)
                        
                        
                        cohortseq <- unique(mainds$GRADUATING_COHORT)
                        
                        
                        #for all choices, all five graduating cohort together
                        DS2_2011_15 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                                             from DS2 a
                                             left join mainds b
                                             on a.org=COLLEGE_DEGREE_NAME and a.org1=COLLEGE_FIRST_NAME  ")
                        xtb_2011_15 <- xtabs(count ~ Org + org1,data=DS2_2011_15)
                        
                        
                        
                        #all five graduating cohorts from 2011-12 to 2015-16
                        
                        vec_2011_15<-vector()
                        for(i in  seq(nrow(xtb_2011_15))) {
                                x<-c(xtb_2011_15[nrow(xtb_2011_15)+1-i,])
                                names(x)<-NULL
                                vec_2011_15<-rbind(x,vec_2011_15)
                                
                        }
                        
                        
                        #loop through each graduating cohort
                        vec <- list("All"=vec_2011_15)
                        
                        for (j in cohortseq){
                                mds_2009 <- mainds%>% filter(GRADUATING_COHORT==j)
                                
                                DS2_2009 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                                                  from DS2 a
                                                  left join mds_2009 b
                                                  on  a.org=COLLEGE_DEGREE_NAME and a.org1=COLLEGE_FIRST_NAME  ")
                                
                                xtb_2009 <- xtabs(count ~ Org + org1,data=DS2_2009)
                                vec_2009<-vector()
                                for(i in  seq(nrow(xtb_2009))) {
                                        x<-c(xtb_2009[nrow(xtb_2009)+1-i,])
                                        names(x)<-NULL
                                        vec_2009<-rbind(x,vec_2009)
                                        
                                        curlist <- list(vec_2009)
                                        names(curlist) <- paste(substr(j,1,4), substr(j,8,9), sep = "-")
                                }
                                
                                vec <- c(vec,curlist)
                                
                        }
                        
                        
                        
                        #region
                        coll <- unique(DS1$coll)
                        
                        
                        regionnum <- sapply(coll, function(x){ which(lvl== x)-1})
                        names(regionnum) <- NULL
                        
                        
                        names <- lvl
                        
                        
                        
                        list <- list("names"=names, "regions"=regionnum, "matrix"=vec)
                        
                       
                        
                        
                        jsonOut<-toJSON(list)
                        #cat(jsonOut)
                        
                        sink(paste('data',k, '.json', collapse ='', sep=""))
                        cat(jsonOut)
                        
                        sink()
                        
                   
                }
        }
        else if (type=='COHORT'){
                firstcoll <- unique(PAG$COLLEGE_FIRST)
                
                
                for (k in firstcoll){
                        
                        data <- PAG[PAG$COLLEGE_FIRST==k,]
                        
                        #recode those who have not graduated yet
                        data$COLLEGE_DEGREE <- ifelse(is.na(data$COLLEGE_DEGREE),99, data$COLLEGE_DEGREE)
                        data$COLLEGE_DEGREE_NAME <- as.character(data$COLLEGE_DEGREE_NAME)
                        data$COLLEGE_DEGREE_NAME <- ifelse(is.na(data$COLLEGE_DEGREE_NAME),'Not Graduate', data$COLLEGE_DEGREE_NAME)
                        data$MAJOR_DEGREE <- ifelse(is.na(data$MAJOR_DEGREE), 9999, data$MAJOR_DEGREE)
                        
                        data$MAJOR_NAME_DEGREE <- as.character(data$MAJOR_NAME_DEGREE)
                        data$MAJOR_NAME_DEGREE <- ifelse(is.na(data$MAJOR_NAME_DEGREE), 'Not Graduate', data$MAJOR_NAME_DEGREE)
                        
                        #concate to prevent same character
                        data$COLLEGE_FIRST_NAME <- paste(data$COLLEGE_FIRST, data$COLLEGE_FIRST_NAME, sep = "-")
                        data$MAJOR_NAME_FIRST <- paste(data$MAJOR_FIRST_SEMESTER, data$MAJOR_NAME_FIRST, sep = "-")
                        
                        data$COLLEGE_DEGREE_NAME <- paste(data$COLLEGE_DEGREE, data$COLLEGE_DEGREE_NAME, sep = "-")
                        data$MAJOR_NAME_DEGREE <- paste(data$MAJOR_DEGREE, data$MAJOR_NAME_DEGREE, sep = "-")
                        
                        
                        
                        #aggregation
                        library(dplyr)
                        
                        Agg1 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
                        Agg2 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME, MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg3 <- data %>% group_by(COHORT,  MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg4 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
                        Agg5 <- data %>% group_by(COHORT, MAJOR_NAME_FIRST,  COLLEGE_DEGREE_NAME) %>% summarise(count=n())
                        
                        DS1<- sqldf("select distinct COLLEGE_FIRST_NAME as coll, MAJOR_NAME_FIRST as mjr
                                    from Agg2
                                    union
                                    select distinct COLLEGE_DEGREE_NAME ,MAJOR_NAME_DEGREE
                                    from Agg2")
                        
                        t<- split(DS1, DS1$coll)
                        listf <- function(x){ c(unique(x$coll),x$mjr)}
                        test <- sapply(t, listf)
                        listall <- as.data.frame(do.call(c,test))
                        #listall<-as.data.frame(listall[!duplicated(listall), ])
                        colnames(listall) <- "Org"
                        listall$merge <-1
                        
                        #main structure
                        DS2 <- sqldf("select a.org, b.org as org1
                                     from listall a, listall b
                                     on 1=1
                                     ")
                        
                        
                        lvl <- unique(DS2$org1)
                        
                        
                        levels(DS2$Org) <- lvl
                        DS2$org1 <- as.factor(DS2$org1)
                        levels(DS2$org1) <- lvl
                        
                        
                        
                        names(Agg3)<-names(Agg1)
                        names(Agg4) <- names(Agg1)
                        names(Agg5) <- names(Agg1)
                        mainds <- rbind(Agg1, Agg3, Agg4, Agg5)
                        
                        
                        cohortseq <- unique(mainds$COHORT)
                        
                        #2005-2009
                        DS2_2005_09 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                                             from DS2 a
                                             left join mainds b
                                             on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
                        xtb_2005_09 <- xtabs(count ~ Org + org1,data=DS2_2005_09)
                        
                        
                        
                        
                        
                        #2005 to 2009 new
                        
                        vec_2005_09<-vector()
                        for(i in  seq(nrow(xtb_2005_09))) {
                                x<-c(xtb_2005_09[nrow(xtb_2005_09)+1-i,])
                                names(x)<-NULL
                                vec_2005_09<-rbind(x,vec_2005_09)
                                
                        }
                        
                        vec <- list("All"=vec_2005_09)
                        
                        for (j in cohortseq){
                                mds_2009 <- mainds%>% filter(COHORT==j)
                                
                                DS2_2009 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                                                  from DS2 a
                                                  left join mds_2009 b
                                                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
                                xtb_2009 <- xtabs(count ~ Org + org1,data=DS2_2009)
                                vec_2009<-vector()
                                for(i in  seq(nrow(xtb_2009))) {
                                        x<-c(xtb_2009[nrow(xtb_2009)+1-i,])
                                        names(x)<-NULL
                                        vec_2009<-rbind(x,vec_2009)
                                        #names(vec_2009) <- j
                                        #veccur <-cbind(j,  vec_2009)
                                        #curlist<-as.list(vec_2009)
                                        curlist <- list(vec_2009)
                                        names(curlist) <- j
                                }
                                #list<- c(list,j=curlist)
                                vec <- c(vec,curlist)
                                #vec <- rbind(vec,veccur)
                                
                                
                        }
                        
                        
                        
                        
                        
                        
                        #region
                        coll <- unique(DS1$coll)
                        
                        
                        regionnum <- sapply(coll, function(x){ which(lvl== x)-1})
                        names(regionnum) <- NULL
                        
                        
                        names <- lvl
                        
                        
                        
                        list <- list("names"=names, "regions"=regionnum, "matrix"=vec)
                        
                       
                        jsonOut<-toJSON(list)
                     
                        sink(paste('data',k, '.json', collapse ='', sep=""))
                        cat(jsonOut)
                        
                        sink()
                }
                
        }
        else{
                stop('wrong type name')
        }
}