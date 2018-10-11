#########################################################################
#Project: Coursera R-Programming Week 2 Assignement
#Author: Nicolas Preuss
#date: 11-Oct-2018
#########################################################################

library(dplyr)
library(tidyr)

pollutantmean <-function(directory,pollutant,id=1:332){
  
  my_path <-'C:/Users/npreuss/Data Science/R Assignements/Week 2/'  
  my_path <- paste(my_path,
                   directory,
                   sep="")
  all_data<-data_frame()
  
  
        for (i in id){
    
               if(i<10){
                      
                      the_file<-paste(paste("00",i,sep=""),
                                      "csv",
                                      sep=".")
               } else if(i>=10 & i<100){
                      
                      the_file<-paste(paste("0",i,sep=""),
                                      "csv",
                                       sep=".")
               } else{
                      
                 
                      the_file<-paste(i,
                                      "csv",
                                       sep=".")
               }
               
                my_df <-read.csv(paste(my_path,
                                 the_file,
                                 sep="/")
                        )
  
              all_data<-rbind(all_data,my_df)
        
    
        }
  
        value_pollutant <- all_data%>%
                           select(one_of(pollutant))
        
        mean(value_pollutant[[1]],na.rm = T)
}




complete <- function(directory,id=1:332){
  my_path <-'C:/Users/npreuss/Data Science/R Assignements/Week 2/'  
  my_path <- paste(my_path,
                   directory,
                   sep="")
  all_data<-data_frame()
  
  
  for (i in id){
    
    if(i<10){
      
      the_file<-paste(paste("00",i,sep=""),
                      "csv",
                      sep=".")
    } else if(i>=10 & i<100){
      
      the_file<-paste(paste("0",i,sep=""),
                      "csv",
                      sep=".")
    } else{
      
      
      the_file<-paste(i,
                      "csv",
                      sep=".")
    }
    
    my_df <-read.csv(paste(my_path,
                           the_file,
                           sep="/")
    )
    
    all_data<-rbind(all_data,my_df)
    
    
  }
  
  all_data_complete<-all_data%>%
                     group_by(ID)%>%
                     dplyr::summarise(nobs=sum(!is.na(sulfate) & !is.na(nitrate)))
  return(all_data_complete)
}


corr<-function(directory,threshold=0){
  
  
  my_path <-'C:/Users/npreuss/Data Science/R Assignements/Week 2/'  
  my_path <- paste(my_path,
                   directory,
                   sep="")
  
  complete_data<-complete(directory)
  
  id_above_threshold <- complete_data$ID[which(complete_data$nobs>threshold)]
  if(length(id_above_threshold)==0){
    return(numeric(0))
  }
  cor_vector<-numeric(length(id_above_threshold))
  for (j in 1:length(id_above_threshold)){
    
    i=id_above_threshold[j]
    if(i<10){
      
      the_file<-paste(paste("00",i,sep=""),
                      "csv",
                      sep=".")
    } else if(i>=10 & i<100){
      
      the_file<-paste(paste("0",i,sep=""),
                      "csv",
                      sep=".")
    } else{
      
      
      the_file<-paste(i,
                      "csv",
                      sep=".")
    }
    
    my_df <-read.csv(paste(my_path,
                           the_file,
                           sep="/")
    )
    index_no_na <- which(!is.na(my_df$sulfate) & !is.na(my_df$nitrate) )
    cor_vector[j] <-cor(my_df$sulfate[index_no_na],my_df$nitrate[index_no_na])
  }
  return(cor_vector)
}



