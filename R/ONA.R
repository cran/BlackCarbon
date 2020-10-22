

#' @export

ONA<-function(data = data, ATN = "ATN", BC="BC", dATN=0.05){
  y<-NA #the first BC is always missing so
  min<-dATN #keeping the minimum change in ATN as 0.05 as default
  target_ATN<-data[,ATN][1]+min # setting the target ATN
  list<-which(data[, ATN]>target_ATN)
  if (nrow(data)<2){ #when the dataset has only one row
    y<-y
    return(y)
  } # listing all the ATNs greater than target ATN
  else if (length(list)==0){ #for a file having less observations the list is zero
    end<-nrow(data)# for them end is last row
    x<-data[, BC][2:nrow(data)] #just replacing the new value with the old
    y<-c(y,x) #combing with 1st row
  } else{
    end<-min(which(data[, ATN]>target_ATN))#if the length is above 0 the end row is the row having slightly greater ATN than the target
    x<-rep(mean(data[,BC][2:end], na.rm = T), times = length(2:end))#mean is single so repeating it for number of rows in the window
    y<-c(y,x)#combing it with the last result
  }
  while(end<nrow(data)){ #dealing till the last observation
    target_ATN<-target_ATN+min #setting a new target
    list<-which(data[, ATN]>target_ATN)
    if(length(list)>0){#now when the list has values
      start<-end+1 #preceeding window strats as the last one ends
      end<-min(which(data[,ATN]>target_ATN))
      if (start>end){ #sometimes because of the fluctuation in ATN some previous values can have maximum ATN then target so saying it to skip that part
        next
      }
      x<-rep(mean(data[,BC][start:end], na.rm = T), times = length(start:end))
      y<-c(y,x)
    } else { #when the length of list is zero
      start<-end+1
      end<-nrow(data)
      x<-rep(mean(data[,BC][start:end], na.rm = T), times = length(start:nrow(data)))
      y<-c(y,x)
      break #we only want it to be done once if we do not have any ATN value greater than target ATN
    }
  }
  return(y) # returning the result
}


#' @export

filter_loading<-function(data, ONA_BC = "BC_ONA", ATN = "ATN"){
  data[,ONA_BC]*(0.88*exp(-(data[,ATN])/100)  +0.12)^(-1)
}



#' @export

filter_adjust_ONA<-function(data=data, ATN = "ATN", BC = "BC",dATN = 0.05, threshold =5, skip =15, ignore = 10){
  if(nrow(data)< (skip+ignore+2)){
    print("not enough observations to correct for filter loading")
  } else{
    for(j in skip:(nrow(data)-ignore)){
      if ((data[, ATN][j]-data[, ATN][j-1])<(-threshold)){
        f<-j
        k1<-data[1:(f-skip), ]
        k2<-data[(f+ignore):nrow(data), ]
        k1[,"BC_ONA"]<-ONA(data=k1, ATN = "ATN", BC ="BC", dATN =dATN)
        k1[, "BC_ONA_Cor"]<-k1[ ,"BC_ONA"]*(0.88*exp(-(k1[,ATN])/100)  +0.12)^(-1)
        k2[,"BC_ONA"]<-ONA(data=k2, ATN = "ATN", BC ="BC", dATN =dATN)
        k2[,"BC_ONA_Cor"]<-k2[ ,"BC_ONA"]*(0.88*exp(-(k2[,ATN])/100)  +0.12)^(-1)
      }
    }
  }
  if(exists("k1")==T){
    data<-rbind(k1, k2)
    rm(k1, k2)
  } else {
    data[, "BC_ONA"]<-ONA(data, ATN = "ATN", BC ="BC", dATN =dATN)
    data[,"BC_ONA_Cor"]<-data[ ,"BC_ONA"]*(0.88*exp(-(data[,ATN])/100)  +0.12)^(-1)
  }
  return(data)
}




#' @export

ONA_compiled<-function(data=data, ATN = "ATN", BC ="BC", file_id ="file_ID", dATN =0.05, threshold=5, skip =15, ignore=10){
  data[, "file_ID"]<-as.factor(data[, "file_ID"])
  data1<-lapply(split(data, data[,file_id]),function (x){filter_adjust_ONA(x, ATN=ATN, BC=BC, dATN =dATN,threshold =threshold, skip =skip, ignore = ignore)})
  names(data1)<-NULL
  data2<-do.call("rbind", data1)
  return(data2)
}

