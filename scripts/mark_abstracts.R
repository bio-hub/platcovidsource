### ### ### ### ### ### ### ### ### ##
###### Bold the Context Sentence #####
### ### ### ### ### ### ### ### ### ##
abs_bold = db[,c("pmid","abstract")]
  for(i in 1:nrow(context$bold$g1)){
    a = context$bold$g1[i,1]
    b = db[which(db$pmid==a),"abstract"]
    c = context$bold$g1[i,2]
    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
  }
  
  for(i in 1:nrow(context$bold$g2)){
    a = context$bold$g2[i,1]
    b = db[which(db$pmid==a),"abstract"]
    c = context$bold$g2[i,2]
    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
  }
  
  for(i in 1:nrow(context$bold$g3)){
    a = context$bold$g3[i,1]
    b = db[which(db$pmid==a),"abstract"]
    c = context$bold$g3[i,2]
    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
  }
  
  
  for(i in 1:nrow(context$bold$g4)){
    a = context$bold$g4[i,1]
    b = db[which(db$pmid==a),"abstract"]
    c = context$bold$g4[i,2]
    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
  }
  
  
#  for(i in 1:nrow(context$bold$g4)){
#    a = context$bold$g4[i,1]
#    b = db[which(db$pmid==a),"abstract"]
#    c = context$bold$g4[i,2]
#    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
#  }
  
  
  
  for(i in 1:nrow(context$bold$g5)){
    a = context$bold$g5[i,1]
    b = db[which(db$pmid==a),"abstract"]
    c = context$bold$g5[i,2]
    abs_bold = apply(abs_bold, 2 ,function(x) sub(pattern = c, replacement = paste0("**",c,"**"),x = x ,fixed = T))
  }


abs_bold = as.data.frame(abs_bold)
abs_bold$pmid = as.character(abs_bold$pmid)
abs_bold$abstract = as.character(abs_bold$abstract)
colnames(abs_bold)=c("pmid","abstract_bold")

