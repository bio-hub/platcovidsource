setwd('updates/current/')

df = NULL
for (i in 1:length(my_PM_list)){
  df = rbind(df,article_to_df(my_PM_list[i], max_chars = -1,getKeywords = TRUE, getAuthors = F))
}

df_address = NULL
for (i in 1:length(my_PM_list)){
  df_address = rbind(df_address,article_to_df(my_PM_list[i], max_chars = -1,getKeywords = F, getAuthors = T)[,c(1,11:14)])
  
}

### ### ### ### ### ### ###
##### Article type ######## 
### ### ### ### ### ### ###
time = Sys.time()
article_type = sapply(litcovid$pmid, function(x) entrez_summary(db="pubmed",id=x)$pubtype)
Sys.time() - time
  # article_type1 = article_type
  names(article_type) = litcovid$pmid
  article_type = cbind(names(article_type),unlist(article_type))
  article_type = as.data.frame(article_type)
    colnames(article_type) = c("pmid","type")
    two_type=unique(article_type[which(duplicated(article_type[,1])==TRUE),1])
    one.1 = article_type[-which(article_type[,1] %in% two_type),]
    one.2 = article_type[which(article_type[,1] %in% two_type),]
    df2 = NULL
    for(i in unique(one.2[,1])){
      a = which(one.2[,1]==i)
      df2 = rbind(data.frame(pmid = i, type = paste0(one.2[a,2], collapse = " | ")),df2)
    }
    one.1 = as.data.frame(one.1)
    colnames(one.1) = c("pmid","type")

article_type = rbind(one.1,df2)

### ### ### ### ### ### ###
##### Article Lang ######## 
### ### ### ### ### ### ###

time = Sys.time()
article_lang = sapply(litcovid$pmid, function(x) entrez_summary(db="pubmed",id=x)$lang)
Sys.time() - time
names(article_lang) = litcovid$pmid
article_lang = cbind(names(article_lang),unlist(article_lang))

article_lang = as.data.frame(article_lang)
colnames(article_lang) = c("pmid","lang")
two_type=unique(article_lang[which(duplicated(article_lang[,1])==TRUE),1])
one.1 = article_lang[-which(article_lang[,1] %in% two_type),]
one.2 = article_lang[which(article_lang[,1] %in% two_type),]

df2 = NULL
for(i in unique(one.2[,1])){
  a = which(one.2[,1]==i)
  df2 = rbind(data.frame(pmid = i, lang = paste0(one.2[a,2], collapse = " | ")),df2)
}
one.1 = as.data.frame(one.1)
colnames(one.1) = c("pmid","lang")

article_lang = rbind(one.1,df2)

### ### ### ### ### ### ### ### ##
###### Article Affiliation #######
### ### ### ### ### ### ### ### ##

df.2 =na.omit(unique(df_address[,c("pmid","address")]))
df.2 = apply(df.2, 2, function(x) gsub(pattern = "\\(|\\)|\\.|\\:|\\[|\\]|\\+|\\-|\\'",replacement = " ", x = x))
df.2 = as.data.frame(df.2)

pmid.country=NULL
for(i in c(countr,"USA","UK")){
  i.1 = paste0("\\b",i,"\\b")
  a = df.2[grep(i.1,ignore.case = T,value = F,df.2[,2]),]
  tryCatch({for(l in 1:nrow(a)){
    pmid.country = rbind(pmid.country,data.frame(pmid= a[l,1], country = sapply(a[l,2], function(x) i)))
  }}, error = function(e) {})
}
rm(a,i,i.1,l)

Cyt2Country = data.frame(city = c("London","Oxford",
                                  "Kawasaki","Tokyo",
                                  "California","\\bUS\\b","Philadelphia","Madison","Minnesota","Wichita","Maryland","Harvard University","Florida","Stanford","Oklahoma City","New Jersey","Omaha","Iowa","North Carolina","\\bNY\\b","Newport","Providence","New York","Boston","Washington","Chicago","Seattle",
                                  "México",
                                  "Wuhan","Beijing","Chinese","Huazhong University",
                                  "Toronto","Montreal",
                                  "Madrid","Española","España",
                                  "Milan","Roma",
                                  "Korea","Bangalore","Hong Kong",
                                  "Sidney","Melbourne",
                                  "Brasil","Sao Paulo",
                                  "Berlin",
                                  "Beirut",
                                  "New Delhi"), 
                         country = c(rep("United Kingdom",2),
                                     rep("Japan",2),
                                     rep("United States",23),
                                     "Mexico",
                                     rep("China",4),
                                     rep("Canada",2), 
                                     rep("Spain",3),
                                     rep("Italy",2),
                                     "South Korea",
                                     "India",
                                     "Hong Kong",
                                     rep("Australia",2),
                                     rep("Brazil",2),
                                     "Germany",
                                     "Libanon",
                                     "India"))

a = setdiff(unique(df.2$pmid),unique(pmid.country$pmid))
c = df.2[df.2$pmid %in% a,]
b = NULL
for(i in 1:nrow(Cyt2Country)){
  d = c[grep(pattern = Cyt2Country[i,1], ignore.case = T,value = F, c[,2]),]
  if (nrow(d)>1){
    i.1 = Cyt2Country[i,2]
    for(l in 1:nrow(d)){
      b = rbind(b,data.frame(pmid= d[l,1], country = sapply(d[l,2], function(x) i.1)))
      }
    }
}

missing_affiliation = df.2[df.2$pmid %in% setdiff(unique(c$pmid),unique(b$pmid)),]

pmid.country=rbind(pmid.country,b)
pmid.country = apply(pmid.country, 2, function(x) sub("USA","United States",x))
pmid.country = apply(pmid.country, 2, function(x) sub("UK","United Kingdom",x))
pmid.country = as.data.frame(unique(pmid.country))
rm(a,b,c,d,i,i.1,l,df.2)

#CountryAffiliation =  as.data.frame(table(pmid.country[,2]))
  two_type=unique(pmid.country[which(duplicated(pmid.country[,1])==TRUE),1])
  one.1 = pmid.country[-which(pmid.country[,1] %in% two_type),]
  one.2 = pmid.country[which(pmid.country[,1] %in% two_type),]
  df.2 = NULL
    for(i in unique(one.2[,1])){
      a = which(one.2[,1]==i)
      df.2 = rbind(data.frame(pmid = i, country = paste0(one.2[a,2], collapse = " | ")),df.2)
    }
    one.1 = as.data.frame(one.1)
    colnames(one.1) = c("pmid","country")
article_affiliation = rbind(one.1,df.2)
rm(df.2,one.1,one.2,a,i,two_type,Cyt2Country)


### ### ### ### ### ### ### ###
####### Articel date ########## 
### ### ### ### ### ### ### ###

artical_date = apply(df[,c(1,5:7)],1, function(x) cbind(x[1], paste0(x[2:4],collapse = "-")))
artical_date = as.data.frame(t(artical_date))
colnames(artical_date) = c("pmid","date")
artical_date$date = as.Date(artical_date$date)

### ### ### ### ### ### ### ###
##### Article Categories ######
### ### ### ### ### ### ### ###

source("./scripts/create_categories.R")

  two_type=unique(article_category[which(duplicated(article_category[,1])==TRUE),1])
  one.1 = article_category[-which(article_category[,1] %in% two_type),]
  one.2 = article_category[which(article_category[,1] %in% two_type),]
  df2 = NULL
  for(i in unique(one.2[,1])){
    a = which(one.2[,1]==i)
    df2 = rbind(data.frame(pmid = i, category = paste0(one.2[a,2], collapse = " | ")),df2)
  }
  one.1 = as.data.frame(one.1)
  colnames(one.1) = c("pmid","category")
  
  article_category = rbind(one.1,df2)
  
category_new = category

abstracts_new = abstracts
load("./updates/01jun2020/Rdata/old.RData")
abstracts = combineabs(abstracts_old,abstracts_new)

source("./scripts/create_categories.R")

### ### ### ### ### ### ### ###
##### Articel Authors #######
### ### ### ### ### ### ### ###    
  
  authors_name = apply(df_address[,c("firstname", "lastname")],1, function(x) paste0(x, collapse = " "))
  article_authors = data.frame(pmid = df_address[,"pmid"], author = authors_name)
  two_type=unique(article_authors[which(duplicated(article_authors[,1])==TRUE),1])
  one.1 = article_authors[-which(article_authors[,1] %in% two_type),]
  one.2 = article_authors[which(article_authors[,1] %in% two_type),]
  df2 = NULL
  for(i in unique(one.2[,1])){
    a = which(one.2[,1]==i)
    df2 = rbind(data.frame(pmid = i, author = paste0(one.2[a,2], collapse = " & ")),df2)
  }
  one.1 = as.data.frame(one.1)
  colnames(one.1) = c("pmid","author")
  article_authors = rbind(one.1,df2)
 
### ### ### ### ### ### ### ###
##### Authors Email #######
### ### ### ### ### ### ### ###    

authors_name = apply(df_address[,c("firstname", "lastname")],1, function(x) paste0(x, collapse = " "))
authors_email = na.omit(df_address)

### ### ### ### ### ### ###
####### Database ########## 
### ### ### ### ### ### ###

save(artical_date,article_affiliation,article_category,article_lang,article_authors,article_type,authors_email, file = "updates/current/tmp/Articles_info.RData")

db = Reduce(function(x, y) merge(x, y, by='pmid', all=TRUE),list(df[,1:4],artical_date,df[,c(1,8:10)],article_type,article_lang,article_affiliation,article_authors,article_category))

  df_temp = data.frame(pmid =abstracts@PMID, abs = abstracts@Abstract)
  db = db[-which(db$pmid %in% setdiff(db$pmid, df_temp$pmid)),]
  
rm(a,df,df_temp,article_lang,article_type,article_affiliation,one.1,one.2,df2,i,two_type,df_address,artical_date,article_category,article_authors,litcovid,m,time)