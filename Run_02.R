### ### ### ### ### ### ### #### ### ### 
######### 1 Word Atomization ###########
### ### ### ### ### ### ### #### ### ### 

### ### ### ### ### #
###  Global View  ###
### ### ### ### ### #

for (i in 1:length(abstracts@Abstract)){
  if (nchar(abstracts@Abstract[[i]]) == 1){ 
    abstracts@Abstract[[i]] = gsub("\\.", "No Abstract Found",abstracts@Abstract[[i]])}
}
setwd("updates/current/tmp")
wordAll= word_atomizations(abstracts)
file.remove("word_table.txt")

#Common words (in addition from pubmed.R)
  a = c("china","wuhan","abstract","covid","hubei","province","coronavirus","\\bsars-cov-2\\b",
        "2020","2019","january","december","february",
        "patient","cases","data","reported","number","including","early","\\bs\\b","\\bct\\b","\\bci\\b",
        "found","\\bhas\\b","\\bhad\\b","been","bmore","may","rights","should","among","high","both",
        "however","rapidly","three","while","further","evidence","lower","analyzed","many",
        "©","copyright","\\bused\\b","\\bsuch\\b","\\bmore\\b", "results","\\bwithout\\b","\\b000\\b","\\bb\\b","\\bvery\\b",
        "need" , "001)" , "review" , "10" , "here" , "ltd" , "within" , "might" , "\\binc\\b" ,"\\bthose\\b",
        "14" , "11" , "20" , "12" , "24" , "95%" , "known" , "type" , "16" , "30" , "occurred" ,"\\belsevier\\b", "\\bchinese\\b","\\bsars-cov\\b",
        "15" , "identify" , "interval" , "key" , "united" , "\\busing\\b", "25" , "\\bcount\\b","\\vs\\b","\\bdue\\b","likely"
  )
  commonWord = paste0(a,collapse = "|");rm(a)
  a = grep(pattern = commonWord, x = wordAll$words,value = F,ignore.case = T,invert = T)

wordAll.filtred = wordAll[a,]
wordAllTop50 = wordAll[which(which(wordAll$Freq>=50) %in% a),]
excluded.words = wordAll[grep(pattern = commonWord, x = wordAll$words,value = F,ignore.case = T,invert = F),]
rm(a)
  
### ### ### ### ### ###
###  category View  ###
### ### ### ### ### ###
wordList = eval(parse(text = paste0("list(",paste0("g",1:5,"= word_atomizations(category$abstract$g",1:5,")", collapse = ","),")")))
file.remove("word_table.txt")

#Word.category Function --> 
  word.category = function(df_name = "The name of output",Word_List = "WordList object"){
    for (i in 1:5){
      assign(paste0("a",i), eval(parse(text= paste0("grep(pattern = commonWord, x = ",Word_List,"$g",i,"$words,value = F,ignore.case = T,invert = T)"))))
      assign(paste0("a.",i), 
             eval(parse(text = paste0(Word_List,"$g",i,"[grep(pattern = commonWord, x = ",Word_List,"$g",i,"$words,value = F,ignore.case = T,invert = F),]"))))
     }
  eval(parse(text = paste0(df_name,"<<- list(",paste0("g",1:5," = wordList$g",1:5,"[a",1:5,",]",collapse = ","),")")))
  eval(parse(text = paste0(df_name,"_exluded<<- list(",paste0("g",1:5," = a.",1:5,"[a",1:5,",]",collapse = ","),")")))
  }

word.category(df_name = "wordAll.g.Top50", Word_List = "wordList")
rm(word.category)
box.category = data.frame(
  Diagnose =paste0(wordAll.g.Top50$g1[1:10,1]," (",wordAll.g.Top50$g1[1:10,2],")"),
  Treatment= paste0(wordAll.g.Top50$g2[1:10,1]," (",wordAll.g.Top50$g2[1:10,2],")"),
  Epidemiology= paste0(wordAll.g.Top50$g3[1:10,1]," (",wordAll.g.Top50$g3[1:10,2],")"),
  Transmission= paste0(wordAll.g.Top50$g4[1:10,1]," (",wordAll.g.Top50$g4[1:10,2],")"),
  Sings= paste0(wordAll.g.Top50$g5[1:10,1]," (",wordAll.g.Top50$g5[1:10,2],")")
)
colnames(box.category) = c("Diagnose (n)","Treatment (n)","Epidemiology (n)","Transmission (n)","Signs (n)")

box.2 =cbind(wordAllTop50[1:50,],
             wordAll.g.Top50$g1[1:50,], 
             wordAll.g.Top50$g2[1:50,],
             wordAll.g.Top50$g3[1:50,],
             wordAll.g.Top50$g4[1:50,],
             wordAll.g.Top50$g5[1:50,])
colnames(box.2) = c("Global","Freq","Diagnose","Freq","Treatment","Freq","Epidemiology","Freq","Transmission","Freq","Signs","Freq")

box.3 =cbind(excluded.words[1:50,],
             wordAll.g.Top50_exluded$g1[1:50,], 
             wordAll.g.Top50_exluded$g2[1:50,],
             wordAll.g.Top50_exluded$g3[1:50,],
             wordAll.g.Top50_exluded$g4[1:50,],
             wordAll.g.Top50_exluded$g5[1:50,])
colnames(box.3) = c("Global","Freq","Diagnose","Freq","Treatment","Freq","Epidemiology","Freq","Transmission","Freq","Sings","Freq")

WriteXLS::WriteXLS(x = c("box.2","box.3"), "../export/table_words.xlsx",SheetNames = c("included","excluded"), row.names = F, FreezeRow =  1)
rm(commonWord)

### ### ### ### ### ### ### ### ### ### ### ### ### 
######### 2 Find Conclusion by Category ###########
### ### ### ### ### ### ### ### ### ### ### ### ###
conclusion.abs = function(group = "vector with groups name"){
  eval(parse(text = paste0("a = Find_conclusion(category$abstract$",group,")")))
  eval(parse(text = paste0("conclusion_",group," <<- setNames(a,category$abstract$",group,"@PMID)")))
}
for(i in c("g1","g2","g3","g4","g5")){
  conclusion.abs(i)
}

conclusion = eval(parse(text = paste0("list(",paste0("g",1:5,"= conclusion_g",1:5, collapse = ","),")")))
rm(conclusion_g1,conclusion_g2,conclusion_g3,conclusion_g4,conclusion_g5,i,conclusion.abs)

group = c("g1","g2","g3","g4","g5") 
for (j in group){
  assign(j, NULL)
  assign("a",eval(parse(text = paste0("Filter(Negate(is.null), conclusion$",j,")"))))
  for(i in 1:length(a)){
    a.1= paste0(1,"-",a[[i]][1]," | ",
                2,"-",a[[i]][2]," | ",
                3,"-",a[[i]][3]," | ",
                4,"-",a[[i]][4])
    assign(j, rbind(get(j),data.frame(pmid = names(a)[i], conclusion = a.1, stringsAsFactors = F)))
  }
}  
conclusion_df <<- eval(parse(text = paste0("list(",paste0("g",1:5,"= g",1:5, collapse = ","),")")))
rm(j,g1,g2,g3,g4,g5,i,a,a.1)

### ### ### ### ### ### ### ### ### ### ### ###  
######### 3 Find Context by Category ##########
### ### ### ### ### ### ### ### ### ### ### ###  

term1.1 = paste0(term1,collapse = "\\b|\\b")
term2.1 = paste0(term2,collapse = "\\b|\\b")
term3.1 = paste0(term3,collapse = "\\b|\\b")
term4.1 = paste0(term4,collapse = "\\b|\\b")
term5.1 = paste0(term5,collapse = "\\b|\\b")

for(j in c("term1.1","term2.1","term3.1","term4.1","term5.1")){
    a.3 = NULL;a.4=NULL
    for(i in grep(get(j), db$abstract, ignore.case = T, value = F)){
      a.1 = db$abstract[i]
      a.2 = tokenizers::tokenize_sentences(a.1)
      a.3 = rbind(a.3,
                  cbind(db$pmid[i],
                  paste0(grep(get(j),a.2[[1]],ignore.case = T, value = T),collapse = " [...] ")))
      a.4 = rbind(a.4,cbind(db$pmid[i],grep(get(j),a.2[[1]],ignore.case = T, value = T)))
    
      if(j=="term1.1"){
        colnames(a.3) = c("pmid","context")
        g1_context = as.data.frame(a.3)
        g1_bold = a.4
      } else if(j=="term2.1"){
        colnames(a.3) = c("pmid","context")
        g2_context =as.data.frame(a.3)
        g2_bold = a.4
      } else if(j=="term3.1"){
        colnames(a.3) = c("pmid","context")
        g3_context = as.data.frame(a.3)
        g3_bold = a.4
      } else if(j=="term4.1"){
        colnames(a.3) = c("pmid","context")
        g4_context = as.data.frame(a.3)
        g4_bold = a.4
      } else if(j=="term5.1"){
        colnames(a.3) = c("pmid","context")
        g5_context = as.data.frame(a.3)
        g5_bold = a.4
      }
    }
}

g1_context = merge(g1_context,db[,c("pmid","date")], by="pmid",all.x = T,all.y = F)
g2_context = merge(g2_context,db[,c("pmid","date")], by="pmid",all.x = T,all.y = F)
g3_context = merge(g3_context,db[,c("pmid","date")], by="pmid",all.x = T,all.y = F)
g4_context = merge(g4_context,db[,c("pmid","date")], by="pmid",all.x = T,all.y = F)
g5_context = merge(g5_context,db[,c("pmid","date")], by="pmid",all.x = T,all.y = F)

context = list(
              context = eval(parse(text = paste0("list(",paste0("g",1:5,"=g",1:5,"_context", collapse = ","),")"))),
              bold =     eval(parse(text = paste0("list(",paste0("g",1:5,"=g",1:5,"_bold", collapse = ","),")")))
  
) 

rm(a.1,a.2,a.3,a.4,i,j,g1_context,g2_context,g3_context,g4_context,g5_context,g1_bold,g2_bold,g3_bold,g4_bold,g5_bold)
rm(term1,term2,term3,term4,term5,term1.1,term2.1,term3.1,term4.1,term5.1)


WriteXLS(c("a1","a2","a3","a4","a5"), ExcelFileName = "../export/Conclusion_context_Comparisons.xlsx", 
         SheetNames = c("diagnsosis","treatment","epidemiology","trasmission","sings"),row.names = F, FreezeRow =  1)

rm(a,a1,a2,a3,a4,a5,b,c,i)

### ### ### ### ### ### ### ###
####### 4 Date Analysis ########
### ### ### ### ### ### ### ###

library(dplyr)
#country.date.pmid = na.omit(merge(pmid.country,db[,c("pmid","date")], by ="pmid",all = T))
a = db[,c("pmid","date")]
start.date = as.Date("2020-01-01")
start.month = as.numeric(format(start.date, "%m"))
last.date = max(na.omit(a$date))
last.month = as.numeric(format(last.date, "%m")) 
n=seq(7,(round(as.numeric(last.date-start.date)/7,0)*7),7)

eval(parse(text = paste0("date.pmid = 
  list(",paste0(month.abb[start.month:last.month]," = country.date.pmid %>% filter(between(date,as.Date('", 
                paste0('2020-',start.month:last.month,'-01'),"'),as.Date('", 
                sub("2020-2-30","2020-2-29",paste0('2020-',start.month:last.month,'-',31:30)),"')))", collapse = ","),",
       
  week = list(",paste0("s",1:round(as.numeric(last.date-start.date)/7,0)," = country.date.pmid %>% filter(between(date,as.Date('",start.date,"'),as.Date('",start.date+n,"')))" , collapse = "," )," 
              )
  )"
)
))

source("../../../function/abs.date_fun.R")
abs.date(df_name = "abs_date",df.week = date.pmid, df_abstract_db = abstracts)
rm(abs.date,a)



### ### ### ### ### ### ### #### ### ### 
###### 5 Affiliation Analysis ########
### ### ### ### ### ### ### #### ### ### 

### ### ### ### ### #
###  Entire View  ###
### ### ### ### ### #
CountryAffiliation =  as.data.frame(table(pmid.country[,2]))
colnames(CountryAffiliation) = c("country","Freq")
CountryAffiliation = CountryAffiliation[order(-CountryAffiliation$Freq),]

df = data.frame(pmid =abstracts@PMID, abs = abstracts@Abstract)
pmid_abs = df[grep(pattern = "No Abstract Found",x = df$abs,value = F,invert = T),]

CountryAffiliation_abs =  as.data.frame(table(pmid.country[which(pmid.country$pmid %in% pmid_abs$pmid),2]))
CountryAffiliation_abs = CountryAffiliation_abs[order(-CountryAffiliation_abs$Freq),]


country.date.pmid = na.omit(merge(pmid.country,db[,c("pmid","date")], by ="pmid",all = T))
load("../../01jun2020/Rdata/country_date_old.Rdata")
country.date.pmid = rbind(country.date.pmid_old,country.date.pmid)

start.date = as.Date("2020-01-01")
start.month = as.numeric(format(start.date, "%m"))
last.date = max(na.omit(country.date.pmid$date))
last.month = as.numeric(format(last.date, "%m")) 
n=seq(7,(round(as.numeric(last.date-start.date)/7,0)*7),7)

eval(parse(text = paste0("country.date.pmid.1 = 
  list(",paste0(month.abb[start.month:last.month]," = country.date.pmid %>% filter(between(date,as.Date('", 
                paste0('2020-',start.month:last.month,'-01'),"'),as.Date('", 
                sub("2020-2-30","2020-2-29",paste0('2020-',start.month:last.month,'-',31:30)),"')))", collapse = ","),",
       
  week = list(",paste0("s",1:round(as.numeric(last.date-start.date)/7,0)," = country.date.pmid %>% filter(between(date,as.Date('",start.date,"'),as.Date('",start.date+n,"')))" , collapse = "," )," 
              )
  )"
)
))

source("../../../function/WorldAffiliation_Week_fun.R")
WorldAffiliation.Week(df_name = "CountryAffiliationWeek",country_date_pmid = "country.date.pmid.1")
CountryAffiliationWeek = CountryAffiliationWeek[order(-CountryAffiliationWeek[,ncol(CountryAffiliationWeek)]),]
CountryAffiliationWeek = CountryAffiliationWeek[which(CountryAffiliationWeek[,ncol(CountryAffiliationWeek)]>0),]
rm(WorldAffiliation.Week)

### ### ### ### ###
### For Maps ### ##
### ### ### ### ###

AffiliationWeek = NULL
for(i in CountryAffiliation$country){
  for (j in paste0("s",1:10)){
    a = paste0("\\b",i,"\\b")
    eval(parse(text =paste0("b = db[which(db$pmid %in% date.pmid$week$",j,"$pmid),c('country')]")))
    c = grep(a,x = b, ignore.case = T,value = T)
    AffiliationWeek =  rbind(AffiliationWeek, data.frame(country = i,week = j, Freq = length(c)))
  }
}

### ### ### ### ### ### ### #
### Category Weeky View  ###
### ### ### ### ### ### ### #
AffiliationWeekCategory = NULL
for(i in CountryAffiliation$country){
  for (j in paste0("s",1:10)){
    for(l in c("diagnosis","epidemiology","transmission","signs","treatment")){
      a = paste0("\\b",i,"\\b")
      b = db[grep(l,db$category,ignore.case = T,value = F),]
      eval(parse(text =paste0("c = db[which(b$pmid %in% date.pmid$week$",j,"$pmid),c('country')]")))
      d = grep(a,x = c, ignore.case = T,value = F)
      AffiliationWeekCategory = rbind(AffiliationWeekCategory, data.frame(country = i,week = j, category = l, Freq = length(d)))
    }
  }
}

### ### ### ### ### ### ### #### ### ### 
###### 6 World Citation Analysis #######
### ### ### ### ### ### ### #### ### ###

### ### ### ### ### #
###  Entire View  ###
### ### ### ### ### #
WorldCount = NULL
  for (i in countr$country.name.en){
    a = paste0("\\b",i,"\\b")
    b = grep(a,x = db$abstract, ignore.case = T,value = F)
    WorldCount =  rbind(WorldCount, data.frame(country = i,Freq = length(b)))
  }
    rm(a,b,i)
WorldCount = WorldCount[which(WorldCount$Freq>0),]
WorldCount = WorldCount[order(-WorldCount$Freq),]

### ### ### ### ### #
###  Weekly View  ###
### ### ### ### ### #

CountryWordWeek = NULL
for(i in WorldCount$country){
  for (j in paste0("s",1:10)){
    a = paste0("\\b",i,"\\b")
    eval(parse(text =paste0("b = db[which(db$pmid %in% date.pmid$week$",j,"$pmid),c('abstract')]")))
    c = grep(a,x = b, ignore.case = T,value = F)
    CountryWordWeek =  rbind(CountryWordWeek, data.frame(country = i,week = j, Freq = length(c)))
  }
}

### ### ### ### ### ### ### #
###  Category Weeky View  ###
### ### ### ### ### ### ### #

CountryWorldCategoryWeek = NULL
for(i in WorldCount$country){
  for (j in paste0("s",1:10)){
    for(l in c("diagnosis","epidemiology","transmission","signs","treatment")){
      a = paste0("\\b",i,"\\b")
      b = db[grep(l,db$category,ignore.case = T,value = F),]
      eval(parse(text =paste0("c = db[which(b$pmid %in% date.pmid$week$",j,"$pmid),c('abstract')]")))
      d = grep(a,x = c, ignore.case = T,value = F)
      CountryWorldCategoryWeek = rbind(CountryWorldCategoryWeek, data.frame(country = i,week = j, category = l, Freq = length(d)))
    }
  }
}
rm(a,b,c,d,i,j,l)

### ### ### ### ### ### ### ###
###### Gene Atomization #######
### ### ### ### ### ### ### ###

gene.fun = function(df_name = "name of ouput",abstracts = "Class S4 of Abstract"){
  a = gene_atomization(abstracts)
  b = as.data.frame(a,make.names = F)
  b = b[-grep(pattern = "\\bSARS\\b|\\bT\\b",value = F,x = b$Gene_symbol),]
  nam = NULL
  for(j in as.character(b$Gene_symbol)){
    a.3 = NULL;
    for(i in grep(paste0("\\b",j,"\\b"), db$abstract, ignore.case = T, value = F)){
      a.1 = db$abstract[i]
      a.2 = tokenizers::tokenize_sentences(a.1)
      a.3 = rbind(a.3,
                  cbind(db$pmid[i],
                        paste0(grep(pattern = j,x = a.2[[1]],ignore.case = T, value = T),collapse = " [...] ")))
      assign(j ,a.3)
    }
    if(identical(grep(paste0("\\b",j,"\\b"), db$abstract, ignore.case = T, value = F),integer(0))){
      b = b[-grep(pattern = j,value = F,x = b$Gene_symbol),]
    } else {
      nam <- c(nam,j)
    }
  }
  eval(parse(text = paste0(df_name,"_list <<-b"))) 
  eval(parse(text = paste0(df_name,"<<- list(",paste0("`",nam,"` = `",nam,"`",collapse = ","),")")))
}

gene.fun(df_name = "gene",abstracts)
rm(gene.fun)


### ### ### ### ### ### ### ###
###### Drug Atomization #######
### ### ### ### ### ### ### ###
setwd("../../../")
Products <- read.delim("./drugsatfda20200414/Products.txt", quote="", comment.char="#", stringsAsFactors=FALSE)
drugbank <- read.csv("drugbank vocabulary.csv",stringsAsFactors = F)

drugs1 = tolower(unique(Products$DrugName))
drugs2 = tolower(unique(drugbank$Common.name))
drugs.names = unique(c(drugs1,drugs2))
rm(drugs1,drugs2)

drug_atomization = function(m){tempzz = unlist(lapply(m@Abstract, function(x){tempa = strsplit(x, ".  ",fixed=T);tempa1 = which( nchar(tempa[[1]]) == max(nchar(tempa[[1]])));
  tempb = unlist(strsplit(tempa[[1]][tempa1], ".",fixed = T));
  tempc = unlist(strsplit(tempb, ",",fixed = T));
  tempd = unlist(strsplit(tempc, ":",fixed = T));
  tempe = unlist(strsplit(tempd, ";",fixed = T));
  tempe1 = unlist(strsplit(tempe, "'",fixed = T));
  tempf = unlist(strsplit(tempe1, " ",fixed = T));
  tempf1 = unlist(strsplit(tempf, "/",fixed = T));
  tempg = tolower(tempf1);
  temph = sort(tempg);
  return(tempg)}))
  tempi = as.data.frame(table(tempzz));
  tempj = unlist(lapply(drugs.names, function(x){tempoo = which(as.character(tempi[,1]) == x   ); if (length(tempoo) != 0) return(tempoo)}));
  tempk = tempi[tempj,];
  tempk2=tempk[order(as.numeric(tempk$Freq), decreasing = T),]
  colnames(tempk2)=c("drug", "Freq")
  write.table(tempk2, file = "gene_table.txt", sep = "\t", row.names = F); return(tempk2)}

drug.fun = function(df_name = "name of ouput",abstracts = "Class S4 of Abstract"){
  a = drug_atomization(abstracts)
  b = as.data.frame(a,make.names = F)
  b$drug = as.character(b$drug)
  nam = NULL;
  del = NULL;
  for(j in as.character(b$drug)){
    a.3 = NULL;
    for(i in grep(j, db$abstract, ignore.case = T, value = F)){
      a.1 = db$abstract[i]
      a.2 = tokenizers::tokenize_sentences(a.1)
      a.3 = rbind(a.3,
                  data.frame(pmid = as.character(db$pmid[i]),
                            sentence =  paste0(grep(pattern = paste0("\\b",j,"\\b"),x = a.2[[1]],ignore.case = T, value = T),collapse = " [...] "),stringsAsFactors = F))
      assign(gsub("-",".",j) ,a.3)
    }
    
    if(identical(grep(paste0(j), db$abstract, ignore.case = T, value = F),integer(0))==TRUE){
      del = c(del,j)} else {nam = c(nam,gsub("-",".",j))}
  }
if(is.null(del)){
  eval(parse(text = paste0(df_name,"_list <<-b")))
} else { 
  b = b[-grep(paste0(del,collapse = "|"),b$drug),]
  eval(parse(text = paste0(df_name,"_list <<-b")))
  }
  eval(parse(text = paste0(df_name,"<<- list(",paste0("`",nam,"` = `",nam,"`",collapse = ","),")")))
}

setwd("updates/current/tmp/")
drug.fun(df_name = "drugs",abstracts)

a = NULL
for(i in 1:length(drugs)){
  a = rbind(a,nrow(subset(drugs[[i]], !sentence=="", select = "pmid")))
}

drugs_list = data.frame(drugs_list,a,stringsAsFactors = FALSE)
colnames(drugs_list) = c("Drug","Citation Frequency","Article number")

setwd("../../../")
