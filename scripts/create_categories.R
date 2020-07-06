### ### ### ### ### ### ### ### ### ### 
###### Defining Category Terms ########
### ### ### ### ### ### ### ### ### ###
term1 = c("Diagnostic", "diagnosis", "Techniques", "Reagent Kits", "Immunologic Tests", "Nursing Diagnosis",
          "Serologic Tests", "Diagnostic Imaging", "Clinical Diagnosis", "Medicamentous Diagnosis", 
          "Diagnosis of Health Situation", "Clinical Laboratory Techniques", "Diagnostic Techniques and Procedures", 
          "Molecular Diagnostic Techniques", "Telediagnostics")

term2 = c("treatment", "Therapeutics", "Therapy","Medication", "Health Resorts", "Life Support Care", "Palliative Care", 
          "Patient Care Planning", "Patient Dropouts", "Residential Treatment", "drug effects", 
          "Intensive Care Units", "Pretreatment", "Clinical Protocols", "Patient Compliance", "Treatment Outcome",
          "Medical Futility", "Homebound Persons", "Withholding Treatment", "Medication Therapy Management", 
          "Medication Adherence", "Molecular Targeted Therapy", "Molecular Therapy","Transitional Care", 
          "Conservative Treatment","Teletherapy")

term3 = c("Epidemiology", "Epidemiologic", "Pharmacoepidemiology", "Health Services Research")

term4 = c("Transmission", "Sexually Transmitted Diseases", "Infectious Disease Transmission", 
          "Disease Transmission","Infection Transmission")
term5 = c("Clinical","Symptoms","Sing", "Sing and Symptoms", "Delivery of Health Care", 
          "Signs and Symptoms", "Clinical Medicine", "Medical Records ", "Treatment Outcome", 
          "Practice Guideline", "Critical Pathways", "Disease Management", "Decision Support Systems", 
          "Equivalence Trial")

### ### ### ### ### ### ### ###
### ### Creat term box.4 ### ##
### ### ### ### ### ### ### ###
eval(parse(text =
             paste0("m = max(unlist(lapply(list(",paste0("term",1:5,collapse = ","),"),length)))")
))
eval(parse(text = 
             paste0("box.4=data.frame(",paste0('term',1:5,'=c(term',1:5,',rep(',NA,',m-length(term',1:5,')))',collapse = ","),")")
))

WriteXLS(box.4,ExcelFileName = "export/Box2_categories_terms.xlsx",SheetNames = "terms",AdjWidth = T,BoldHeaderRow = T,FreezeRow = 1)

#setwd("./updates/current/")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
####### Category Creator & Counter Function Count #########
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
terms.count.table <-function(term,df_output_name){
  assign(df_output_name, searchabsL(abstracts, include = term),pos = ".GlobalEnv")
  df = read.table("dataout.txt",header = F, sep = "\n",quote = "")
  if(nrow(df) <= 3){
    a = df[c(1,2,nrow(df)),]
  } else {
    a = df[c(1,2,seq(4,nrow(df),2),nrow(df)),]
  }
  a = grep("No abstracts ",x = a,invert = T,value = T)
  a = sub("combined abstracts for above terms",paste0("abstracts combined terms ",df_output_name),a)
  assign(paste0(df_output_name,".1"),str_split(sub(" abstracts ",";",a), pattern = ";",n = 2,simplify = T),
         pos = ".GlobalEnv")
  eval(parse(text = paste0(df_output_name,".1 <<-as.data.frame(",df_output_name,".1)")))
  eval(parse(text= paste0("colnames(",df_output_name,".1) <<- c('Count','Term')")))
  eval(parse(text= paste0(df_output_name,".1[,1] <<- as.character(",df_output_name,".1[,1])")))
  eval(parse(text= paste0(df_output_name,".1[,1] <<- as.numeric(",df_output_name,".1[,1])")))
  eval(parse(text= paste0(df_output_name,".1[,2] <<- as.character(",df_output_name,".1[,2])")))
  file.remove("dataout.txt")
}

terms.count.table(term = term1, "g1")
terms.count.table(term = term2, "g2")
terms.count.table(term = term3, "g3")
terms.count.table(term = term4, "g4")
terms.count.table(term = term5, "g5")

abs  = list(g1 = g1,
            g2 = g2,
            g3 = g3,
            g4 = g4,
            g5 = g5)
count  = list(g1 = g1.1,
              g2 = g2.1,
              g3 = g3.1,
              g4 = g4.1,
              g5 = g5.1,
              all = rbind(g1.1,g2.1,g3.1,g4.1,g5.1))
category = list(abstract = abs, count = count)
rm(g1,g2,g3,g4,g5,abs,count,g1.1,g2.1,g3.1,g4.1,g5.1,terms.count.table)
setwd("../../")
#rm(term1,term2,term3,term4,term5)

a = data.frame(df[which(df$pmid %in% category$abstract$g1@PMID),], category = "diagnosis")
b = data.frame(df[which(df$pmid %in% category$abstract$g2@PMID),], category = "treatment")
c = data.frame(df[which(df$pmid %in% category$abstract$g3@PMID),], category = "epidemiology")
d = data.frame(df[which(df$pmid %in% category$abstract$g4@PMID),], category = "transmission")
e = data.frame(df[which(df$pmid %in% category$abstract$g5@PMID),], category = "signs")

db_abstract = rbind(a,b,c,d,e)
article_category = unique(db_abstract[,c("pmid","category")])
rm(a,b,c,d,e,db_abstract)
