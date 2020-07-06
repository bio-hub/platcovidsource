
### ### ### ### ### ### ### #### ### ### 
######## 1 Source the Libraries ######
### ### ### ### ### ### ### #### ### ###
source("./scripts/libraries_corona.R")
rm(corona_library)

dir.create("./updates/current")
dir.create("./updates/current/export")
dir.create("./updates/current/Rdata")
dir.create("./updates/current/tmp")

### ### ### ### ### ### ### #### ### 
###### 2 Creats the database #######
### ### ### ### ### ### ### #### ### 

download.file(url = "https://www.ncbi.nlm.nih.gov/research/coronavirus-api/export/tsv?",
                    destfile = "./updates/current/litcovid.tsv")
                            
write.table(Sys.time(), file = "./updates/current/update_date.txt",row.names = F, col.names = F)
                            
litcovid_new <- read.delim("./updates/current/litcovid.tsv", quote="", comment.char="#", stringsAsFactors=FALSE)
litcovid_old <- read.delim("./updates/01jun2020//litcovid.tsv", quote="", comment.char="#", stringsAsFactors=FALSE)
litcovid_new$pmid = as.character(litcovid_new$pmid)
litcovid_old$pmid = as.character(litcovid_old$pmid)
litcovid_old = litcovid_old[-which(litcovid_old$pmid %in% setdiff(litcovid_old$pmid, litcovid_new$pmid)),]

litcovid = litcovid_new[litcovid_new$pmid %in% setdiff(litcovid_new$pmid, litcovid_old$pmid),]
rm(litcovid_new,litcovid_old)

n = seq(1, length(litcovid$pmid), 200)
xml_files = NULL 
for (i in n){
  my_query = paste0(paste0(na.omit(litcovid$pmid[i:(i+199)]),collapse = "[UID] OR "),"[UID]")
  a = easyPubMed::batch_pubmed_download(my_query, 
                            format = "xml",
                            dest_file_prefix = paste0("updates/current/tmp/abstract_analysis_",i), 
                            api_key = "04ab6ece27f0a5fa6fdd6621cfc50beeb00a", 
                            batch_size = 200)
  xml_files = rbind(xml_files,a)
}
rm(n,my_query,a)


pmid = litcovid$pmid

#Download information by easyPumed 
my_PM_list = NULL
for (i in 1:length(xml_files)){
  my_PM_list <- c(my_PM_list, articles_to_list(pubmed_data = xml_files[i]))
}

nam = NULL
for (i in 1:length(xml_files)){
  n = paste0("abs_analisis_",i)
  assign(n, xmlreadabs(xml_files[i]))
  nam = c(nam,n)
  rm(n)
}
if(length(nam)>2){
  abstracts = eval(parse(text = paste0("combineabs(",paste0(nam[1:2],collapse = ","),")")))
  for(i in 3:length(nam)){
    abstracts = combineabs(abstracts, get(nam[i]))
  }
} else if(length(nam)==2) {
  abstracts = eval(parse(text = paste0("combineabs(",paste0(nam[1:2],collapse = ","),")")))
} else {
  abstracts = get(nam)
}
rm(list = nam)
rm(nam,i)

### Country List ###
countr = countrycode::codelist[,"country.name.en"] #Nome dos paises
countr=countr[-which(countr$country.name.en %in% c("Hamburg","Hanover","Modena","Jersey","Parma","Tuscany")),]
CountrWord = paste0(countr,collapse = "\\b|\\b") #Nome dos paises Para o grep

# Creates our db
source("scripts/create_data_base.R")


#OutPut Two Databases
db_new = db
load("updates//01jun2020/Rdata/old.RData")

db = rbind(db_old,db_new)