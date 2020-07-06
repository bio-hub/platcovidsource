
## Abrir esse file apenas no project source.
load('../../updates/current/Rdata/final.RData')

### ### ### ### ### #
#### Gene Panel #####
### ### ### ### ### #
for(i in 1:nrow(gene_list_new)){
  template.dir = paste0("../unsourced/gene/")
    list.of.files = paste0(template.dir,list.files(path = template.dir, pattern = ".Rmd"))
  new.dir = paste0("content/genepanel/",gene_list_new[i,5])
    dir.create(new.dir)
  file.copy(from = list.of.files, to = new.dir,copy.mode = T)
  list.of.files.new = list.files(new.dir, pattern = ".Rmd")
  
  for(j in list.of.files.new){
    file1 = readLines(paste0(new.dir,"/",j))
      file1 = gsub("number_to_sub",i,file1)
      file1 = gsub("gene.menu",paste0("gene.menu.",i),file1)
    writeLines(file1, con=paste0(new.dir,"/",j))
  }
 }


### ### ### ### ### #
#### Drug Panel #####
### ### ### ### ### #
for(i in 1:nrow(drugs.view)){
  template.dir = paste0("../unsourced/drug/")
    list.of.files = paste0(template.dir,list.files(path = template.dir, pattern = ".Rmd"))
    new.dir = paste0("content/drugpanel/",drugs.view[i,1])
    dir.create(new.dir)
    file.copy(from = list.of.files, to = new.dir,copy.mode = T)
    list.of.files.new = list.files(new.dir, pattern = ".Rmd")
    
    for(j in list.of.files.new){
      file1 = readLines(paste0(new.dir,"/",j))
      file1 = gsub("number_to_sub",i,file1)
      file1 = gsub("drug.menu",paste0("drug.menu.",i),file1)
      writeLines(file1, con=paste0(new.dir,"/",j))
    }
}  



### ### ### ### ### ###
#### tissue Panel #####
### ### ### ### ### ###
# Tissue
for(i in 1:nrow(tissue_list)){
  template.dir = paste0("../unsourced/tissue/")
  list.of.files = paste0(template.dir,list.files(path = template.dir, pattern = ".Rmd"))
  new.dir = paste0("content/tissuepanel/",sub(" ","_",tissue_list[i,1]))
  dir.create(new.dir,showWarnings = F)
  file.copy(from = list.of.files, to = new.dir,copy.mode = T)
  list.of.files.new = list.files(new.dir, pattern = ".Rmd")
  
  for(j in list.of.files.new){
    file1 = readLines(paste0(new.dir,"/",j))
    file1 = gsub("number_to_sub",i,file1)
    file1 = gsub("title_to_sub",tissue_list[i,1],file1)
    file1 = gsub("tissue.menu",paste0("tissue.menu.",i),file1)
    writeLines(file1, con=paste0(new.dir,"/",j))
  }
}  



#Cell
for(i in 1:nrow(cell_list)){
  template.dir = paste0("../unsourced/cell/")
  list.of.files = paste0(template.dir,list.files(path = template.dir, pattern = ".Rmd"))
  new.dir = paste0("content/tissuepanel/",sub(" ","_",cell_list[i,1]))
  dir.create(new.dir,showWarnings = F)
  file.copy(from = list.of.files, to = new.dir,copy.mode = T,overwrite = T)
  list.of.files.new = list.files(new.dir, pattern = ".Rmd")
  
  for(j in list.of.files.new){
    file1 = readLines(paste0(new.dir,"/",j))
    file1 = gsub("number_to_sub",i,file1)
    file1 = gsub("title_to_sub",cell_list[i,1],file1)
    file1 = gsub("tissue.menu",paste0("tissue.menu.",i),file1)
    writeLines(file1, con=paste0(new.dir,"/",j))
  }
}  

#Rodar o bug_fix_menu
soure('../unsourced/fix_bug_menu.R')

server = blogdown:::serve_site()
server$stop_server()


### ### ### ### ### ### ### #
#### Article Post Panel #####
### ### ### ### ### ### ### #

### Mark abstract ###
source("../../scripts/mark_abstracts.R")
db_marked = merge(db, abs_bold, by = "pmid", all = T)

### Curated Articles ###
curated_pmids <- read.delim("/media/patgen/0d14f033-9f0b-48f9-97a1-b437c85165a5/Lucas/Corona/RData/curated_pmids.txt", stringsAsFactors=FALSE)
curated_pmids <- read.delim("/Volumes/HD 320GB Arquivos/MEGA/Corona/RData/curated_pmids.txt", stringsAsFactors=FALSE)

### Fix the database ###
#a = unique(subset(curated_pmids, !deletar =="x")[,1])
a = unique(subset(db.abstract, selected == 3, select = "pmid"))
a = a[a$pmid %in% db_new$pmid,]
a = db_marked[db_marked$pmid %in% a,]
a = sapply(a, function(x) sub("^$",NA,x))
a = data.frame(a, stringsAsFactors = F)
a = sapply(a, function(x) gsub("&quot;","",x))
a = data.frame(a, stringsAsFactors = F)
a = sapply(a, function(x) gsub("\"","",x))
a = data.frame(a, stringsAsFactors = F)
a = sapply(a, function(x) gsub("@","",x))
a = data.frame(a, stringsAsFactors = F)
a = sapply(a, function(x) gsub("<i>|</i>","",x))
a = data.frame(a, stringsAsFactors = F)
a = a[-which(is.na(a$doi)),]
a[which(is.na(a$jabbrv)),"jabbrv"] = "None"
a$keywords = sapply(a$keywords, function(x) gsub("\\ \\(","; ",x))
a$keywords = sapply(a$keywords, function(x) gsub("\\(","",x))
a$abstract_bold = sapply(a$abstract_bold, function(x) gsub("\\*\\*\\*\\*","\\*\\*",x))
#a[grep("32275256",value = F, a$pmid),"author"]= NA
#a[grep("32278065",value = F, a$pmid),"keywords"]= NA



for(i in 1:nrow(a)){
new.dir.rm = paste0("content/publication/",a$pmid[i],"/")
new.dir.rm2 = paste0("../patgen.github.io/publication/",a$pmid[i],"/") 
unlink(new.dir.rm, recursive = T)
unlink(new.dir.rm2, recursive = T)
}

for (i in 1:nrow(a)){
  template.dir = paste0("../unsourced/publication/")
    list.of.files = paste0(template.dir,list.files(path = template.dir, pattern = ".md"))
  new.dir = paste0("content/publication/",a$pmid[i])
    dir.create(new.dir,showWarnings = F)
    file.copy(from = list.of.files, to = new.dir,copy.mode = T,overwrite = T)
    files.new = list.files(new.dir, pattern = ".md")

    
#type  
    types.abstract=sub("Journal Article","2",a$category[i])
      types.abstract=sub("diagnosis","9",types.abstract)
      types.abstract=sub("treatment","10",types.abstract)
      types.abstract=sub("epidemiology","11",types.abstract)
      types.abstract=sub("transmission","12",types.abstract)
      types.abstract=sub("signs","13",types.abstract)
      types.abstract = gsub("\\|", "\n-",types.abstract)

    file1 = readLines(paste0(new.dir,"/",files.new))
  # Category    
      category.abstract = gsub("\\|", "\n-",a$category[i])
      file1 = gsub("sub_to_category",category.abstract,file1)
      authors = if(is.na(a$author[i])==T){
      ""} else {gsub("&", "\n-",a$author[i])}
      file1 = gsub("sub_to_epub",a$date[i],file1)
      file1 = gsub("sub_to_pmid",a$pmid[i],file1)
      file1 = gsub("sub_to_authors",authors,file1)
      file1 = gsub("\\bsub_to_journal\\b",a$journal[i],file1)
      file1 = gsub("\\bsub_to_journal_short\\b",a$jabbrv[i],file1)
  #tag - keywords      
      tag.keywords = if(is.na(a$category[i])==T){
        ""} else {gsub(";|,", "\n-",a$category[i])}
      file1 = gsub("\\bsub_to_keywords\\b",tag.keywords,file1)
      
#summary
      summary.abstract = paste0("Journal: ",a$jabbrv[i], " | PMID: ",a$pmid[i]," | doi: ",a$doi[i]," | Type: ",a$type[i],". <br> ",substr(a$abstract[i], start = 1, stop = 120)," (...)")
      if(length(grep("\\:|'|\\[",x = summary.abstract))>=1){file1 = gsub("sub_to_summary.abstract",paste0("\"",summary.abstract,"\""),file1) 
      } else {file1 = gsub("sub_to_summary.abstract",paste0("\'",summary.abstract,"\'"),file1) }
    
#title      
      if(length(grep("\\:|'|\\[",x = a$title[i]))>=1){file1 = gsub("sub_to_title",paste0("\"",a$title[i],"\""),file1) 
      } else {file1 = gsub("sub_to_title",paste0("\'",a$title[i],"\'"),file1) }
#doi      
      #file1 = if(is.na(a$doi[i])==T){gsub("sub_to_doi_link","",file1)
      #  } else gsub("sub_to_doi_link", paste0("url_pdf: http://doi.org/sub_to_doi",a$doi[i]),file1)
      file1 = if(is.na(a$doi[i])==T){gsub("sub_to_doi","",file1)
      } else gsub("sub_to_doi", a$doi[i],file1)
#Abstract
      abstract.temp = gsub(" "," ",a$abstract_bold[i])
      if(length(grep("\\:",x = a$abstract_bold[i]))>=1){file1 = gsub("sub_to_abstract",paste0("\"",abstract.temp,"\""),file1) 
      } else if (length(grep("\\*",strsplit(a$abstract_bold[i],split = " ",fixed = T)[[1]][1],value = F))>=1){
        file1 = gsub("sub_to_abstract",paste0("\"",abstract.temp,"\""),file1) 
      } else {file1 = gsub("sub_to_abstract",abstract.temp,file1) 
      }    
#Type      
      file1 = gsub("sub_to_type",types.abstract,file1)
      
      writeLines(file1, con=paste0(new.dir,"/",files.new))
      #server = blogdown:::serve_site()
      #server$stop_server()
}


### ### ### ### ### ###
#### Context Panel ####
### ### ### ### ### ###

# context[["context"]][["g1"]]
# context[["context"]][["g2"]]

temp = unique(subset(db.abstract, selected == 3, select = "pmid"))
temp = db_marked[db_marked$pmid %in% temp$pmid,]
save(context,category,box.4,temp, file = "../../updates/current/Rdata/context.Rdata")

save.image(file = "../../updates/current/Rdata/final.Rdata")



server = blogdown:::serve_site()
server$stop_server()






df=context[["context"]][["g1"]]
df$pmid = as.character(df$pmid)
df = (merge(a,df, by="pmid", all.x = F, all.y=T))
for (i in 1:nrow(df)){
  if(is.na(df$category[i])==FALSE) { 
    df[i,"title"] = paste0("<a href='/publication/",df[i,"pmid"],"' target ='_blank'>",df[i,"title"],"</a>")
  } else { df[i,"title"] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",df[i,"pmid"],"' target ='_blank'>",df[i,"pmid"],"</a>")
  }
}


df = category[["count"]][["g2"]]
df=df[-setdiff(
  grep("combined terms g",category[["count"]][["g1"]][["Term"]]),
  max(grep("combined terms g1",category[["count"]][["g1"]][["Term"]]))),]

df = merge(df, data.frame(Term=na.omit(box.4[,2])), by="Term", all=T)

df$Term = sapply(df$Term,function(x) sub("combined terms g2","All (sum)", x))

df = df[order(-df$Count),]
     
setwd("../../")     


### Old ###

render(input = "../unsourced/gpanel_template.Rmd",output_format = "html_document",
       output_file = "index.html",output_dir = "./static/gpanel/")

for(i in 1:nrow(gene.view.ALL_new)){
  render(input = "../unsourced/gpanel_template.Rmd",output_format = "html_document",
         output_file = paste0(gene_list_new[i,5],".html"),output_dir = "./static/gpanel/")
}


for(i in 1:nrow(drugs.view)){
  render(input = "../unsourced/dpanel_template.Rmd",output_format = "html_document",
         output_file = paste0(sub("-",".",drugs.view[i,1],".html")),output_dir = "./static/dpanel/")
}



for(i in 1:nrow(tissue_list)){
  render(input = "../unsourced/tpanel_template.Rmd",output_format = "html_document",
         output_file = paste0(sub(" ","_",tissue_list[i,1],".html")),output_dir = "./static/tpanel/")
}


for(i in 1:nrow(cell_list)){
  render(input = "../unsourced/cpanel_template.Rmd",output_format = "html_document",
         output_file = paste0(sub(" ","_",cell_list[i,1],".html")),output_dir = "./static/cpanel/")
}