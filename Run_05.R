### ### ### ### ### ### ### ##
#### Data.frame Selection ####
### ### ### ### ### ### # ###

#No Abstract
for (i in 1:nrow(db.no_abstract)){
   db.no_abstract[i,"title"] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",db.no_abstract[i,"pmid"],"' target ='_blank'>",db.no_abstract[i,"title"],"</a>")
   db.no_abstract[i,"jabbrv"] = paste0("<a href='https://doi.org/",db.no_abstract[i,"doi"],"' target ='_blank'>",db.no_abstract[i,"jabbrv"],"</a>") 
}

#abstract
curated_pmids <- read.delim("/media/patgen/0d14f033-9f0b-48f9-97a1-b437c85165a5/Lucas/Corona/updates/17abril2020/RData/curated_pmids.txt", stringsAsFactors=FALSE)
curated_pmids <- read.delim("/Volumes/HD 320GB Arquivos/MEGA/Corona/RData/curated_pmids.txt", stringsAsFactors=FALSE)
a = unique(subset(curated_pmids, !deletar =="x")[,1])

for (i in paste0("\\b",pmid_category,"\\b")){
  db.abstract[grep(i, db.abstract$pmid),"selected"] = 3
}

for (i in paste0("\\b",a,"\\b")){
  db.abstract[grep(i, db.abstract$pmid),"curated"] = 3
}

for (i in 1:nrow(db.abstract)){
  if(is.na(db.abstract$selected[i])==FALSE) { 
     db.abstract[i,"title"] = paste0("<a href='/publication/",db.abstract[i,"pmid"],"' target ='_blank'>",db.abstract[i,"title"],"</a>")
    } else { db.abstract[i,"title"] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",db.abstract[i,"pmid"],"' target ='_blank'>",db.abstract[i,"title"],"</a>")
      }
db.abstract[i,"jabbrv"] = paste0("<a href='https://doi.org/",db.abstract[i,"doi"],"' target ='_blank'>",db.abstract[i,"jabbrv"],"</a>") 
}

#No category
#db.abstract[-which(db.abstract$pmid %in% pmid_category),c("title",'jabbrv')]
