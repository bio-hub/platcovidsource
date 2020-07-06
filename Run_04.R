### ### ### ### ### ### #
#### Gene View Panel ####
### ### ### ### ### ### #

library(rentrez)
library(GeneBook)
library(org.Hs.eg.db)

### ### ### ### 
### EntrezID ##
### ### ### ### 
b = select(x = org.Hs.eg.db, keys = as.character(gene_list$Gene_symbol),columns = c("SYMBOL","ENTREZID"), keytype = "SYMBOL")
source("../../../89a2b186-953d-4db5-9e30-f9c92445273c/InterOmics/genome/output/function_fix_GenID.R")
fix.geneID(b = b , df = "a")
c = select(x = org.Hs.eg.db, keys = as.character(a$ENTREZID),columns = c("SYMBOL","ENTREZID","ENSEMBL"), keytype = "ENTREZID")
setwd('updates/current/')
### ### ### ### # 
### Gene View ###
### ### ### ### #
gene.view.fun = function(entrezid = "ENTREZID"){
  gene.info <- rentrez::entrez_summary(db="gene", id=entrezid)
  gene.view <<- data.frame(name = gene.info$name, 
                           ENTREZID = gene.info$uid,
                           description = gene.info$description,
                           type = GeneCard_Symbol_Details(gene.info$name)$type,
                           map = gene.info$maplocation,
                           summary_entrez =  gene.info$summary,
                           #summary_genecard = GeneCard_Symbol_Details(gene.info$name)$summary_genecard,
                           summary_genecard =sub(paste0("\\GeneCards\\ \\Summary \\for\\ \\",gene.info$name,"\\ \\Gene\\ "),"",GeneCard_Symbol_Details(gene.info$name)$summary_genecard),
                           #summary_uniport = GeneCard_Symbol_Details(gene.info$name)$summary_uniport,
                           summary_uniport =sub(paste0("\\UniProtKB/Swiss-Prot\\ \\for\\ \\",gene.info$name,"\\ \\Gene"),"",GeneCard_Symbol_Details(gene.info$name)$summary_uniport),
                           #summary_Tocris = GeneCard_Symbol_Details(gene.info$name)$summary_Tocris, 
                           summary_Tocris = sub(paste0("\\Tocris\\ \\Summary\\ \\for\\ \\",gene.info$name,"\\ \\Gene"),"",GeneCard_Symbol_Details(gene.info$name)$summary_Tocris),
                           #summary_CIViC = GeneCard_Symbol_Details(gene.info$name)$summary_CIViC,
                           stringsAsFactors = F
  )
}

#gene.view.fun(id = a$ENTREZID[55])
gene.view.ALL = NULL
for (i in na.omit(a$ENTREZID)){
  gene.view.ALL = rbind(gene.view.ALL, gene.view.fun(entrezid = i))
}


### ### ### ### ### ##
### Gene Conection ###
### ### ### ### ### ##

gene.conection.fun = function(id = "ENTREDIZ"){
  gene.conection <<- list(
    geoprofiles = list(entrez_link(dbfrom='gene', id = id,db="geoprofiles")$links$gene_geoprofiles),
    omim = list(entrez_link(dbfrom='gene', id = id,db="omim")$links$gene_omim),
    clinVar = list(entrez_link(dbfrom='gene', id = id,db="clinvar")$links$gene_clinvar)
  )
  names(gene.conection$geoprofiles) <<- id
  names(gene.conection$omim) <<- id
  names(gene.conection$clinVar) <<- id
  
}

gene.conection.fun(id = a$ENTREZID[1])

gene.conection.ALL = list()
for(i in a$ENTREZID){
  gene.conection.fun(id = i)
  gene.conection.ALL[[i]] = gene.conection
}

rm(gene.conection.fun,gene.view.fun,i)

### ### ### ### ### ##
### Gene Expression ###
### ### ### ### ### ##
library(hpar)
gene.expression.fun = function(ENTREZID, ENSEMBL){
  a = getHpa(id = ENSEMBL,hpadata = 'hpaNormalTissue')
  if(nrow(a)>0){
    a = apply(a, 2, function(x) sub(" 1| 2","", x))
  }
  a.1 = getHpa(id = ENSEMBL,hpadata = 'rnaGeneTissue')
  a.1 = merge(unique(a[,c(3,5)]), a.1[,c("Sample","Value")], by.x = "Tissue",by.y = "Sample",all = T)
  
  a.2 = a.1[which(a.1[,"Level"]!="Not detected" |  a.1[,"Value"]>=1),]
  colnames(a.2) = c("Tissue","Protein","mRNA")
  if(nrow(a.2)>0){
    gene.expression <<- data.frame(ENTREZID =ENTREZID ,ENSEMBL = ENSEMBL, a.2)
  } 
}


gene.expression.ALL = NULL
for(i in 1:length(c$ENSEMBL)){
  gene.expression.ALL = rbind(gene.expression.ALL, gene.expression.fun(ENTREZID = c$ENTREZID[i], ENSEMBL = c$ENSEMBL[i]))
}

rm(i, gene.expression.fun)

### ### ### ### ### #
#### Drug Panel #####
### ### ### ### ### #

## ### ### ### #
## Drugs info ##
## ### ### ### #

a =apply(drugbank,2,function(x) tolower(x))
drugs.view = merge(a,drugs_list ,by.x = "Common.name",by.y = "Drug", all.x=F,all.y = T)


## ### ### ### #
## Drugs terms #
## ### ### ### #
term3.2 = c("drug","Anti-Bacterial Agents","Prodrugs","Multidrug-Resistant","Anti-Asthmatic Agents","medication","remedy","cure","antidote","medicine","medicament")
term3.2 = paste0("\\b",paste0(term3.2, collapse = "\\b|\\b"),"\\b")

for(j in "term3.2"){
  a.3 = NULL;a.4=NULL
  for(i in grep(get(j), db$abstract, ignore.case = T, value = F)){
    a.1 = db$abstract[i]
    a.2 = tokenize_sentences(a.1)
    a.3 = rbind(a.3,
                data.frame(pmid = as.character(db$pmid[i]),
                           sentece = paste0(grep(get(j),a.2[[1]],ignore.case = T, value = T),collapse = " [...] ")))
  }
}

drug.terms = a.3

### ### ### ### ### ###
#### Tissue Panel #####
### ### ### ### ### ###

## ### ### ###
## tissues ###
## ### ### ###
library(hpar)
data(hpaNormalTissue)
a = unique(sub(" 1| 2",replacement ="" ,ignore.case = T, x = levels(hpaNormalTissue$Tissue)))
a.3 = NULL;a.4=NULL
for(j in a){
  for(i in grep(paste0("\\b",j,"\\b"), db$abstract, ignore.case = T, value = F)){
    a.1 = db$abstract[i]
    a.2 = tokenizers::tokenize_sentences(a.1)
    a.3 = rbind(a.3,
                data.frame(pmid = as.character(db$pmid[i]),
                           sentece = paste0(grep(j,a.2[[1]],ignore.case = T, value = T),collapse = " [...] "),
                           term = j))
    a.4 = rbind(a.4,data.frame(pmid = as.character(db$pmid[i]), sentence = grep(j,a.2[[1]],ignore.case = T, value = T),
                               term = j))
  }
}
tissue.db = a.3
tissue_list = as.data.frame(table(tissue.db$term),stringsAsFactors = F)
colnames(tissue_list) = c("Tissue","Article number")
tissue_list = tissue_list[order(-tissue_list$`Article number`),]

## ### ### #
## cells ###
## ### ### #
a = unique(sub("cells in | cells", replacement ="" ,ignore.case = T, x = levels(hpaNormalTissue$Cell.type)))
a.3 = NULL;a.4=NULL
for(j in a){
  for(i in grep(paste0("\\b",j,"\\b"), db$abstract, ignore.case = T, value = F)){
    a.1 = db$abstract[i]
    a.2 = tokenizers::tokenize_sentences(a.1)
    a.3 = rbind(a.3,
                data.frame(pmid = as.character(db$pmid[i]),
                           sentece = paste0(grep(j,a.2[[1]],ignore.case = T, value = T),collapse = " [...] "),
                           term = j))
    a.4 = rbind(a.4,data.frame(pmid = as.character(db$pmid[i]), sentence = grep(j,a.2[[1]],ignore.case = T, value = T),
                               term = j))
  }
}

cells.db = a.3
cell_list = as.data.frame(table(cells.db$term),stringsAsFactors = F)
colnames(cell_list) = c("Cell","Article number")
cell_list = cell_list[order(-cell_list$`Article number`),]

rm(a.2,a.3,a.4,b,drug_atomization,drug.fun,gene.expression.fun,gene.conection.fun,gene.view.fun,a,a.1,d1,df_name,i,j,m,n,df,df2,
   one.1,one.2)                    