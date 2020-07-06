abs.date = 
  function(df_name = "Output df name",df.week="list() with PMID and PubDate",df_abstract_db = "Global Abs db"){
  
  for (j in names(df.week$week)){
      id = eval(parse(text = paste0("df.week$week$",j,"$pmid")))
      assign(paste0(df_name,"_",j),  new("Abstracts",
              Journal  = df_abstract_db@Journal[which(df_abstract_db@PMID %in% id)],
              Abstract = df_abstract_db@Abstract[which(df_abstract_db@PMID %in% id)],
              PMID     = df_abstract_db@PMID[which(df_abstract_db@PMID %in% id)])
            )
  }
  
  assign(df_name, eval(parse(text = paste0("list(",paste0(names(df.week$week),"=",df_name,"_",names(df.week$week), collapse = ","),")")))
       ,pos = ".GlobalEnv")
}


