
WorldAffiliation.Week = function(df_name = "Output df name", country_date_pmid = "list created by 'country.date.pmid'")  {

  for (j in paste0(names(get(country_date_pmid)$week))){
  
    a = eval(parse(text = paste0(country_date_pmid,"$week$",j,"[,1:2]")))
      if(length(a$pmid) >= 1){
        df = NULL
        for(i in countr$country.name.en){
          freq <- sapply(gregexpr(i,a$country,ignore.case = T),function(x)if(x[[1]]!=-1) print(1) else 0)
          df = rbind(df,data.frame(Country = i, Freq = sum(freq)))
        }
        assign(paste0(df_name,j), df[order(-df$Freq),])
      } else {
        assign(paste0(df_name,j), data.frame(Country = NA, Freq = NA))
      }
    }
  eval(parse(text = paste0(df_name,"<<- Reduce(function(x, y) merge(x, y, by='Country', all=TRUE),list(",paste0(names(get(country_date_pmid)$week),'=',df_name,names(get(country_date_pmid)$week),collapse = ','),"))"))) #verificar se precisa do get    
  eval(parse(text = paste0("colnames(",df_name,")<<-c('country.affiliation','",paste0(names(get(country_date_pmid)$week),collapse = "','"),"')")))
}


