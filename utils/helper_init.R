#重建資料庫
Create_New_Dataset_  <-  function(Csv_Dir = paste0(getwd(),"/AQI_HIS/"),db_info,tb_info){
  db = Open_Database_(db_info)
  on.exit(Close_Database_(db))
  #建立DB
  dbExecute(db,
            sprintf("DROP TABLE IF EXISTS %s",tb_info)
  )
  
  #取得所有AQI資料的CSV檔案名
  Data_Name = list.files(path = Csv_Dir)
  
  
  #自動建立路徑並加入限制（若有同站點同時間點的資料無法存入）
  basic_create_sql  <-  read.csv(paste0(Csv_Dir,Data_Name[1]),sep = ",", header = T,fileEncoding = "BIG5") %>%
    DBI::sqlCreateTable(db,tb_info,.)
  if(tb_info == "Aqi_His"){
    constraint_sql <-  ",\n CONSTRAINT SITEANDTIME UNIQUE (sitename, datacreationdate)"
  }else{
    constraint_sql <-  ""
    
  }
  modified_create_sql <- sub("\\)[[:space:]]*$", paste0(constraint_sql, "\n)"), basic_create_sql)
  
  #建立DB
  dbExecute(db, modified_create_sql)
  
  #批次存入資料進入資料庫
  for(i in Data_Name){
    sprintf("已載入資料: %s",i) %>% message()
    
    read.csv(paste0(Csv_Dir,i),sep = ",", header = T,fileEncoding = "BIG5") %>%
      dbWriteTable(db, tb_info, ., overwrite = FALSE,append = TRUE, row.names = FALSE)
  }
}
Create_Related_Dataset_  <-  function(db_info_attach,db_info_restore,tb_info_attach){
  db_a = Open_Database_(db_info_attach)
  db_r = Open_Database_(db_info_restore)
  on.exit({
    dbClearResult(Query)
    Close_Database_(db_a)
    Close_Database_(db_r)
  })
  tb_info = list()
  
  #建立Geo table
  tb_info[[1]] = c(
    "Geo",
    
    "Create Table Geo(
              sitename varchar(255),
              county varchar(255),
              longitude int,
              latitude int
              )",
    sprintf("SELECT DISTINCT sitename, county, longitude, latitude FROM %s 
  WHERE (longitude != \"nan\" OR latitude != \"nan\") AND latitude > 21
  GROUP BY sitename",tb_info_attach),
    "INSERT OR IGNORE INTO Geo (sitename, county, longitude, latitude) VALUES")
  
  tb_info[[2]]  = c("TimeRange",
                    "Create Table TimeRange(
              datacreationdate int
              )",
                    sprintf("SELECT DISTINCT datacreationdate FROM %s",tb_info_attach),
                    "INSERT OR IGNORE INTO TimeRange (datacreationdate) VALUES") 
  
  for(i in 1:2){
    dbExecute(db_r,
              sprintf("DROP TABLE IF EXISTS %s",tb_info[[i]][1])
    )
    dbExecute(db_r, tb_info[[i]][2])
    
    data = dbGetQuery(db_a,tb_info[[i]][3]) %>% unname()
    
    Query <- paste0(tb_info[[i]][4], "(", paste(rep("?",dim(data)[2]),collapse = ","), ")")
    Query <- dbSendStatement(db_r,Query)
    
    dbBind(Query,data)
    
    message(sprintf("已改變 %s 行",dbGetRowsAffected(Query)))
  }
  
}
Store_csv <- function(db_info,sitename = NULL, upper = ymd_hm("2018-01-01 00:00"), lower = ymd_hm(format(now(),"%Y-%m-%d %H:%M"))){

  if(is.null(sitename)){
    sitename = "all"
  }
  
  Range_spilt = data.frame(Star_Time = upper, End_Time = lower) %>% 
    rowwise() %>%
    mutate(rn = list(seq(Star_Time,End_Time,dyears(1)))) %>%
    unnest(rn) %>%
    group_by(Star_Time) %>%
    mutate(rn2 = lead(rn,default = max(End_Time))) %>%
    transmute(Star_Time=rn,End_Time=rn2) %>%
    mutate(Star_Time = format(Star_Time,"%Y-%m-%d %H:%M"),End_Time = format(End_Time,"%Y-%m-%d %H:%M"))
  
  purrr::walk2(.x = Range_spilt$Star_Time,
               .y = Range_spilt$End_Time,
               function(x,y){
                 name = paste0(year(x),month(x),"_",year(y),month(y),".csv")
                 Get_Value(db_info,list("all",x,y)) %>%
                   lazy_dt() %>%
                   Base_Data() %>% 
                   Processed_Data %>%
                   as.data.frame() %>%
                   write.csv(.,file = paste0("data/sta_csv/",name),fileEncoding ="BIG5",row.names = FALSE)
  })
}
