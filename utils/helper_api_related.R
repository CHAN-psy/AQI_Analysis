library(glue)
library(DBI)
library(lubridate)
library(tidyr)
library(dplyr)

#抓取最新資料
Get_Lasted_Data <-  function(time_range = c(NA,NA), type = "his"){
  #選擇Current / Historical Data
  api_type <-  switch(type,
                      his = 488,
                      cur = 432,
                      NULL
  )
  if(is.null(api_type)){
    stop("type輸入錯誤，請再確認type為his或cur")
  }
  
  time_range = gsub(" ","%20",time_range)
  
  Upper <-  if(is.na(time_range[1])){""}else{sprintf("datacreationdate,GR,%s",time_range[1])}
  Lower <-  if(is.na(time_range[2])){""}else{sprintf("datacreationdate,LE,%s",time_range[2])}
  
  time <- if(any(c(Upper,Lower) == "")){""}else{paste(Upper,Lower,sep = "%7C")}
  
  #串接API並調整offset和type
  Url <- "https://data.moenv.gov.tw/api/v2/aqx_p_488?format=csv&language=zh&api_key=4cc0f742-f7ac-48bd-8363-7f970e7fc2b4&filters=%s" %>%
    sprintf(.,time)
  
  #解析資料並存成data.table
  Raw_Data  <-  read.csv(Url,header = T) 
  return(Raw_Data)
}
TimeChecker_  <-  function(db,tb_info){
  
  #呼叫db並取得時間
  db_times_df <- dbGetQuery(db,
                            paste0("SELECT DISTINCT strftime('%Y-%m-%d %H:%M', datacreationdate) AS datacreationdate FROM ", tb_info)
  )
  
  # --- 2. 建立邊界時間的 data.frame ---
  boundary_times_df <- data.frame(
    datacreationdate = c(
      "2016-11-25 01:00", # 固定的全局起始時間
      format(now(), "%Y-%m-%d %H:%M") # 當前時間
    )
  )
  
  # --- 3. 【關鍵修改】使用 bind_rows 進行穩健的合併 ---
  # bind_rows 會正確地將它們合併成一個單欄的 data.frame
  Db_time <- bind_rows(db_times_df, boundary_times_df) %>%
    # 確保沒有重複的時間點
    distinct()
  
  
  Range = Db_time %>% 
    arrange(datacreationdate) %>%
    mutate(datacreationdate = ymd_hm(datacreationdate),Time_lag = lag(datacreationdate)) %>% 
    filter(as.numeric(difftime(datacreationdate, Time_lag, units = 'hours')) > 1L) %>%
    transmute(Star_Time = Time_lag, End_Time = datacreationdate) 
  
  Range_spilt = Range %>% 
    rowwise() %>%
    mutate(rn = list(seq(Star_Time,End_Time,dhours(11)))) %>%
    unnest(rn) %>%
    group_by(Star_Time) %>%
    mutate(rn2 = lead(rn,default = max(End_Time))) %>%
    transmute(Star_Time=rn,End_Time=rn2)
  
  
  #回覆T/F
  if(dim(Range)[1] == 0){
    message("取得連續時間，沒有資料需要更新")
    return(Range_spilt)
  }else{
    sprintf('未取得連續時間，區間缺失有:%s筆，須補足:%s筆',dim(Range)[1],dim(Range_spilt)[1]) %>%
      message()
    return(Range_spilt)
  }
}
#連續取得資料直到連續性符合
ContinuousDataCrawler  <-  function(db_info,tb_info){
  #
  db <-  Open_Database_(db_info)
  on.exit({
    dbClearResult(Query)
    Close_Database_(db)
  })
  #
  
  #計算datacreationdate的空格，並製作idx
  Idx <-  TimeChecker_(db,tb_info)
  
  #資料庫操作參數化
  Column_Names <- dbListFields(db, tb_info) %>% unlist
  
  placeholders <- paste(rep("?", length(Column_Names)), collapse = ", ")
  
  query_part1 <- glue::glue_sql(
    "INSERT OR IGNORE INTO {`tb_info`} ({`Column_Names`*}) VALUES ",
    .con = db
  )
  
  query_string <- paste0(query_part1, "(", placeholders, ")")
  
  Query <- dbSendStatement(db,query_string)
  
  #透過idx取得所有區間的資料
  for(i in 1:nrow(Idx)){
    Index = c(Idx[[1]][i],Idx[[2]][i])
    Raw_Data <-  Get_Lasted_Data(Index) %>%
      unique() %>% unname()
    #如果Raw_Data抓不到資料
    #1.記錄起來以後不掃描這個區域
    #2.返回空值則例外處理
    sprintf("第%s筆數據已抓取, dim = (%s,%s)",i,dim(Raw_Data)[1],dim(Raw_Data)[2]) %>% message()
    if(nrow(Raw_Data) != 0){
      dbBind(Query,Raw_Data) 
      rows_i <- DBI::dbGetRowsAffected(Query)
      message(sprintf(" → 實際寫入 %d 列", rows_i))
    }
  }
  
  if (nrow(Idx) > 0) {
    tryCatch({
      message("寫入資料庫")
      DBI::dbCommit(db)
      message("資料已成功寫入")
    },error = function(e){
      message("無資料需要寫入")
      print(e$message)
    }
    )
  }
  
  return()
}