Open_Database_ <-  function(db_info){
  conn <- NULL
  
  #try-catch
  conn <- tryCatch({
    driver <- RSQLite::SQLite()
    DBI::dbConnect(driver,paste0("data/",db_info))
  },error = function(e){
    message("資料庫連接錯誤:", e$message)
    NULL
  })
  
  if(!is.null(conn)){
    message("成功連結到資料庫",db_info)
  }
  
  return(conn)
}
Close_Database_ <-  function(conn){
  if(!is.null(conn) && DBI::dbIsValid(conn)){
    tryCatch({
      DBI::dbDisconnect(conn)
      message("資料庫已關閉")
    },error = function(e){
      message("資料庫關閉發生錯誤:",e$message())
    })
  }
}
Get_Value <- function(db_info, condition = list("",NA,NA)){
  db = Open_Database_(db_info)
  on.exit({
    Close_Database_(db)
  })
  
  
  name = dbGetQuery(db,"SELECT sitename FROM Geo") %>% unlist(.,use.names = F)
  
  if(!all(is.element(condition[[1]],name))){
    condition[[1]] =  "花蓮"
  }else if(any(condition[[1]] == "all")){
    condition[[1]] = name
  }
  if(is.na(condition[[2]])){
    condition[[2]] =  (now() %m-% months(3)) %>% format(.,"%Y-%m-%d %H:%M")
  }
  if(is.na(condition[[3]])){
    condition[[3]] =  now() %>% format(.,"%Y-%m-%d %H:%M")
  }
  names(condition) <- c("sitename","upper","lower")
  
  Sql_defualt = glue::glue_sql("
  SELECT Aqi_Sta.*, Geo.longitude AS long, Geo.latitude AS lati 
  FROM Geo  
  LEFT JOIN Aqi_Sta 
  ON Geo.sitename = Aqi_Sta.sitename
  WHERE Aqi_Sta.sitename in ( {sitename*} ) AND Aqi_Sta.datacreationdate BETWEEN {upper} AND {lower}
  " ,.con = db,sitename = condition[[1]], upper = condition[[2]], lower = condition[[3]])
  
  # 3. 執行查詢
  for(i in 1:length(Sql_defualt)){
    if(i == 1){
      Data =  dbGetQuery(db, Sql_defualt[[i]])
    }else{
      Data =  dbGetQuery(db, Sql_defualt[[i]]) %>% rbind(Data,.)
    }
  }
  
  return(Data)
}

ref_data <- function(){
  res = data.frame(
    Group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7),
    AQI_Index = c(0, 50, 51, 100, 101, 150, 151, 200, 201, 300, 301, 400, 401, 500),
    Statement = rep(c("良好","普通","對敏感族群不健康","對所有族群不健康","非常不健康","危害"),c(rep(2,5),4)),
    Boundary = rep(c("lower", "upper"),7),
    o3_8hr  = c(0, 0.054, 0.055, 0.07, 0.071, 0.085, 0.086, 0.105, 0.106, 0.2, NA, NA, NA, NA)*1000,
    o3 = c(NA, NA, NA, NA, 0.101, 0.134, 0.135, 0.204, 0.205, 0.404, 0.405, 0.504, 0.505, 0.604)*1000,
    pm2.5  = c(0, 12.4, 12.5, 30.4, 30.5, 50.4, 50.5, 125.4, 125.5, 225.4, 225.5, 325.4, 325.5, 500.4),
    pm10 = c(0, 30, 31, 75, 76, 190, 191, 354, 355, 424, 425, 504, 505, 604), 
    co_8hr = c(0, 4.4, 4.5, 9.4, 9.5, 12.4, 12.5, 15.4, 15.5, 30.4, 30.5, 40.4, 40.5, 50.4),
    so2 = c(0, 8, 9, 65, 66, 160, 161, 304, 305, 604, 605, 804, 805, 1004),
    so2_avg = c(0, 8, 9, 65, 66, 160, 161, 304, 305, 604, 605, 804, 805, 1004),
    no2 = c(0, 21, 22, 100, 101, 360, 361, 649, 650, 1249, 1250, 1649, 1650, 2049), 
    aqi = c(0, 50, 51, 100, 101, 150, 151, 200, 201, 300, 301, 400, 401, 500),
    
    
    stringsAsFactors = FALSE
  )
  res <-  res %>% 
    pivot_longer(cols = -c(Group, AQI_Index, Boundary,Statement),names_to ="Pollutant",values_to = "Quality") %>%
    pivot_wider(names_from = Boundary, values_from = c(Quality,AQI_Index)) %>%
    filter(!is.na(Quality_lower) | !is.na(Quality_upper))
  
  return(res)
}
desc_data <-function(){
  data.frame(
    pollutant  = c("pm2.5",
                   'pm10',
                   'o3',
                   'no',
                   'no2',
                   'nox',
                   'so2',
                   'co',
                   'co_8hr',
                   'o3_8hr',
                   'pm2.5_avg',
                   'pm10_avg',
                   'so2_avg',
                   'aqi'),
    description = c("<b>PM2.5的來源：</b><br>道路、施工、工業、境外污染<br>以24小時移動平均作為空氣副指標",
                    "<b>PM10的來源：</b><br>道路、施工、工業、境外污染<br>以24小時移動平均作為空氣副指標",
                    "<b>O3的來源：</b><br>其他汙染物衍生的光化學煙霧<br>通常以8小時移動平均作為空氣副指標<br>當濃度過高時則採每小時平均",
                    "<b>NOX的來源：</b><br>燃料燃燒後氮化物氧化產生<br>以NO2的每小時移動平均作為空氣副指標",
                    "<b>NOX的來源：</b><br>燃料燃燒後氮化物氧化產生<br>以NO2的每小時移動平均作為空氣副指標",
                    "<b>NOX的來源：</b><br>燃料燃燒後氮化物氧化產生<br>以NO2的每小時移動平均作為空氣副指標",
                    "<b>SO2的來源：</b><br>火山或燃料中的硫燃燒<br>以SO2的每小時移動平均作為空氣副指標<br>當濃度過高時則採24小時移動平均",
                    "<b>CO2的來源：</b><br>生物活動或燃料的不完全燃燒<br>以CO2的8小時移動平均作為空氣副指標",
                    "<b>CO2的來源：</b><br>生物活動或燃料的不完全燃燒<br>以CO2的8小時移動平均作為空氣副指標",
                    "<b>O3的來源：</b><br>其他汙染物衍生的光化學煙霧<br>通常以8小時移動平均作為空氣副指標<br>當濃度過高時則採每小時平均",
                    "<b>PM2.5的來源：</b><br>道路、施工、工業、境外污染<br>以24小時移動平均作為空氣副指標",
                    "<b>PM10的來源：</b><br>道路、施工、工業、境外污染<br>以24小時移動平均作為空氣副指標",
                    "<b>SO2的來源：</b><br>火山或燃料中的硫燃燒<br>以SO2的每小時移動平均作為空氣副指標<br>當濃度過高時則採24小時移動平均",
                    "<b>AQI的計算：</b><br>計算各污染物的副指標並取當下的最大值作為空氣品質指標")
  )
}
val_data <- function(){
  data.frame(
    pollutant     = c("pm10", "pm2.5"),
    roc_standard  = c(75, 30),
    who_standard  = c(45, 15),
    stringsAsFactors = FALSE
  )
}

Trans_to_aqi = function(num, type){
  ref <- ref_data()
  
  # 保留原始數值用於計算，但建立rounded版本用於display
  input <- data.frame(
    Quality_original = num,  # 保留原始值
    Quality_joinby = case_when(
      type %in% c("pm2.5","co_8hr") ~ round(num, 1),
      TRUE ~ round(num, 0)
    ),
    Pollutant = type
  )
  
  # 使用原始數值進行join
  match <- input %>% 
    left_join(ref, by = join_by(
      Pollutant, 
      Quality_joinby >= Quality_lower, 
      Quality_joinby <= Quality_upper
    ))
  
  res <- match %>% 
    mutate(
      sub_aqi = ((AQI_Index_upper - AQI_Index_lower) / (Quality_upper - Quality_lower)) * 
        (Quality_original - Quality_lower) + AQI_Index_lower,
      sub_aqi = ifelse(is.na(sub_aqi), 500, round(sub_aqi))
    )
  
  return(res$sub_aqi)
}

create_bins <- function(x) {
  breaks <- c(seq(0, 200, by = 10), Inf)
  upper <- c(seq(9, 199, by = 10), Inf)
  labels <- paste0(seq(0, 180, by = 10), "~", upper)
  
  cut(
    x,
    breaks = breaks,
    labels = labels,
    right = FALSE,
    include.lowest = TRUE
  ) %>% factor(.,levels = labels)
}

Base_Data = function(df){
  available_pollutants = desc_data()$pollutant[!desc_data()$pollutant %in% c( "pm2.5_avg","pm10_avg", "nox","no","co" )]
  
  base_data <- df %>%
    select(c(sitename, datacreationdate, all_of(available_pollutants))) %>%
    mutate(
      datetime = ymd_hm(datacreationdate),
      date = as.Date(datetime),
      hour = hour(datetime)
    ) %>%
    pivot_longer(
      cols = -c(sitename, datacreationdate,datetime, date, hour),
      names_to = "pollutant",
      values_to = "quality"
    ) %>%
    filter(!is.na(quality) | quality < 0) %>%
    as.data.frame()
  return(base_data)
}

Processed_Data = function(df){
  processed_data <- df %>%
    group_by(sitename, date, pollutant) %>%
    reframe(
      quality = case_when(
        pollutant %in% c("pm2.5", "pm10") ~ mean(quality, na.rm = TRUE),
        pollutant %in% c("o3_8hr","co_8hr","so2_avg") ~ {
          target_data = quality[hour >= 7 & hour <= 23]
          if(length(target_data) > 0) max(target_data, na.rm = TRUE) else NA_real_
        },
        pollutant %in% c("o3","so2","no2","aqi") ~ max(quality, na.rm = TRUE),
        TRUE ~ mean(quality, na.rm = TRUE)
      )
    ) %>%
    filter(!is.na(quality)) %>%
    # 計算AQI副指標
    mutate(aqi_value = Trans_to_aqi(quality, pollutant)) %>%
    # 特殊邏輯處理
    group_by(sitename, date) %>%
    mutate(
      aqi_value = case_when(
        pollutant == "o3_8hr" & aqi_value > 300 ~ {
          o3_aqi <- filter(cur_data(), pollutant == "o3")$aqi_value
          if(length(o3_aqi) > 0 ) max(300, o3_aqi, na.rm = TRUE) else aqi_value
        },
        pollutant == "so2" & aqi_value > 150 ~ {
          so2_avg_aqi <- filter(cur_data(), pollutant == "so2_avg")$aqi_value
          if(length(so2_avg_aqi) > 0 ) max(150, so2_avg_aqi, na.rm = TRUE) else aqi_value
        },
        pollutant == "o3" & quality < 101 ~ NA_real_,
        TRUE ~ aqi_value
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(aqi_value) & !pollutant %in% c("o3","so2_avg")) %>%
    rename(datacreationdate = date) %>%
    unique()
  return(processed_data)
}



# 1-6. 計算統計量 & 分箱頻率
compute_stats <- function(df) {
  df %>% 
    group_by(sitename, pollutant) %>% 
    mutate(
      median_aqi = median(aqi_value, na.rm = TRUE),
      q1_aqi     = quantile(aqi_value, .25, na.rm = TRUE)%>%unname(),
      q3_aqi     = quantile(aqi_value, .75, na.rm = TRUE)%>%unname(),
      mean_aqi   = mean(aqi_value, na.rm = TRUE),
      total_n    = n(),
      outlier_n  = sum(aqi_value < (q1_aqi - 1.5*(q3_aqi-q1_aqi)) |
                         aqi_value > (q3_aqi + 1.5*(q3_aqi-q1_aqi))),
      outlier_pct= outlier_n/total_n*100,
      bin        = create_bins(aqi_value)
    ) %>% 
    group_by(sitename, pollutant, bin) %>% 
    mutate(
      bin_n     = n(),
      bin_pct   = bin_n/total_n*100
    ) %>% 
    ungroup() 
}

#— 2. 主函式：將以上模組串起來 —#

Trans_function <- function(raw_data) {
  raw_data %>%
    compute_stats %>% 
    left_join(
      desc_data(),
      by = "pollutant"
    ) %>%
    mutate(
      pollutant_conc  = case_when(
        pollutant %in% c("pm2.5","pm10")       ~ glue("{round(quality,1)} μg/m³"),
        pollutant %in% c("o3","o3_8hr")  ~ glue("{round(quality,3)} ppm"),
        pollutant %in% c("co_8hr","co")        ~ glue("{round(quality,1)} ppm"),
        pollutant %in% c("no2","so2","so2_avg")        ~ glue("{round(quality)} ppb"),
        TRUE                                   ~ as.character(round(quality,2))
      )
    )%>% 
    mutate(
      hover_violin = paste(
        "<b>站點：</b>", sitename, "<br>",
        "<b>污染物：</b>", pollutant, "<br>",
        "<b>中位數：</b>", median_aqi, "<br>",
        "<b>Q1：</b>", q1_aqi, "<br>",
        "<b>Q3：</b>", q3_aqi, "<br>",
        "<b>極端值：</b>", outlier_n, " (", round(outlier_pct, 1), "%)<br>",
        "<b>平均：</b>", round(mean_aqi, 1),
        sep = ""
      ),
      hover_freq = paste(
        "<b>站點：</b>", sitename, "<br>",
        "<b>污染物：</b>", pollutant, "<br>",
        "<b>", sitename, "於[", bin, "]數量為：</b>", bin_n, " (", round(bin_pct, 1), "%)",
        sep = ""
      ),
      hover_line = paste(
        "<b>站點：</b>", sitename, "<br>",
        "<b>污染物：</b>", pollutant, "<br>",
        "<b>AQI(濃度)：</b> ", aqi_value, " (", pollutant_conc, ")<br><br>",
        sep = ""
      )
    ) %>%
    as.data.frame()
}
Trans_to_valuebox <- function(currentsitename,raw_data){
  #要輸出這些東西 c("世衛超標天數","台規超標天數","小時中位數","主要汙染物")
  
  hourmedium <- raw_data %>% 
    filter(sitename == currentsitename & pollutant == "aqi") %>%
    select(median_aqi) %>%
    unique()
  
  PM_data <-  lazy_dt(raw_data) %>% 
    select(sitename,pollutant, quality) %>%
    filter(sitename == currentsitename)  %>% 
    filter(pollutant %in% c("pm2.5","pm10")) %>%
    left_join(.,val_data(),by = "pollutant") %>%
    mutate(
      ROC = case_when(roc_standard < quality ~ 1,
                      TRUE ~ 0),
      WHO = case_when(who_standard < quality ~ 1,
                      TRUE ~ 0)
    ) %>% 
    group_by(pollutant) %>%
    summarise(
      higher_than_roc = round(sum(ROC),1),
      higher_than_who =  round(sum(WHO),1),
      higher_than_roc_pct =  round(sum(ROC)/n()*100,1),
      higher_than_who_pct =  round(sum(WHO)/n()*100,1),
    ) %>%
    transmute(pollutant,
              roc_text = sprintf("%s天(%s%s)",higher_than_roc,higher_than_roc_pct,"%"),
              who_text = sprintf("%s天(%s%s)",higher_than_who,higher_than_who_pct,"%")) %>%
    as.data.frame()
  
  main_pollutant <- lazy_dt(raw_data) %>%
    filter(sitename == currentsitename & pollutant != "aqi") %>%
    select(datacreationdate, pollutant, aqi_value) %>%
    group_by(datacreationdate) %>%
    filter(aqi_value == max(aqi_value,na.rm = TRUE)) %>%
    group_by(pollutant) %>%
    summarise(
      pollutant,
      count_pollutant = n()
    ) %>% 
    unique() %>%
    ungroup() %>%
    filter(rank(desc(count_pollutant),ties.method="first") <= 2) %>%
    arrange(desc(count_pollutant)) %>%
    as.data.frame()
  
  slide1 = PM_data$roc_text
  slide2 = PM_data$who_text 
  slide3 = hourmedium$median_aqi
  slide4 = sprintf("%s  (%s)",main_pollutant$pollutant[1],main_pollutant$pollutant[2])
  
  return(c(slide1[2],slide2[2],slide1[1],slide2[1],slide3,slide4))
}

