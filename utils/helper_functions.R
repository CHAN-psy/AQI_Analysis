#資料分析
library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
library(dtplyr)
library(lubridate)
library(glue)

#視覺化
library(ggplot2)
library(plotly)

#資料取得
library(RSQLite)




####資料庫相關函數####
#資料庫參數
db_info <-"Aqi_Sta.sqlite"
tb_info <- "Aqi_Sta"


#### 分析與視覺化 ####
#定義折線圖繪製
Line_Plotlty_ <- function(Raw_Data){

  
  Raw_Data  <- Raw_Data  %>% 
    group_split(pollutant,.keep = TRUE)
  
  plot_list = purrr::imap(Raw_Data,function(data,idx){
    p <-  plot_ly(data, 
                  x = ~datacreationdate, 
                  y = ~aqi_value, 
                  color = ~sitename,
                  split = ~pollutant,
                  showlegend = idx==1,
                  type = 'scatter', 
                  mode = 'lines',
                  customdata = ~sitename,
                  hovertemplate = ~hover_line,
                  legendgroup = ~sitename,
                  name = ~sitename
                  ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "",rangemode = 'tozero')
      )
    return(p)
  }
  )

  p <- subplot(plot_list,nrows = 2,shareX = FALSE, shareY = FALSE,
               margin = 0.05) %>%
    layout(
      showlegend = TRUE,
      legend = list(
      orientation = "h",   # 1. 將圖例設為水平排列
      xanchor = "center",  # 2. 設定圖例的錨點為中心
      x = 0.5,           # 3. 將錨點放置在 X 軸 50% 的位置 (正中央)
      y = -0.2           # 4. 將圖例放置在繪圖區下方，避免遮擋
    ))
   return(p) 
   
}
#定義add_heatmap熱力圖圖繪製
Trace_Heat_Plotly = function(Raw_Data){
  # 2.2. 建立一個空的 plotly 物件作為畫布
  Raw_Data <- Raw_Data %>%
    group_split(sitename)
  
  # 2. 為每個數據子集建立一個 plotly 圖表
  plot_list <- purrr::map(Raw_Data, function(data) {
    
    # 2.2. 建立一個空的 plotly 物件作為畫布
    p <- plot_ly(data = data) %>%
      # 2.3. 使用 add_heatmap 新增圖層，並傳入準備好的矩陣
      add_heatmap(
        x = ~datacreationdate, # x 軸是時間 (欄名)
        y = ~pollutant, # y 軸是站點 (列名)
        z = ~quality,         # z 是濃度矩陣
        type = 'heatmap',
        colorscale = "Viridis",
        showscale = FALSE # 在子圖中先隱藏顏色條
      ) %>%
      layout(
        xaxis = list(title = "", type = "category"), # 將 x 軸設為類別，避免被當成連續數值
        yaxis = list(title = "汙染物", type = "category")
      )
    
    return(p)
  })
  
  # 3. 使用 subplot 組合所有圖表
  p_final <- subplot(
    plot_list,
    nrows = length(plot_list),
    shareX = TRUE,
    titleY = TRUE
  ) %>%
    layout(
      title = "各污染物濃度熱力圖 (add_heatmap 版本)",
      showlegend = FALSE # 熱力圖通常不需要圖例
    )
  
  return(p_final)
}
#定義heatmap熱力圖圖繪製
Heat_Plotlty_ <- function(Raw_Data){
  Raw_Data  <- Raw_Data  %>% group_split(pollutant)
  
  plot_list = purrr::map(Raw_Data,function(data){
    name <- data$pollutant %>% unique() %>% as.character()
    p <-  plot_ly(data, 
                  x = ~datacreationdate, 
                  y = ~sitename, 
                  z = ~quality,
                  type = 'heatmap', 
                  customdata = ~sitename,
                  showlegend = F) %>%
      layout(
        xaxis = list(title = "")
      )
    return(p)
  }
  )
  p <- subplot(plot_list,nrows = length(plot_list),
               shareX = TRUE, 
               titleY = TRUE) %>%
    layout(title = "汙染物按時間的趨勢圖")  
  return(p) 
  
}
#定義小提琴圖繪製
Violin_Plotly_1 <- function(Raw_Data) {
  p <- Raw_Data %>%
    plot_ly(
      x = ~ pollutant,
      y = ~ quality,
      split = ~ pollutant,
      color = ~ sitename,
      type = 'violin',
      points = "all",
      width = 1,
      box = list(visible = TRUE),
      customdata = ~ sitename,
      text = ~ hover_violin,
      hoverinfo = "text"
    )  %>%
    layout(
      xaxis = list(title = "污染物"),
      yaxis = list(
        title = "數值",
        rangemode = 'tozero',
        zeroline = FALSE
      ),
      showlegend = F
    ) 
  return(p)
}
Violin_Plotly <- function(Raw_Data) {
  
  p <- Raw_Data %>%
    plot_ly(
      x = ~ pollutant,
      y = ~ quality,
      color = ~ sitename,
      type = 'violin',
      points = "all",
      box = list(visible = TRUE),
      customdata = ~ sitename,
      text = ~ hover_violin,
      hoverinfo = "text"
    )  %>%
    layout(
      xaxis = list(
        title = "污染物"
      ),
      yaxis = list(
        title = "數值",
        rangemode = 'tozero',
        zeroline = FALSE
      ),
      violinmode="group",
      autoscale = TRUE,
      legend = list(
        orientation = "h",   # 1. 將圖例設為水平排列
        xanchor = "center",  # 2. 設定圖例的錨點為中心
        x = 0.5,           # 3. 將錨點放置在 X 軸 50% 的位置 (正中央)
        y = -0.2           # 4. 將圖例放置在繪圖區下方，避免遮擋
      )
    ) 
  return(p)
}
#定義雷達圖繪製
Radar_Plotlty <- function(Raw_Data) {
  Raw_Data <- Raw_Data %>% filter(datacreationdate == min(datacreationdate) & pollutant != "aqi") %>%
    select(aqi_value,pollutant,sitename,description,pollutant_conc) %>%
    mutate(desc = paste0("<b>",pollutant,"於",sitename,"的數值為:</b><br>",pollutant_conc,"  (AQI=",aqi_value,")<br><br>",description))
  p <- plot_ly(
    data = Raw_Data,
    type = 'scatterpolar',
    mode = "lines+markers",
    fill = 'toself',
    r = ~aqi_value,
    theta = ~pollutant,
    color = ~sitename,
    customdata = ~sitename,
    hovertemplate = ~desc
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          # 範圍最好根據數據動態調整
          range = c(0, max(Raw_Data$quality, na.rm = TRUE) * 1.1)
        )
      ),
      title = paste("雷達圖 @", unique(Raw_Data$datacreationdate)),
      legend = list(
        orientation = "h",   # 1. 將圖例設為水平排列
        xanchor = "center",  # 2. 設定圖例的錨點為中心
        x = 0.5,           # 3. 將錨點放置在 X 軸 50% 的位置 (正中央)
        y = -0.2           # 4. 將圖例放置在繪圖區下方，避免遮擋
      )
    )
  
  return(p)
}
#定義頻率圖繪製
Frequncy_Plotly = function(Raw_Data){
  Raw_Data <- Raw_Data %>% select(sitename,pollutant,bin,bin_n,bin_pct,hover_freq) %>%
    unique() %>% group_split(pollutant)

  
  
  plot_list = purrr::imap(Raw_Data,function(data,idx){
    
  p <- plot_ly(
    data = data,
    type = "bar",
    x = ~bin,
    y = ~bin_n ,
    color = ~sitename,
    split = ~pollutant,
    showlegend = (idx == 1),
    customdata = ~sitename,
    hovertemplate  = ~hover_freq,
    hoverinfo = 'skip',
    legendgroup = ~sitename,
    name = ~sitename
  )  %>% 
    layout(
    xaxis = list(title = ""),
    yaxis = list(title = "", zeroline = FALSE,
                 rangemode = 'tozero'),
    barmode = "group",
    hovermode = 'closest'
  )
})
  p <- subplot(plot_list,nrows = 2,shareX = FALSE, shareY = FALSE,
               margin = 0.05) %>%
      layout(
        showlegend = TRUE,
        legend = list(
               orientation = "h",   # 1. 將圖例設為水平排列
               xanchor = "center",  # 2. 設定圖例的錨點為中心
               x = 0.5,           # 3. 將錨點放置在 X 軸 50% 的位置 (正中央)
               y = -0.2           # 4. 將圖例放置在繪圖區下方，避免遮擋
             ))
  
  return(p)
}
#定義地形圖繪製
Topographic_Plotly = function(Raw_Data){
  sf_data = sf::st_simplify(twmap::tw_county, dTolerance = 0.02)
  map_bounds = calculate_map_bounds(Raw_Data)
  
  p <- plot_ly() %>%
    add_sf(
      data = sf_data,
      inherit = FALSE,
      line = list(color = "white", width = 1),
      fillcolor = "#F0F0F0",
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    add_markers(
      data = Raw_Data,
      x = ~long,
      y = ~lati,
      customdata = ~sitename,
      color = ~sitename,
      marker = list(size = 15, line = list(width = 1, color = "white")),
      text = ~paste0("站點: ", sitename)
    ) %>%
    layout(
      xaxis = list(range = c(map_bounds[[2]], map_bounds[[1]]), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(range = c(map_bounds[[4]], map_bounds[[3]]), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      showlegend = TRUE,
      legend = list(
        orientation = "h",   # 1. 將圖例設為水平排列
        xanchor = "center",  # 2. 設定圖例的錨點為中心
        x = 0.5,           # 3. 將錨點放置在 X 軸 50% 的位置 (正中央)
        y = -0.2           # 4. 將圖例放置在繪圖區下方，避免遮擋
      )
      ,
      plot_bgcolor = "white",
      paper_bgcolor = "white" 
    ) 
  return(p)
}
calculate_map_bounds <- function(site_data, target_ratio = 1/2.7) {
  if (nrow(site_data) == 1) {
    buffer <- 0.05
    return(list(
      xmax = site_data$long + buffer, xmin = site_data$long - buffer,
      ymax = site_data$lati + buffer, ymin = site_data$lati - buffer
    ))
  }
  
  ranges <- c(diff(range(site_data$long)), diff(range(site_data$lati)))
  buffers <- pmax(ranges * 0.15, 0.02)
  
  temp_bounds <- c(
    max(site_data$long) + buffers[1], min(site_data$long) - buffers[1],
    max(site_data$lati) + buffers[2], min(site_data$lati) - buffers[2]
  )
  
  actual_ranges <- c(temp_bounds[1] - temp_bounds[2], temp_bounds[3] - temp_bounds[4])
  
  if (actual_ranges[2] / actual_ranges[1] > target_ratio) {
    expand <- (actual_ranges[2] / target_ratio - actual_ranges[1]) / 2
    temp_bounds[1:2] <- temp_bounds[1:2] + c(expand, -expand)
  } else {
    expand <- (actual_ranges[1] * target_ratio - actual_ranges[2]) / 2
    temp_bounds[3:4] <- temp_bounds[3:4] + c(expand, -expand)
  }
  
  return(list(xmax = temp_bounds[1], xmin = temp_bounds[2], 
              ymax = temp_bounds[3], ymin = temp_bounds[4]))
}
#AQI轉換
