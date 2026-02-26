# =============================================================================
# utils/helper_functions.R
# 視覺化繪圖函數
# 套件載入統一由 app.R 處理
# =============================================================================

# --- 資料庫參數 ---
db_info <-"Aqi_Sta.sqlite"
tb_info <- "Aqi_Sta"




# =============================================================================
# 視覺化繪圖函數
# =============================================================================

#'折線圖
#'
#' @param raw_data 資料框
#' @param arg      單位："aqi"（副指標）或其他（濃度）
#' @param arg2     要顯示的污染物向量
line_plotly <- function(raw_data,arg = "aqi", arg2 = "aqi"){
    
    y_var <- if (arg == "aqi") "aqi_value" else "quality"
    title <- if (arg == "aqi") "AQI副指標" else "濃度"
    len = if(length(arg2) < 5) 1 else 2
    
    raw_data <- raw_data %>% 
      select(sitename,pollutant,datacreationdate,y_var,hover_line) %>%
      filter(pollutant %in% arg2) %>% 
      group_split(pollutant)
    
    plot_list = purrr::imap(raw_data,function(data,idx){
      titleY = data$pollutant %>% unlist %>% unique()
  
      p <-  plot_ly(data, 
                    x = ~datacreationdate, 
                    y = data[[y_var]], 
                    color = ~sitename,
                    showlegend = idx==1,
                    type = 'scatter', 
                    mode = 'lines',
                    customdata = ~sitename,
                    hovertemplate = ~hover_line,
                    legendgroup = ~sitename,
                    name = ~sitename
                    ) %>%
        layout(
          xaxis = list(title = "",
                       tickfont = list(size = 10)),
          yaxis = list(title = titleY,rangemode = 'tozero')
        )
      return(p)
    }
    )
  
    p <- subplot(plot_list,nrows = len,shareX = FALSE, shareY = FALSE, titleY = TRUE, 
                 margin = c(0.02,0.02,0.09,0.02)) %>%
      layout(
        title = list(
          text = sprintf("各污染物的%s趨勢",title), # 您想要顯示的總標題文字
          y = 0.99, # 標題的Y軸位置 (0到1，1為最頂部)
          x = 0.5,  # 標題的X軸位置 (0到1，0.5為置中)
          xanchor = 'center', # X軸錨點，設為center確保置中
          yanchor = 'top'     # Y軸錨點，設為top確保標題文字從上方開始對齊
        ),
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
#'熱力圖（add_heatmap 版本）
trace_heat_plotly <- function(raw_data){
  # 2.2. 建立一個空的 plotly 物件作為畫布
  raw_data <- raw_data %>%
    group_split(sitename)
  
  # 2. 為每個數據子集建立一個 plotly 圖表
  plot_list <- purrr::map(raw_data, function(data) {
    
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
heat_plotly <- function(raw_data){
  raw_data  <- raw_data  %>% group_split(pollutant)
  
  plot_list = purrr::map(raw_data,function(data){
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
#'小提琴圖（簡易版，不含單位切換）
violin_plotly_simple <- function(raw_data) {
  p <- raw_data %>%
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
#'小提琴圖
#'
#' @param raw_data 資料框
#' @param arg      單位
#' @param arg2     污染物向量
violin_plotly <- function(raw_data,arg = "aqi", arg2 = "aqi") {
  
  y_var <- if (arg == "aqi") "aqi_value" else "quality"
  title <- if (arg == "aqi") "AQI副指標" else "濃度"
  
  raw_data <- raw_data %>% 
    filter(pollutant %in% arg2)
  
  p <- 
    plot_ly(
      data = raw_data,
      x = ~ pollutant,
      y = raw_data[[y_var]],
      color = ~ sitename,
      type = 'violin',
      points = "all",
      box = list(visible = TRUE),
      customdata = ~ sitename,
      text = ~ hover_violin,
      hoverinfo = "text"
    )  %>%
    layout(
      title = list(
        text = sprintf("各污染物的%s趨勢",title), # 您想要顯示的總標題文字
        y = 1, # 標題的Y軸位置 (0到1，1為最頂部)
        x = 0.5,  # 標題的X軸位置 (0到1，0.5為置中)
        xanchor = 'center', # X軸錨點，設為center確保置中
        yanchor = 'top'     # Y軸錨點，設為top確保標題文字從上方開始對齊
      ),
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
#'雷達圖
radar_plotly <- function(raw_data) {
  raw_data <- raw_data %>% filter(datacreationdate == min(datacreationdate) & pollutant != "aqi") %>%
    select(aqi_value,pollutant,sitename,description,pollutant_conc) %>%
    mutate(desc = paste0("<b>",pollutant,"於",sitename,"的數值為:</b><br>",pollutant_conc,"  (AQI=",aqi_value,")<br><br>",description))
  p <- plot_ly(
    data = raw_data,
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
          range = c(0, max(raw_data$quality, na.rm = TRUE) * 1.1)
        )
      ),
      title = paste("雷達圖 @", unique(raw_data$datacreationdate)),
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
#'頻率長條圖
#'
#' @param raw_data 資料框
#' @param arg      單位
#' @param arg2     污染物向量
frequency_plotly <- function(raw_data,arg = "aqi", arg2 = "aqi"){
  
  y_var <- if (arg == "aqi") "aqi" else "quality"
  title <- if (arg == "aqi") "AQI副指標" else "濃度"
  var_raw <- c(sprintf("bin_%s",y_var),
               sprintf("bin_%s_n",y_var),
               sprintf("hover_freq_%s",y_var))
  len = if(length(arg2) < 5) 1 else 2
  
  raw_data <- raw_data %>% 
    select(sitename,pollutant,all_of(var_raw)) %>%
    unique() %>% filter(pollutant %in% arg2) %>% 
    group_split(pollutant)
  

  
  
  plot_list = purrr::imap(raw_data,function(data,idx){
  titleY = data$pollutant %>% unlist %>% unique()
    
  p <- plot_ly(
    data = data,
    type = "bar",
    x = data[[var_raw[1]]],
    y = data[[var_raw[2]]],
    color = ~sitename,
    split = ~pollutant,
    showlegend = (idx == 1),
    customdata = ~sitename,
    hovertemplate  = ~data[[var_raw[3]]],
    hoverinfo = 'skip',
    legendgroup = ~sitename,
    name = ~sitename
  )  %>% 
    layout(
    xaxis = list(title = "",
                 tickfont = list(size = 10)),
    yaxis = list(title = titleY, zeroline = FALSE,
                 rangemode = 'tozero'),
    barmode = "group",
    hovermode = 'closest'
  )
})
  p <- subplot(plot_list,nrows = len,shareX = FALSE, shareY = FALSE, titleY = TRUE,
               margin = c(0.02,0.02,0.09,0.05)) %>%
      layout(
        title = list(
          text = sprintf("各污染物的%s趨勢",title), # 您想要顯示的總標題文字
          y = 0.99, # 標題的Y軸位置 (0到1，1為最頂部)
          x = 0.5,  # 標題的X軸位置 (0到1，0.5為置中)
          xanchor = 'center', # X軸錨點，設為center確保置中
          yanchor = 'top'     # Y軸錨點，設為top確保標題文字從上方開始對齊
        ),
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
#'地形圖（台灣地圖 + 站點標記）
topographic_plotly <- function(raw_data){
  sf_data = sf::st_simplify(twmap::tw_county, dTolerance = 0.02)
  map_bounds = calculate_map_bounds(raw_data)
  
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
      data = raw_data,
      x = ~long,
      y = ~lati,
      customdata = ~sitename,
      color = ~sitename,
      marker = list(size = 15, line = list(width = 1, color = "white")),
      text = ~paste0("站點: ", sitename)
    ) %>%
    event_register("plotly_click") %>%
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
#'計算地圖邊界（供 topographic_plotly 與 server_module_plotly 使用）
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
