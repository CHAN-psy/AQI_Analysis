# =============================================================================
# R/server.R
# Server 邏輯
# =============================================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # 靜態資料（啟動時取一次）
  # ---------------------------------------------------------------------------
  
  # 站點名稱清單
  site_names <- dbGetQuery(open_database(db_info), "SELECT sitename FROM Geo") %>%
    unlist(use.names = FALSE)
  
  # 從 SQLite 取最新一筆資料的日期，作為 DatePanel 的預設終止日
  latest_date <- tryCatch({
    conn   <- open_database(db_info)
    result <- dbGetQuery(conn, "SELECT MAX(datacreationdate) FROM Aqi_Sta") %>%
      unlist(use.names = FALSE)
    close_database(conn)
    as.Date(result)
  }, error = function(e) {
    message("無法取得最新日期，改用今日：", e$message)
    Sys.Date()
  })
  
  # ---------------------------------------------------------------------------
  # 共用狀態
  # ---------------------------------------------------------------------------
  
  vals <- reactiveValues(
    click_site = c(),
    pollutants = "aqi",
    date       = NULL,
    unit       = "aqi"
  )
  
  # ---------------------------------------------------------------------------
  # 主要資料（按下 Submit 後觸發）
  # ---------------------------------------------------------------------------
  
  data <- eventReactive(input$Submit, {
    message("提交按鈕已點擊，正在執行資料庫查詢...")
    get_value(
      db_info,
      list(input$SitenamePanel, input$DatePanel[[1]], input$DatePanel[[2]])
    ) %>%
      trans_function() %>%
      as.data.frame()
  })
  
  pollutant_cols <- reactive({
    req(data())
    data() %>%
      select(pollutant) %>%
      unique() %>%
      unlist(use.names = FALSE)
  })
  
  # ---------------------------------------------------------------------------
  # 動態 UI
  # ---------------------------------------------------------------------------
  
  output$SitenamePanel <- renderUI({
    selectizeInput(
      inputId = "SitenamePanel",
      label   = "",
      choices = site_names,
      multiple = TRUE,
      selected = "花蓮",
      options  = list(plugins = list("remove_button"), maxItems = 5)
    )
  })
  
  output$DatePanel <- renderUI({
    # 預設：最新日期往前推三個月 ~ 最新日期
    end_date   <- format(latest_date, "%Y-%m-%d")
    start_date <- format(latest_date %m-% months(3), "%Y-%m-%d")
    
    dateRangeInput(
      inputId = "DatePanel",
      label   = NULL,
      min     = "2016-11-25",
      max     = end_date,
      start   = start_date,
      end     = end_date
    )
  })
  
  output$PollutantPanel <- renderUI({
    req(pollutant_cols())
    selectizeInput(
      inputId  = "PollutantPanel",
      label    = NULL,
      choices  = pollutant_cols(),
      selected = "aqi",
      multiple = TRUE,
      options  = list(plugins = list("remove_button"))
    )
  })
  
  output$UnitPanel <- renderUI({
    selectizeInput(
      inputId  = "UnitPanel",
      label    = NULL,
      choices  = c("AQI", "濃度"),
      selected = "AQI",
      multiple = FALSE
    )
  })
  
  output$ClickPanel <- renderUI({
    req(input$SitenamePanel)
    selectizeInput(
      inputId  = "Highlightening",
      label    = NULL,
      choices  = input$SitenamePanel,
      selected = vals$click_site,
      multiple = TRUE,
      options  = list(plugins = list("remove_button"))
    )
  })
  
  # ---------------------------------------------------------------------------
  # 動態 ValueBox 區塊
  # ---------------------------------------------------------------------------
  
  output$Accordion <- renderUI({
    req(data)
    site <- input$SitenamePanel
    
    item_keys <- c(
      "PM2_5_BiggerthanWHO_Day",
      "PM2_5_BiggerthanTaiwan_Day",
      "PM10_BiggerthanWHO_Day",
      "PM10_BiggerthanTaiwan_Day",
      "AQIMedium",
      "MainPollutant"
    )
    
    tab_panels <- purrr::map(site, function(x) {
      ids <- paste(x, item_keys, sep = "_")
      tabPanel(
        title = x,
        fluidRow(
          valueBoxOutput(ids[[1]], width = 6),
          valueBoxOutput(ids[[2]], width = 6),
          valueBoxOutput(ids[[3]], width = 6),
          valueBoxOutput(ids[[4]], width = 6),
          valueBoxOutput(ids[[5]], width = 6),
          valueBoxOutput(ids[[6]], width = 6)
        )
      )
    })
    
    do.call(tabBox, c(
      list(title = "重點資訊整理", id = "site_info_tabs", width = 12),
      tab_panels
    ))
  })
  
  # ---------------------------------------------------------------------------
  # Submit 事件：更新 ValueBox 與 tooltip
  # ---------------------------------------------------------------------------
  
  observeEvent(input$Submit, {
    # 確保資料存在 (加上括號，這是在 onFlushed 外面，可以安全呼叫)
    req(data(), input$SitenamePanel)
    
    vals$date <- input$DatePanel
    site      <- input$SitenamePanel
    date      <- vals$date
    
    item_keys <- c(
      "PM2_5_BiggerthanWHO_Day", "PM2_5_BiggerthanTaiwan_Day",
      "PM10_BiggerthanWHO_Day",  "PM10_BiggerthanTaiwan_Day",
      "AQIMedium", "MainPollutant"
    )
    titles <- c(
      "PM2.5 台規超標", "PM2.5 世衛超標",
      "PM10 台規超標",  "PM10 世衛超標",
      "AQI中位數", "主（次）汙染物"
    )
    icons <- c("mountain", "globe", "mountain", "globe", "align-center", "skull-crossbones")
    tooltips <- c(
      "環境部定義的PM2.5標準為30μg/m³",
      "世界衛生組織建議的PM2.5標準為15μg/m³",
      "環境部定義的PM10標準為75μg/m³",
      "世界衛生組織建議的PM10標準為30μg/m³",
      sprintf("%s~%s之間的AQI中位數", date[[1]], date[[2]]),
      "通常決定該地區AQI指標的汙染物"
    )
    
    # ── 1. 正常生成 ValueBox，並順便收集 Tooltip 所需的 ID 列表 ──
    # 使用 map_dfr 可以把多個站點的 ID 與 Tooltip 文字整理成一個 data.frame
    tooltip_data <- purrr::map_dfr(site, function(x) {
      idx  <- trans_to_valuebox(x, data())
      unit <- paste(x, item_keys, sep = "_")
      
      purrr::pwalk(
        list(id = unit, value = idx, subtitle = titles, icon_vec = icons),
        function(id, value, subtitle, icon_vec) {
          # 這裡只產生乾淨的 bs4ValueBox，不包裝 tooltip，避開 tagAssert 錯誤
          output[[id]] <- renderbs4ValueBox({
            bs4ValueBox(
              value = value, 
              subtitle = subtitle, 
              icon = icon(icon_vec), 
              color = "success"
            )
          })
        }
      )
      
      # 回傳該站點對應的 ID 和 Tooltip 文字配對
      data.frame(id = unit, text = tooltips, stringsAsFactors = FALSE)
    })
    
    card_tooltips <- c(
      "站位地理位置，點選站位可標註特定站位",
      sprintf("%s~%s之間的副指標趨勢資訊", date[[1]], date[[2]]),
      sprintf("%s~%s之間的副指標頻率資訊", date[[1]], date[[2]]),
      sprintf("%s~%s之間的副指標數據分布", date[[1]], date[[2]])
    )
    
    # ── 2. 等待畫面渲染完成後，透過 JS 掛載所有 Tooltips ──
    session$onFlushed(function() {
      
      # 掛載 ValueBox 的 Tooltips (根據前面收集好的 tooltip_data)
      purrr::pwalk(
        list(id = tooltip_data$id, tooltip_text = tooltip_data$text),
        function(id, tooltip_text) {
          removeTooltip(id = id, session = session)
          addTooltip(
            id = id, 
            options = list(title = tooltip_text, placement = "left"), 
            session = session
          )
        }
      )
      
      # 掛載 Card 圖表的 Tooltips
      purrr::walk(1:4, function(i) {
        plot_id <- paste0("Card", i, "-plot")
        removeTooltip(id = plot_id, session = session)
        addTooltip(
          id = plot_id, 
          options = list(title = card_tooltips[[i]], placement = "top"), 
          session = session
        )
      })
      
    }, once = TRUE)
    
  })
  
  # ---------------------------------------------------------------------------
  # 其他 observers
  # ---------------------------------------------------------------------------
  
  # 站點數量上限提示
  observeEvent(input$SitenamePanel, {
    req(input$SitenamePanel)
    if (length(input$SitenamePanel) == 5) {
      # 確保 selectizeInput 已經渲染完成再掛載
      session$onFlushed(function() {
        addTooltip(
          id = "SitenamePanel", 
          options = list(title = "最多五個站位>.0", placement = "bottom"), 
          session = session
        )
      }, once = TRUE)
    } else {
      removeTooltip(id = "SitenamePanel", session = session)
    }
  })
  
  # 點選圖表高亮
  observeEvent(event_data("plotly_click"), {
    current_click <- event_data("plotly_click")$customdata %>% unique()
    req(current_click)
    
    if (current_click %in% vals$click_site) {
      new_click      <- setdiff(vals$click_site, current_click)
      vals$click_site <- if (length(new_click) == 0) NULL else new_click
    } else {
      vals$click_site <- union(vals$click_site, current_click)
    }
  }, ignoreNULL = FALSE)
  
  # selectizeInput 高亮同步
  observeEvent(input$Highlightening, {
    if (!identical(sort(input$Highlightening), sort(vals$click_site))) {
      vals$click_site <- if (length(input$Highlightening) == 0) NULL else input$Highlightening
    }
  }, ignoreNULL = FALSE)
  
  # 單位切換
  observeEvent(input$UnitPanel, {
    req(input$UnitPanel)
    vals$unit <- if (input$UnitPanel == "AQI") "aqi" else "quality"
  }, ignoreNULL = FALSE)
  
  # 汙染物切換
  observeEvent(input$PollutantPanel, {
    req(input$PollutantPanel)
    
    vals$pollutants <- input$PollutantPanel
  }, ignoreNULL = FALSE)
  
  # ---------------------------------------------------------------------------
  # Controlbar tooltip
  # ---------------------------------------------------------------------------
  session$onFlushed(function() {
    addTooltip(id = "data_acc", options = list(title = "向資料庫取得站位與時間資料，透過GO!!!!!送出", placement = "top"),session = session)
    addTooltip(id = "plot_acc", options = list(title = "控制站位透明度與汙染物", placement = "top"),session = session)
  }, once = TRUE)
  
  # ---------------------------------------------------------------------------
  # 圖表輸出（使用 module）
  # ---------------------------------------------------------------------------
  
  server_module_plotly("Card1", data, topographic_plotly, "map",   vals)
  server_module_plotly("Card2", data, line_plotly,        "other", vals)
  server_module_plotly("Card3", data, frequency_plotly,   "other", vals)
  server_module_plotly("Card4", data, violin_plotly,       "other", vals)
}