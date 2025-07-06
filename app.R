library(shiny)
library(bs4Dash)
library(ggplot2)
library(plotly)
library(forcats)
library(lubridate)
library(dtplyr)

purrr::walk(list.files("utils", pattern = "\\.R$", full.names = TRUE), source)

DashCard_plotly <- function(id, title = "預設標題") {
  ns <- NS(id)
  box(
    title = title,
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    shinycssloaders::withSpinner(
      plotlyOutput(
        ns("plot"),width = "100%", height = "500px"
      )
    )
  )
}
DashInfoCard <- function(id, subtitle = "預設標題") {
  ns <- NS(id)
  bs4ValueBox(
    subtitle = subtitle,
    value = textOutput(ns("text")),
    icon = icon("upload"),
    color = "success",
    width = 12
  )
}
ServerModule_plotly <-
  function(id,
           data,
           default_function,
           type ,
           vals,
           pol = NULL) {
    moduleServer(id, function(input, output, session) {
      
      plot_obj <- reactive({
        df <- data()
        req(df)
        default_function(df)
      })
      
      output$plot <- renderPlotly({
        plot_obj()
      })
      
      proxy = plotlyProxy("plot", session)
      
      
      
      a <-  observeEvent(vals$click_site, {
        current_data <- data()
        req(current_data)
        
        # 所有 trace 的 sitename（customdata）
        pb <- plotly_build(plot_obj())
        

        opacity <- purrr::map_dbl(pb$x$data, ~ {
          cds <- .x$customdata
          if (is.null(cds)) {
            1
          } else if (any(cds %in% vals$click_site)) {
            1
          } else {
            0.2
          }
        })
        
        all_traces <- seq_along(opacity) - 1L
        
        if (type == "map") {
          site_data = if (is.null(vals$click_site) |
                          length(vals$click_site) == 0) {
            lazy_dt(current_data) %>%
              select(sitename, long, lati) %>%
              unique() %>%
              as.data.frame()
          } else{
            lazy_dt(current_data) %>%
              select(sitename, long, lati) %>%
              unique() %>%
              filter(sitename %in% vals$click_site) %>%
              as.data.frame()
          }
          req(site_data)
          map_bounds = calculate_map_bounds(site_data)
          
          plotlyProxyInvoke(proxy,
                            "restyle",
                            list("opacity" = opacity),all_traces)
          Sys.sleep(0.1)
          plotlyProxyInvoke(
            proxy,
            "relayout",
            list(
              xaxis = list(
                range = c(map_bounds[[2]], map_bounds[[1]]),
                showgrid = FALSE,
                showticklabels = FALSE,
                zeroline = FALSE
              ),
              yaxis = list(
                range = c(map_bounds[[4]], map_bounds[[3]]),
                showgrid = FALSE,
                showticklabels = FALSE,
                zeroline = FALSE
              ),
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                xanchor = "center",
                x = 0.5,
                y = -0.2
              )
              ,
              plot_bgcolor = "white",
              paper_bgcolor = "white"
            )
          )
        } else{
          plotlyProxyInvoke(proxy, "restyle", list("opacity" = opacity),all_traces)
        }
      }, ignoreNULL  = FALSE)
    })
  }

# UI 定義
ui <- bs4DashPage(
  title = "BS4Dash 實作 - AQI分析",
  header = bs4DashNavbar(title = bs4DashBrand(title = "選單",
                                              image = "mainpage_icon.jpg")),
  sidebar = bs4DashSidebar(sidebarMenu(
    id = "sidebar",
    menuItem("Home",
             tabName = "Home",
             icon = icon("home")),
    menuItem("Dashboard",
             tabName = "Dashboard",
             icon = icon("table"))
  )),
  controlbar = bs4DashControlbar(
    id = "my_controlbar",
    width = 350,
    bs4Accordion(
      id = 'acc',
      bs4AccordionItem(
        title = "資料控制",
        id = "data_acc",
        box(
          uiOutput("SitenamePanel"),
          title = "選擇站點:",
          width = 12,
          collapsed = FALSE
        ),
        box(
          uiOutput("DatePanel"),
          title = "選擇時段:",
          width = 12,
          collapsed = FALSE
        ),
        box(
          div(
            style = "text-align: center;",
            actionButton("Submit", "Go!!!!!")
          ),
          title = "取得資料",
          width = 12,
          collapsed = FALSE
        )
      ),
      bs4AccordionItem(
        title = "圖表控制",
        id = "plot_acc",
        box(
          uiOutput("ClickPanel"),
          title = "Highlight站點:",
          width = 12,
          collapsed = FALSE
        ),
        box(
          uiOutput("PollutantPanel"),
          title = "Highlight汙染物:",
          width = 12,
          collapsed = FALSE
        )
      )
    )
  )
  ,
  body = bs4DashBody(
    tabItems(
      tabItem(
        tabName = "Home",
        bs4Jumbotron(
          title = "環境部AQI資料分析",
          lead = "在Dashboard頁面中，分析各項AQI並視覺化",
          status = "info",
          btnName = "Link",
          href = "https://github.com/CHAN-psy",
          "進入Github以查看更多內容"
        ),
        tabItem(
          tabName = "Chan",
          bs4UserCard(
            title = bs4UserDescription(
              title = div("作者", style = "text-align: center; font-weight: bold;"),
              subtitle = div("Chan Chun Cheng", style = "text-align: center; font-weight: bold;"),
              image = "siang.jpg"
            ),
            br(),
            br(),
            div("國立中正大學 心理所碩士", style = "text-align: center; font-weight: bold;"),
            div("熱愛資料視覺化、分析", style = "text-align: center;"),
            collapsible = FALSE
          )
        )
      )
      ,
      tabItem(
        tabName = "Dashboard",
        fluidRow(column(
          width = 6,
          uiOutput("Accordion")
        ), column(
          width = 6,
          DashCard_plotly("Card1", title = "地圖")
        )),
        # --- 第三列 ---
        fluidRow(
          tabBox(
            id = "tabbox",
            title = "趨勢圖",
            width = 12,
            tabPanel(title = "折線圖",
                     DashCard_plotly("Card2")),
            tabPanel(title = "頻率圖",
                     DashCard_plotly("Card3"))
          )
        ),
        fluidRow(width = 12,
                 DashCard_plotly("Card4", title = "小提琴圖"))
      )
      
    ))
)



# Server 邏輯
server <- function(input, output, session) {
  #資料處理
  data <- eventReactive(input$Submit, {
    message("GO!!!!!")
    message("提交按鈕已點擊，正在執行資料庫查詢...")
    
    Get_Value(db_info,
              list(input$SitenamePanel,
                   input$DatePanel[[1]],
                   input$DatePanel[[2]])) %>%
      Trans_function() %>%
       as.data.frame() 
    
  })
  
  pollutant_cols <- reactive({
    req(data())
    data() %>%
      select(pollutant) %>%
      unique() %>%
      unlist(use.names = FALSE)
  })
  
  
  name = dbGetQuery(Open_Database_(db_info), "SELECT sitename FROM Geo") %>%
    unlist(use.names = FALSE)
  
  vals = reactiveValues(click_site = c(),
                        pollutants = NULL)
  
  
  #UI建立
  output$SitenamePanel = renderUI({
    selectizeInput (
      inputId = "SitenamePanel",
      label = "",
      choices = name,
      multiple = TRUE,
      selected = "花蓮",
      options = list(plugins = list('remove_button'),maxItems = 5)
    )
  })
  output$DatePanel = renderUI({
    #最早的日期是2018-01-01 00:00
    min = "2018-01-01"
    max = now() %>% format(., "%Y-%m-%d")
    start = (now() %m-% months(3)) %>% format(., "%Y-%m-%d")
    end  = now() %>% format(., "%Y-%m-%d")
    dateRangeInput(
      inputId = "DatePanel",
      label = NULL,
      min = min,
      max = max,
      start = start,
      end = end
    )
  })
  output$PollutantPanel = renderUI({
    req(pollutant_cols)
    name = pollutant_cols()
    selectizeInput (
      inputId = "PollutantPanel",
      label = NULL,
      choices = name,
      selected = all_of(name),
      multiple = FALSE
    )
  })
  output$Accordion <- renderUI({
    req(data, input$SitenamePanel)
    site <- input$SitenamePanel
    # 為每個站點生成 tabPanel
    tab_panels <- purrr::map(site, function(x) {
      item <- c(
        "PM2_5_BiggerthanWHO_Day",
        "PM2_5_BiggerthanTaiwan_Day",
        "PM10_BiggerthanWHO_Day",
        "PM10_BiggerthanTaiwan_Day",
        "AQIMedium",
        "MainPollutant"
      )
      name_vector <- paste(x, item, sep = '_')
      
      tabPanel(
        title = x,
        fluidRow(
          valueBoxOutput(outputId = name_vector[[1]], width = 6),
          valueBoxOutput(outputId = name_vector[[2]], width = 6),
          valueBoxOutput(outputId = name_vector[[3]], width = 6),
          valueBoxOutput(outputId = name_vector[[4]], width = 6),
          valueBoxOutput(outputId = name_vector[[5]], width = 6),
          valueBoxOutput(outputId = name_vector[[6]], width = 6)
        )
      )
    })
    
    # 使用 tabBox
    do.call(tabBox, c(
      list(
        title = "重點資訊整理",
        id = "site_info_tabs",
        width = 12
      ),
      tab_panels
    ))
  })
  observe({
    output$ClickPanel = renderUI({
      req(input$SitenamePanel)
      selectizeInput (
        inputId = "Highlightening",
        label = NULL,
        choices = input$SitenamePanel,
        selected = vals$click_site,
        multiple = TRUE,
        options = list(plugins = list('remove_button'))
      )
    })
  })
  observeEvent(input$Submit, {
    req(data, input$SitenamePanel)
    site <- input$SitenamePanel
    item <-
      c(
        "PM2_5_BiggerthanWHO_Day",
        "PM2_5_BiggerthanTaiwan_Day",
        "PM10_BiggerthanWHO_Day",
        "PM10_BiggerthanTaiwan_Day",
        "AQIMedium",
        "MainPollutant"
      )
    title <- c("PM2.5 台規超標",
               "PM2.5 世衛超標",
               "PM10 台規超標",
               "PM10 世衛超標",
               "AQI中位數",
               "主（次）汙染物")
    icon_vec <- c("mountain",
                  "globe",
                  "mountain",
                  "globe",
                  "align-center",
                  "skull-crossbones")
    tooltip = c("環境部定義的PM2.5標準為30μg/m³",
                "世界衛生組織建議的PM2.5標準為15μg/m³",
                "環境部定義的PM10標準為75μg/m³",
                "世界衛生組織建議的PM10標準為30μg/m³",
                sprintf("%s~%s之間的AQI中位數",input$DatePanel[[1]],input$DatePanel[[2]]),
                "通常決定該地區AQI指標的汙染物"
    )
    purrr::walk(site,
                function(x) {
                  idx <- Trans_to_valuebox(x, data())
                  unit <- paste(x, item, sep = "_")
                  
                  purrr::pwalk(list(
                    id = unit,
                    value = idx,
                    subtitle = title,
                    icon_vec = icon_vec,
                    tooltip = tooltip
                  ), function(id, value, subtitle, icon_vec,tooltip) {
                    output[[id]] =
                      renderbs4ValueBox({
                        bs4ValueBox(
                          value = value,
                          subtitle = subtitle,
                          icon = icon(icon_vec),
                          color = "success"
                        )
                      })
                    addTooltip(id = id,
                               options = list(
                                 title = tooltip,
                                 placement = "left"
                               ))
                  })
                })
    
  })
  observeEvent(input$SitenamePanel,{
    req(input$SitenamePanel)
    if(length(input$SitenamePanel) == 5){
      addTooltip("SitenamePanel",options = list(title = "最多五個站位>.0",placemeny = "bottom"))
    }else{
      removeTooltip("SitenamePanel")
    }
  })
  #圖片輸出
  ServerModule_plotly("Card1",
                      data,
                      Topographic_Plotly,
                      "map",
                      vals)
  ServerModule_plotly("Card2",
                      data,
                      Line_Plotlty_,
                      "other",
                      vals)
  ServerModule_plotly("Card3",
                      data,
                      Frequncy_Plotly,
                      "other",
                      vals)
  ServerModule_plotly("Card4",
                      data,
                      Violin_Plotly,
                      "other",
                      vals)
  observe({
    id = 1:4
    lower = input$DatePanel[[1]] 
    upper = input$DatePanel[[2]] 
    tooltip = c("站位地理位置，點選站位可標註特定站位",
                sprintf("%s~%s之間的副指標趨勢資訊",lower,upper),
                sprintf("%s~%s之間的副指標頻率資訊",lower,upper),
                sprintf("%s~%s之間的副指標數據分布",lower,upper))
    purrr::walk(id,
      function(id){
      addTooltip(id = paste0("Card",id,"-plot"),
                 options = list(
                   title = tooltip[id],
                   placement = "top"
                 ))
    })
})
  addTooltip(id = "data_acc",
             options = list(
               title = "向資料庫取得站位與時間資料，透過GO!!!!!送出",
               placement = "top"
             ))
  addTooltip(id = "plot_acc",
             options = list(
               title = "控制站位透明度與汙染物",
               placement = "top"
             ))
  #更新Vals$click_site透過點選圖片
  observeEvent(event_data("plotly_click"), {
    current_click = event_data("plotly_click")$customdata %>% unique()
    req(current_click)
    
    if (current_click %in% vals$click_site) {
      new_click = setdiff(vals$click_site, current_click)
      vals$click_site =  if (length(new_click) == 0)
        NULL
      else
        new_click
      
    } else{
      vals$click_site = union(vals$click_site, current_click)
    }
  }, ignoreNULL  = FALSE)
  
  #更新Vals$click_site透過點選selectizeInput
  observeEvent(input$Highlightening, {
    if (!identical(sort(input$Highlightening), sort(vals$click_site))) {
      vals$click_site <-
        if (length(input$Highlightening) == 0)
          NULL
      else
        input$Highlightening
    }
  }, ignoreNULL  = FALSE)
}

# 執行應用
options(shiny.reactlog = TRUE)
shinyApp(ui, server)