# =============================================================================
# R/ui.R
# UI 定義
# =============================================================================

ui <- bs4DashPage(
  title = "BS4Dash 實作 - AQI分析",

  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "選單",
      image = "mainpage_icon.jpg"
    )
  ),

  sidebar = bs4DashSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",      tabName = "Home",      icon = icon("home")),
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("table"))
    )
  ),

  controlbar = bs4DashControlbar(
    id    = "my_controlbar",
    width = 350,
    bs4Accordion(
      id = "acc",

      # --- 資料控制 ---
      bs4AccordionItem(
        title = "資料控制",
        id    = "data_acc",
        box(uiOutput("SitenamePanel"), title = "選擇站點:", width = 12, collapsed = FALSE),
        box(uiOutput("DatePanel"),     title = "選擇時段:", width = 12, collapsed = FALSE),
        box(
          div(style = "text-align: center;", actionButton("Submit", "Go!!!!!")),
          title = "取得資料", width = 12, collapsed = FALSE
        )
      ),

      # --- 圖表控制 ---
      bs4AccordionItem(
        title = "圖表控制",
        id    = "plot_acc",
        box(uiOutput("ClickPanel"),     title = "Highlight站點:",  width = 12, collapsed = FALSE),
        box(uiOutput("PollutantPanel"), title = "Highlight汙染物:", width = 12, collapsed = FALSE),
        box(uiOutput("UnitPanel"),      title = "單位轉換:",       width = 12, collapsed = FALSE)
      )
    )
  ),

  body = bs4DashBody(
    tabItems(

      # ── Home ──
      tabItem(
        tabName = "Home",
        bs4Jumbotron(
          title   = "環境部AQI資料分析",
          lead    = "在Dashboard頁面中，分析各項AQI並視覺化",
          status  = "info",
          btnName = "Link",
          href    = "https://github.com/CHAN-psy",
          "進入Github以查看更多內容"
        ),
        bs4UserCard(
          title = bs4UserDescription(
            title    = div("作者",            style = "text-align: center; font-weight: bold;"),
            subtitle = div("Chan Chun Cheng", style = "text-align: center; font-weight: bold;"),
            image    = "siang.jpg"
          ),
          br(), br(),
          div("國立中正大學 心理所碩士", style = "text-align: center; font-weight: bold;"),
          div("熱愛資料視覺化、分析",   style = "text-align: center;"),
          collapsible = FALSE
        )
      ),

      # ── Dashboard ──
      tabItem(
        tabName = "Dashboard",
        fluidRow(
          column(width = 6, uiOutput("Accordion")),
          column(width = 6, dash_card_plotly("Card1", title = "地圖"))
        ),
        fluidRow(
          tabBox(
            id     = "tabbox",
            title  = "趨勢圖",
            width  = 12,
            height = "700px",
            tabPanel(title = "折線圖", dash_plotly("Card2")),
            tabPanel(title = "頻率圖", dash_plotly("Card3"))
          )
        ),
        fluidRow(
          width = 12,
          dash_card_plotly("Card4", title = "小提琴圖")
        )
      )
    )
  )
)
