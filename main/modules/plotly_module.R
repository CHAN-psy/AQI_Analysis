# =============================================================================
# modules/plotly_module.R
# Shiny module UI 元件工廠 & ServerModule_plotly
# =============================================================================

# -----------------------------------------------------------------------------
# UI 元件工廠
# -----------------------------------------------------------------------------

#' 帶有 box 外框的 Plotly 輸出元件
#'
#' @param id Module ID
#' @param title 標題文字
dash_card_plotly <- function(id, title = "預設標題") {
  ns <- NS(id)
  box(
    title = title,
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    shinycssloaders::withSpinner(
      plotlyOutput(ns("plot"), width = "100%", height = "500px")
    )
  )
}

#' 不帶 box 外框的 Plotly 輸出元件（用於 tabPanel 內）
#'
#' @param id Module ID
#' @param title 標題文字（目前未使用，保留供未來擴充）
dash_plotly <- function(id, title = "預設標題") {
  ns <- NS(id)
  shinycssloaders::withSpinner(
    plotlyOutput(ns("plot"), width = "100%", height = "650px")
  )
}

#' 數值資訊卡片
#'
#' @param id Module ID
#' @param subtitle 副標題文字
dash_info_card <- function(id, subtitle = "預設標題") {
  ns <- NS(id)
  bs4ValueBox(
    subtitle = subtitle,
    value    = textOutput(ns("text")),
    icon     = icon("upload"),
    color    = "success",
    width    = 12
  )
}

# -----------------------------------------------------------------------------
# Server Module
# -----------------------------------------------------------------------------

#' Plotly 圖表 Server Module
#'
#' 統一處理地圖與一般圖表的渲染、站點高亮邏輯
#'
#' @param id        Module ID
#' @param data      reactive：資料框
#' @param plot_fn   繪圖函數，簽名依 type 而異
#' @param type      "map" 或 "other"
#' @param vals      reactiveValues：共用狀態（click_site, unit, pollutants）
server_module_plotly <- function(id, data, plot_fn, type, vals) {
  moduleServer(id, function(input, output, session) {

    # --- 建立圖表 reactive ---
    plot_obj <- reactive({
      df <- data()
      req(df)
      if (type == "other") {
        plot_fn(df, vals$unit, vals$pollutants)
      } else if (type == "map") {
        plot_fn(df)
      }
    })

    output$plot <- renderPlotly(plot_obj())

    proxy <- plotlyProxy("plot", session)

    # --- 站點高亮 observer ---
    observeEvent(vals$click_site, {
      current_data <- data()
      req(current_data)

      pb      <- plotly_build(plot_obj())
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
        site_data <- if (is.null(vals$click_site) || length(vals$click_site) == 0) {
          lazy_dt(current_data) %>%
            select(sitename, long, lati) %>%
            unique() %>%
            as.data.frame()
        } else {
          lazy_dt(current_data) %>%
            select(sitename, long, lati) %>%
            unique() %>%
            filter(sitename %in% vals$click_site) %>%
            as.data.frame()
        }
        req(site_data)

        map_bounds <- calculate_map_bounds(site_data)

        plotlyProxyInvoke(proxy, "restyle", list("opacity" = opacity), all_traces)
        Sys.sleep(0.1)
        plotlyProxyInvoke(proxy, "relayout", list(
          xaxis = list(
            range          = c(map_bounds[[2]], map_bounds[[1]]),
            showgrid       = FALSE,
            showticklabels = FALSE,
            zeroline       = FALSE
          ),
          yaxis = list(
            range          = c(map_bounds[[4]], map_bounds[[3]]),
            showgrid       = FALSE,
            showticklabels = FALSE,
            zeroline       = FALSE
          ),
          showlegend   = TRUE,
          legend       = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        ))
      } else {
        plotlyProxyInvoke(proxy, "restyle", list("opacity" = opacity), all_traces)
      }
    }, ignoreNULL = FALSE)
  })
}
