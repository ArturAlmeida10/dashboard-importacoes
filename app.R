# app.R
# Dashboard interativo de comércio exterior por NCM
# Versão refinada: boxes menores, gráficos mais limpos e hover sem repetição

library(shiny)
library(dplyr)
library(stringr)
library(scales)
library(bslib)
library(plotly)
library(shinyWidgets)
library(DT)
library(tibble)

# =========================================================
# Carregamento da base
# =========================================================

rds_file <- "base_agregada_ncm_paises.rds"
if (!file.exists(rds_file)) {
  stop("Não encontrei o arquivo '", rds_file, "' na pasta do app.")
}

base_agregada <- readRDS(rds_file)

# Garante colunas essenciais e cria o rótulo, se necessário
required_cols <- c("co_ano", "co_ncm", "co_pais", "pais", "vl_fob", "kg_liquido", "ds_ncm")
missing_cols <- setdiff(required_cols, names(base_agregada))
if (length(missing_cols) > 0) {
  stop("A base_agregada não possui as colunas necessárias: ", paste(missing_cols, collapse = ", "))
}

base_agregada <- base_agregada |>
  mutate(
    co_ncm = as.character(co_ncm),
    co_pais = as.character(co_pais),
    pais = as.character(pais),
    ds_ncm = as.character(ds_ncm),
    ncm_label = if ("ncm_label" %in% names(base_agregada)) {
      as.character(ncm_label)
    } else {
      paste0(co_ncm, " - ", ds_ncm)
    }
  )

ncm_lookup <- base_agregada |>
  distinct(co_ncm, ds_ncm, ncm_label) |>
  mutate(ncm_len = nchar(co_ncm)) |>
  arrange(ncm_len, co_ncm)

ncm_choices <- ncm_lookup |>
  transmute(choice = ncm_label, co_ncm) |>
  tibble::deframe()

# =========================================================
# CSS
# =========================================================

app_css <- tags$style(HTML("\
  .sidebar {
    background: #ffffff;
    border-right: 1px solid #e5e7eb;
  }
  .card {
    border-radius: 18px;
    box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
  }
  .value-box,
  .bslib-value-box {
    border-radius: 14px !important;
    min-height: 82px !important;
    padding: 0.22rem 0.42rem !important;
    overflow: visible !important;
  }
  .value-box .value-box-title,
  .bslib-value-box .value-box-title {
    font-size: 0.8rem !important;
    line-height: 1.05 !important;
    opacity: 0.95;
    margin-bottom: 0.02rem !important;
    margin-top: 0.3rem !important;
    white-space: normal !important;
    overflow: visible !important;
    word-break: break-word !important;
  }
  .value-box .value-box-value,
  .bslib-value-box .value-box-value {
    font-size: 1rem !important;
    line-height: 1.02 !important;
    font-weight: 700 !important;
    margin-top: 0.3rem !important;
    white-space: normal !important;
    overflow: visible !important;
    word-break: break-word !important;
  }
  .value-box .value-box-showcase,
  .bslib-value-box .value-box-showcase {
    opacity: 0.9;
    transform: scale(0.6);
    margin-top: 0rem;
  }
  .nav-tabs .nav-link {
    font-weight: 600;
    color: #0f766e;
  }
  .nav-tabs .nav-link.active {
    color: #0f766e;
    border-top-left-radius: 12px;
    border-top-right-radius: 12px;
  }
  .selectize-input {
    border-radius: 12px !important;
    padding: 0.75rem 0.9rem !important;
  }
  .small-muted {
    color: #6b7280;
    font-size: 0.92rem;
  }
"))

# =========================================================
# Funções auxiliares
# =========================================================

pretty_number_pt <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

make_top7_group <- function(df, metric = c("vl_fob", "kg_liquido")) {
  metric <- match.arg(metric)
  
  top7 <- df |>
    group_by(pais) |>
    summarise(total = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    slice_head(n = 7) |>
    pull(pais)
  
  df |>
    mutate(pais_grupo = if_else(pais %in% top7, pais, "Outros"))
}

make_series <- function(df, metric = c("vl_fob", "kg_liquido", "preco_usd_kg")) {
  metric <- match.arg(metric)
  
  if (metric == "vl_fob") {
    out <- df |>
      group_by(co_ano, pais_grupo) |>
      summarise(valor = sum(vl_fob, na.rm = TRUE), .groups = "drop") |>
      mutate(hover_unidade = paste0("US$ ", pretty_number_pt(valor / 1e6, 2), " mi"),
             y_axis = "US$ mi")
  } else if (metric == "kg_liquido") {
    out <- df |>
      group_by(co_ano, pais_grupo) |>
      summarise(valor = sum(kg_liquido, na.rm = TRUE), .groups = "drop") |>
      mutate(hover_unidade = paste0(pretty_number_pt(valor / 1e6, 2), " mi kg"),
             y_axis = "mi kg")
  } else {
    out <- df |>
      group_by(co_ano, pais_grupo) |>
      summarise(
        vl_fob = sum(vl_fob, na.rm = TRUE),
        kg_liquido = sum(kg_liquido, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        valor = if_else(kg_liquido > 0, vl_fob / kg_liquido, NA_real_),
        hover_unidade = paste0("US$ ", pretty_number_pt(valor, 2), "/kg"),
        y_axis = "US$/kg"
      )
  }
  
  out |>
    group_by(pais_grupo) |>
    arrange(co_ano, .by_group = TRUE) |>
    mutate(is_last = co_ano == max(co_ano, na.rm = TRUE)) |>
    ungroup()
}

make_plotly_line <- function(df, title, subtitle, y_lab, palette = NULL, show_end_labels = FALSE) {
  
  # escolhe o ano de referência para ordenar as traces
  ref_year <- max(df$co_ano, na.rm = TRUE)
  
  ordem_traces <- df |>
    filter(co_ano == ref_year) |>
    arrange(desc(valor)) |>
    pull(pais_grupo)
  
  groups <- ordem_traces
  if (is.null(palette)) {
    palette <- c(
      "#2563eb", "#059669", "#f59e0b", "#ef4444", "#8b5cf6",
      "#14b8a6", "#ec4899", "#64748b", "#22c55e"
    )
    names(palette) <- groups
  } else {
    palette <- palette[groups]
  }
  
  p <- plot_ly(type = "scatter", mode = "lines+markers")
  
  for (g in groups) {
    d <- df |>
      filter(pais_grupo == g) |>
      arrange(co_ano)
    
    p <- p |>
      add_trace(
        data = d,
        x = ~co_ano,
        y = ~valor,
        name = g,
        line = list(width = 2.2, color = palette[[g]]),
        marker = list(size = 6, color = palette[[g]]),
        text = ~paste0(
          "<b>", pais_grupo, "</b>",
          "<br>Ano: ", co_ano,
          "<br>", str_remove_all(y_lab, "\n"), ": ", hover_unidade
        ),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = TRUE
      )
  }
  
  p |>
    layout(
      title = list(text = paste0("<b>", title, "</b><br><span style='font-size:12px;color:#6b7280;'>", subtitle, "</span>")),
      xaxis = list(
        title = list(text = ""),
        tickmode = "array",
        tickvals = seq(min(df$co_ano, na.rm = TRUE), max(df$co_ano, na.rm = TRUE), by = 1),
        gridcolor = "#e5e7eb",
        zeroline = FALSE,
        tickfont = list(color = "#374151"),
        automargin = TRUE,
        standoff = 0
      ),
      yaxis = list(
        title = y_lab,
        gridcolor = "#e5e7eb",
        zeroline = FALSE,
        tickfont = list(color = "#374151")
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.30,
        font = list(size = 10),
        title = list(text = "")
      ),
      margin = list(l = 80, r = 35, t = 60, b = 0),
      hovermode = "x unified",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "white"
    ) |>
    config(displayModeBar = FALSE)
}

# =========================================================
# UI
# =========================================================

ui <- page_sidebar(
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    tags$div(style = "width:14px;height:14px;border-radius:999px;background:#0f766e;"),
    tags$span("Dashboard de Comércio Exterior por NCM")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0f766e",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  fillable = TRUE,
  sidebar = sidebar(
    width = 350,
    bg = "white",
    card(
      card_header(tags$strong("Selecione o NCM")),
      card_body(
        selectizeInput(
          inputId = "ncm",
          label = NULL,
          choices = ncm_choices,
          selected = names(ncm_choices)[1],
          options = list(
            placeholder = "Digite o código ou a descrição do NCM...",
            create = FALSE,
            maxOptions = 3000
          )
        ),
        div(class = "small-muted", "A busca aceita código e descrição. Você pode selecionar desde códigos de 2 até 8 dígitos.")
      )
    ),
    hr(),
    card(
      card_header(tags$strong("NCM selecionado")),
      card_body(textOutput("ncm_info"))
    )
  ),
  
  layout_column_wrap(
    width = 1 / 4,
    value_box(title = "Valor FOB total", value = textOutput("vb_fob", container = span), showcase = bsicons::bs_icon("currency-dollar"), theme = value_box_theme(bg = "#224D4E", fg = "#FFFFFF")),
    value_box(title = "KG líquido total", value = textOutput("vb_kg", container = span), showcase = bsicons::bs_icon("box-seam"), theme = value_box_theme(bg = "#BD8846", fg = "#FFFFFF")),
    value_box(title = "Preço médio", value = textOutput("vb_preco", container = span), showcase = bsicons::bs_icon("graph-up"), theme = value_box_theme(bg = "#683650", fg = "#FFFFFF")),
    value_box(title = "País líder", value = textOutput("vb_pais", container = span), showcase = bsicons::bs_icon("globe-americas"), theme = "secondary")
  ),
  
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Valor FOB",
      card(
        card_body(plotlyOutput("plot_fob", height = 760))
      )
    ),
    nav_panel(
      "KG Líquido",
      card(
        card_body(plotlyOutput("plot_kg", height = 760))
      )
    ),
    nav_panel(
      "Preço médio (FOB / KG)",
      card(
        card_body(plotlyOutput("plot_preco", height = 760))
      )
    ),
    nav_panel(
      "Resumo",
      layout_column_wrap(
        width = 1 / 2,
        card(card_header(tags$strong("Principais países parceiros")), card_body(DTOutput("tbl_paises"))),
        card(card_header(tags$strong("Últimos anos da série")), card_body(DTOutput("tbl_serie")))
      )
    )
  ),
  
  app_css
)

# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  
  selected_ncm_desc <- reactive({
    req(input$ncm)
    ncm_lookup |>
      filter(co_ncm == input$ncm) |>
      slice_head(n = 1)
  })
  
  dados_ncm <- reactive({
    req(input$ncm)
    base_agregada |>
      filter(co_ncm == input$ncm)
  })
  
  dados_top7 <- reactive({
    dados_ncm() |>
      group_by(co_ano, pais) |>
      summarise(
        vl_fob = sum(vl_fob, na.rm = TRUE),
        kg_liquido = sum(kg_liquido, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  metrics <- reactive({
    df <- dados_ncm()
    total_fob <- sum(df$vl_fob, na.rm = TRUE)
    total_kg <- sum(df$kg_liquido, na.rm = TRUE)
    tibble(
      vl_fob = total_fob,
      kg_liquido = total_kg,
      preco = ifelse(total_kg > 0, total_fob / total_kg, NA_real_)
    )
  })
  
  pais_top <- reactive({
    df <- dados_ncm() |>
      group_by(pais) |>
      summarise(vl_fob = sum(vl_fob, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(vl_fob))
    if (nrow(df) == 0) return("—")
    df$pais[1]
  })
  
  output$ncm_info <- renderText({
    info <- selected_ncm_desc()
    if (nrow(info) == 0) return("NCM selecionado: não encontrado")
    paste0(info$co_ncm[1], " — ", info$ds_ncm[1])
  })
  
  output$vb_fob <- renderText({
    x <- metrics()$vl_fob
    paste0("US$ ", pretty_number_pt(x / 1e6, 1), " mi")
  })
  
  output$vb_kg <- renderText({
    x <- metrics()$kg_liquido
    paste0(pretty_number_pt(x / 1e6, 1), " mi kg")
  })
  
  output$vb_preco <- renderText({
    x <- metrics()$preco
    if (is.na(x)) return("—")
    paste0("US$ ", pretty_number_pt(x, 2), "/kg")
  })
  
  output$vb_pais <- renderText({
    pais_top()
  })
  
  output$plot_fob <- renderPlotly({
    df <- dados_top7() |>
      make_top7_group(metric = "vl_fob") |>
      group_by(co_ano, pais_grupo) |>
      summarise(vl_fob = sum(vl_fob, na.rm = TRUE), .groups = "drop") |>
      make_series("vl_fob")
    
    make_plotly_line(
      df,
      title = "Valor FOB importado por país",
      subtitle = paste0("NCM ", input$ncm, " — top 7 países + Outros"),
      y_lab = "Valor FOB (US$ mi)\n",
      show_end_labels = FALSE
    )
  })
  
  output$plot_kg <- renderPlotly({
    df <- dados_top7() |>
      make_top7_group(metric = "kg_liquido") |>
      group_by(co_ano, pais_grupo) |>
      summarise(kg_liquido = sum(kg_liquido, na.rm = TRUE), .groups = "drop") |>
      make_series("kg_liquido")
    
    make_plotly_line(
      df,
      title = "Volume importado (kg líquido) por país",
      subtitle = paste0("NCM ", input$ncm, " — top 7 países + Outros"),
      y_lab = "Volume (mi kg)\n",
      show_end_labels = FALSE
    )
  })
  
  output$plot_preco <- renderPlotly({
    df <- dados_top7() |>
      make_top7_group(metric = "vl_fob") |>
      group_by(co_ano, pais_grupo) |>
      summarise(
        vl_fob = sum(vl_fob, na.rm = TRUE),
        kg_liquido = sum(kg_liquido, na.rm = TRUE),
        .groups = "drop"
      ) |>
      make_series("preco_usd_kg")
    
    make_plotly_line(
      df,
      title = "Preço médio de importação por kg",
      subtitle = paste0("NCM ", input$ncm, " — FOB / KG líquido — top 7 países + Outros"),
      y_lab = "Preço médio (US$/kg)\n",
      show_end_labels = FALSE
    )
  })
  
  output$tbl_paises <- renderDT({
    df <- dados_ncm() |>
      group_by(pais) |>
      summarise(
        vl_fob = sum(vl_fob, na.rm = TRUE),
        kg_liquido = sum(kg_liquido, na.rm = TRUE),
        preco = if_else(sum(kg_liquido, na.rm = TRUE) > 0, sum(vl_fob, na.rm = TRUE) / sum(kg_liquido, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) |>
      arrange(desc(vl_fob)) |>
      slice_head(n = 10) |>
      mutate(
        vl_fob = paste0("US$ ", pretty_number_pt(vl_fob / 1e6, 2), " mi"),
        kg_liquido = paste0(pretty_number_pt(kg_liquido / 1e6, 2), " mi kg"),
        preco = paste0("US$ ", pretty_number_pt(preco, 2), "/kg")
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 10, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
  
  output$tbl_serie <- renderDT({
    df <- dados_ncm() |>
      group_by(co_ano) |>
      summarise(
        vl_fob = sum(vl_fob, na.rm = TRUE),
        kg_liquido = sum(kg_liquido, na.rm = TRUE),
        preco = if_else(sum(kg_liquido, na.rm = TRUE) > 0, sum(vl_fob, na.rm = TRUE) / sum(kg_liquido, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) |>
      arrange(desc(co_ano)) |>
      slice_head(n = 8) |>
      mutate(
        vl_fob = paste0("US$ ", pretty_number_pt(vl_fob / 1e6, 2), " mi"),
        kg_liquido = paste0(pretty_number_pt(kg_liquido / 1e6, 2), " mi kg"),
        preco = paste0("US$ ", pretty_number_pt(preco, 2), "/kg")
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
}

shinyApp(ui, server)
