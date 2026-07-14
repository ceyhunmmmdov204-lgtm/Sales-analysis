

library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(dplyr)
library(stringr)
library(scales)
library(lubridate)

merged <- readRDS("merged_data.rds")
merged$pos           <- iconv(merged$pos, "UTF-8", "UTF-8", sub = "")
merged$product_clean <- iconv(merged$product_clean, "UTF-8", "UTF-8", sub = "")
merged$rec_date      <- as.Date(merged$rec_date)

normalize_pos <- function(x) {
  x <- as.character(x); x <- toupper(x)
  x <- str_replace_all(x, "\u018F", "E"); x <- str_replace_all(x, "\u011E", "G")
  x <- str_replace_all(x, "\u0130", "I"); x <- str_replace_all(x, "\u015E", "S")
  x <- str_replace_all(x, "\u00C7", "C"); x <- str_replace_all(x, "\u00D6", "O")
  x <- str_replace_all(x, "\u00DC", "U")
  x <- str_replace_all(x, "[^A-Z0-9 ]", " "); str_squish(x)
}

brand_rules <- tribble(
  ~brand,            ~pattern,
  "Araz",            "\\bARAZ\\b",
  "Bravo",           "\\bBRAVO\\b",
  "BazarStore",      "\\bBAZARSTORE\\b|\\bBAZAR STORE\\b",
  "Oba",             "\\bOBA\\b",
  "Spar",            "\\bSPAR\\b",
  "King Smart",      "\\bKING ?SMART\\b",
  "Bizim Market",    "\\bBIZIM MARKET\\b",
  "Rahat Market",    "\\bRAHAT\\b",
  "GrandMart",       "\\bGRAND ?MART\\b|\\bGRAND MARKET\\b",
  "MegaStore",       "\\bMEGA ?STORE\\b",
  "Neptun",          "\\bNEPTUN\\b",
  "AvroMart",        "\\bAVROMART\\b",
  "Qadiroglu",       "\\bQ[EA]DIROGLU\\b",
  "Amine Market",    "\\bAMIN[EA]? MARKET\\b",
  "En Ucuz",         "\\bEN UCUZ\\b",
  "McDonalds",       "\\bMC ?DONALD",
  "KFC",             "\\bKFC\\b",
  "Premium",         "\\bPREMIUM\\b",
  "Life Store",      "\\bLIFE STORE\\b|\\bLIFE CENTER\\b",
  "Qayali",          "\\bQAYALI\\b",
  "Sultan Market",   "\\bSULTAN MARKET\\b",
  "A Plus",          "\\bA PLUS\\b",
  "BolMart",         "\\bBOLMART\\b",
  "Ekonom Market",   "\\bEKONOM MARKET\\b",
  "Seda Store",      "\\bSEDA STORE\\b",
  "Port Baku",       "\\bPORT BAKU\\b",
  "Restoran",        "\\bRESTORAN\\b",
  "Aptek",           "\\bAPTEK\\b",
  "Erzaq Magazasi",  "\\bERZAQ MAGAZASI\\b",
  "Magaza (umumi)",  "^MAGAZA$|\\bMAGAZASI\\b",
  "Market (umumi)",  "\\bMARKET\\b"
)

assign_brand_vec <- function(nv) {
  res <- rep("Diger", length(nv)); un <- rep(TRUE, length(nv))
  for (i in seq_len(nrow(brand_rules))) {
    idx <- which(un); if (length(idx) == 0) break
    hit <- str_detect(nv[idx], brand_rules$pattern[i])
    res[idx[hit]] <- brand_rules$brand[i]; un[idx[hit]] <- FALSE
  }
  res
}

pos_lookup <- tibble(pos = unique(merged$pos)) %>%
  mutate(pos_brand = assign_brand_vec(normalize_pos(pos)),
         pos_brand = if_else(pos_brand %in% c("Market (umumi)","Magaza (umumi)"),
                             "Diger", pos_brand))
merged <- merged %>% left_join(pos_lookup, by = "pos")

brand_choices <- merged %>% filter(pos_brand != "Diger") %>%
  count(pos_brand, wt = total_sales, name = "s") %>%
  arrange(desc(s)) %>% pull(pos_brand)

junk_pat <- paste0("SAT SRBST|SATISH|SRBST TICART|KREDIT|AVANS|UMUMI|^RZAQ$|",
                   "GEYIMLRI|DD |MEBEL|DERMAN|^DV |XSTDN|^SATIS$|EDV|MEHSUL|",
                   "^ERZAQ$|RQMSAL|^MAL$|^MAGAZA$")

# Renk paleti (mavi tonlar)
BLUE <- "#2C7FB8"; DARK <- "#0B4F8A"; LIGHT <- "#7FB3D9"; GRID <- "#ECECEC"

# Olculu format funksiyalari (M / K)
fmt_m <- function(x) ifelse(x >= 1e6, paste0(round(x/1e6, 1), "M"),
                            ifelse(x >= 1e3, paste0(round(x/1e3), "K"), as.character(round(x))))

# Ortaq plotly tema (temiz gorunush)
clean_layout <- function(p, xtitle = "", ytitle = "", margin_l = 110) {
  p %>% layout(
    font = list(family = "Inter, sans-serif", size = 12, color = "#333"),
    xaxis = list(title = xtitle, gridcolor = GRID, zeroline = FALSE,
                 showline = FALSE, tickcolor = "#fff"),
    yaxis = list(title = ytitle, gridcolor = GRID, zeroline = FALSE,
                 automargin = TRUE),
    margin = list(l = margin_l, r = 30, t = 30, b = 50),
    plot_bgcolor = "#fff", paper_bgcolor = "#fff",
    showlegend = FALSE, bargap = 0.35
  ) %>% config(displayModeBar = FALSE)
}

#  UI ####
ui <- page_sidebar(
  title = "Perakende Satis Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = BLUE,
                   base_font = font_google("Inter")),
  
  sidebar = sidebar(
    width = 280,
    selectInput("brend", "Brend:", choices = c("Hamisi", brand_choices), selected = "Hamisi"),
    dateRangeInput("tarix", "Tarix araligi:",
                   start = min(merged$rec_date), end = max(merged$rec_date),
                   min = min(merged$rec_date), max = max(merged$rec_date),
                   format = "dd M yyyy", separator = " - "),
    helpText("Filtrler butun chart-lara tesir edir.")
  ),
  
  layout_columns(
    fill = FALSE, row_heights = "150px",
    value_box("Cem Satis", textOutput("kpi_satis"),
              showcase = bs_icon("cash-stack"), theme = "primary"),
    value_box("Qeyd Sayi", textOutput("kpi_qeyd"),
              showcase = bs_icon("receipt"), theme = value_box_theme(bg = "#5B7C99")),
    value_box("Unikal Magaza", textOutput("kpi_magaza"),
              showcase = bs_icon("shop"), theme = value_box_theme(bg = "#3E6B8C")),
    value_box("Orta Qebz", textOutput("kpi_qebz"),
              showcase = bs_icon("tag"), theme = value_box_theme(bg = "#6BA3C9"))
  ),
  
  card(
    card_header(textOutput("trend_title")),
    plotlyOutput("p_trend", height = "300px"), full_screen = TRUE
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(card_header("Top 10 Brend Uzre Satis"),
         plotlyOutput("p_brand", height = "400px"), full_screen = TRUE),
    card(card_header("Brend Uzre Magaza Sayi (Sebeke Boyukluyu)"),
         plotlyOutput("p_store", height = "400px"), full_screen = TRUE)
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(card_header("En Cox Satilan Real Mehsullar (Top 10)"),
         plotlyOutput("p_products", height = "400px"), full_screen = TRUE),
    card(card_header("Heftenin Gunu Uzre Ortalama Satis"),
         plotlyOutput("p_weekday", height = "400px"), full_screen = TRUE)
  )
)

#  SERVER  ####
server <- function(input, output, session) {
  
  fdata <- reactive({
    d <- merged %>% filter(rec_date >= input$tarix[1], rec_date <= input$tarix[2])
    if (input$brend != "Hamisi") d <- d %>% filter(pos_brand == input$brend)
    d
  })
  
  # KPI
  output$kpi_satis  <- renderText(paste0(round(sum(fdata()$total_sales, na.rm=TRUE)/1e6, 1), "M AZN"))
  output$kpi_qeyd   <- renderText(format(nrow(fdata()), big.mark=","))
  output$kpi_magaza <- renderText(format(n_distinct(fdata()$pos), big.mark=","))
  output$kpi_qebz   <- renderText(paste0(round(mean(fdata()$total_sales, na.rm=TRUE), 2), " AZN"))
  
  output$trend_title <- renderText({
    if (input$brend == "Hamisi") "Gunluk Satis Trendi (Umumi)"
    else paste0("Gunluk Satis Trendi - ", input$brend)
  })
  
  # Gunluk trend (xett + sahe)
  output$p_trend <- renderPlotly({
    d <- fdata() %>% group_by(rec_date) %>%
      summarise(satis = sum(total_sales, na.rm=TRUE), .groups="drop") %>% arrange(rec_date)
    plot_ly(d, x = ~rec_date, y = ~satis, type = "scatter", mode = "lines",
            line = list(color = BLUE, width = 2.5),
            fill = "tozeroy", fillcolor = "rgba(44,127,184,0.12)",
            hovertemplate = "%{x|%d %b}<br><b>%{y:,.0f} AZN</b><extra></extra>") %>%
      clean_layout(ytitle = "Satis (AZN)", margin_l = 70)
  })
  
  # Top 10 brend (horizontal bar + reqem)
  output$p_brand <- renderPlotly({
    d <- merged %>%
      filter(rec_date >= input$tarix[1], rec_date <= input$tarix[2], pos_brand != "Diger") %>%
      group_by(pos_brand) %>% summarise(satis = sum(total_sales, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(satis)) %>% slice_head(n = 10)
    plot_ly(d, x = ~satis, y = ~reorder(pos_brand, satis), type = "bar", orientation = "h",
            marker = list(color = BLUE),
            text = ~sapply(satis, fmt_m), textposition = "outside",
            textfont = list(size = 11, color = "#333"),
            hovertemplate = "%{y}: %{x:,.0f} AZN<extra></extra>") %>%
      clean_layout(xtitle = "Satis (AZN)", margin_l = 110)
  })
  
  # Magaza sayi (horizontal bar + reqem)
  output$p_store <- renderPlotly({
    d <- merged %>%
      filter(rec_date >= input$tarix[1], rec_date <= input$tarix[2], pos_brand != "Diger") %>%
      distinct(pos, pos_brand) %>% count(pos_brand, name = "magaza") %>%
      arrange(desc(magaza)) %>% slice_head(n = 10)
    plot_ly(d, x = ~magaza, y = ~reorder(pos_brand, magaza), type = "bar", orientation = "h",
            marker = list(color = DARK),
            text = ~magaza, textposition = "outside",
            textfont = list(size = 11, color = "#333"),
            hovertemplate = "%{y}: %{x} magaza<extra></extra>") %>%
      clean_layout(xtitle = "Magaza sayi", margin_l = 110)
  })
  
  # Top 10 real mehsul (horizontal bar + reqem)
  output$p_products <- renderPlotly({
    d <- fdata() %>%
      filter(!is.na(product_clean), !str_detect(product_clean, junk_pat)) %>%
      group_by(product_clean) %>% summarise(satis = sum(total_sales, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(satis)) %>% slice_head(n = 10) %>%
      mutate(ad = str_to_sentence(product_clean))
    plot_ly(d, x = ~satis, y = ~reorder(ad, satis), type = "bar", orientation = "h",
            marker = list(color = LIGHT),
            text = ~sapply(satis, fmt_m), textposition = "outside",
            textfont = list(size = 11, color = "#333"),
            hovertemplate = "%{y}: %{x:,.0f} AZN<extra></extra>") %>%
      clean_layout(xtitle = "Satis (AZN)", margin_l = 120)
  })
  
  # Heftenin gunu (xett + noqte + reqem)
  output$p_weekday <- renderPlotly({
    d <- fdata() %>% filter(rec_date != as.Date("2024-04-19")) %>%
      mutate(wd = wday(rec_date, week_start = 1)) %>%
      group_by(wd) %>% summarise(orta = sum(total_sales, na.rm=TRUE)/n_distinct(rec_date), .groups="drop")
    wd_ad <- c("B.ert","Cer.ax","Cer","Cum.ax","Cum","Sen","Bazar")
    d$gun <- factor(wd_ad[d$wd], levels = wd_ad)
    plot_ly(d, x = ~gun, y = ~orta, type = "scatter", mode = "lines+markers+text",
            line = list(color = BLUE, width = 2.5), marker = list(color = DARK, size = 9),
            text = ~sapply(orta, fmt_m), textposition = "top center",
            textfont = list(size = 11, color = "#333"),
            hovertemplate = "%{x}: %{y:,.0f} AZN<extra></extra>") %>%
      clean_layout(ytitle = "Orta gunluk satis", margin_l = 70)
  })
}

shinyApp(ui, server)
