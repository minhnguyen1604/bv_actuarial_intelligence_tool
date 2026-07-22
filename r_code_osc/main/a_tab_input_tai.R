
# ---- 1. Lấy danh sách sheet ----
sheets <- reactive({
  req(input$file_excel)
  excel_sheets(input$file_excel$datapath)
})

final_df_rv <- reactiveVal(NULL)


output$sheet_ui <- renderUI({
  req(sheets())
  selectInput(
    "sheet",
    "Chọn sheet",
    choices = sheets()
  )
})

# ---- 2. Đọc dữ liệu Excel ----
raw_df <- reactive({
  req(input$file_excel, input$sheet)
  read_excel(
    input$file_excel$datapath,
    sheet = input$sheet
  )
})

# ---- 3. Xử lý dữ liệu ----
df_tmp <- reactive({
  
  df <- raw_df()
  
  rows <- which(grepl(
    "^hàng hóa|^tr.*giao|sub.*total|^dầu khí|hàng không|xe.*c.*gi|y te|BHNN",
    df[[1]],
    ignore.case = TRUE
  ))
  
  df_tmp <- df[rows, c(1, 4, 11, 13)] %>%
    rename(
      Line         = 1,
      Recovery     = 2,
      Inward       = 3,
      Retrocession = 4
    ) %>%
    mutate(
      Recovery     = parse_number(Recovery),
      Inward       = parse_number(Inward),
      Retrocession = parse_number(Retrocession)
    )
  
  # ---------- HÀNG HÓA ----------
  cargo_row <- df_tmp %>%
    filter(str_detect(tolower(Line), "^hàng hóa|^tr.*giao")) %>%
    summarise(
      Line         = "Cargo in transit",
      Recovery     = sum(Recovery, na.rm = TRUE),
      Inward       = 0,
      Retrocession = 0
    )
  
  df_tmp <- df_tmp %>%
    filter(!str_detect(tolower(Line), "^hàng hóa|^tr.*giao")) %>%
    bind_rows(cargo_row)
  
  # ---------- MARINE ----------
  marine_subtotal <- df_tmp %>%
    filter(str_detect(tolower(Line), "sub.*marine"))
  
  marine_row <- marine_subtotal
  marine_row$Line <- "Hull & PI"
  num_cols <- sapply(marine_row, is.numeric)
  marine_row[, num_cols] <- marine_subtotal[, num_cols] - cargo_row[, num_cols]
  
  df_tmp <- df_tmp %>%
    filter(!str_detect(tolower(Line), "sub.*marine")) %>%
    bind_rows(marine_row)
  
  # ---------- XCG ----------
  mv_row <- df_tmp %>%
    filter(str_detect(tolower(Line), "^xe.*gi")) %>%
    summarise(
      Line         = "Motor Vehicles",
      Recovery     = sum(Recovery, na.rm = TRUE),
      Inward       = sum(Inward, na.rm = TRUE),
      Retrocession = sum(Retrocession, na.rm = TRUE)
    )
  
  df_tmp <- df_tmp %>%
    filter(!str_detect(tolower(Line), "^xe.*gi")) %>%
    bind_rows(mv_row)
  
  # ---------- MAP LINE ----------
  df_tmp <- df_tmp %>%
    mutate(
      Line = case_when(
        str_detect(tolower(Line), "fire")       ~ "Fire and Misc.",
        str_detect(tolower(Line), "engineer")   ~ "Engineering",
        str_detect(tolower(Line), " ma$")       ~ "General Liability",
        str_detect(tolower(Line), "dầu khí")    ~ "Oil and Gas",
        str_detect(tolower(Line), "hàng không") ~ "Aviation",
        str_detect(tolower(Line), "bhnn")       ~ "Agriculture",
        str_detect(tolower(Line), "travel")     ~ "Travel",
        TRUE ~ Line
      )
    )
  
  # ---------- Y TẾ ----------
  health_row <- df_tmp %>%
    filter(str_detect(tolower(Line), "^y.*te")) %>%
    summarise(
      Line         = "Healthcare",
      Recovery     = sum(Recovery, na.rm = TRUE),
      Inward       = sum(Inward, na.rm = TRUE),
      Retrocession = sum(Retrocession, na.rm = TRUE)
    )
  
  df_tmp <- df_tmp %>%
    filter(!str_detect(tolower(Line), "^y.*te")) %>%
    bind_rows(health_row) %>%
    bind_rows(
      tibble(
        Line         = "Personal Accident",
        Recovery     = 0,
        Inward       = 0,
        Retrocession = 0
      ))
  
  # ---------- CLEAN ----------
  df_tmp <- df_tmp %>%
    filter(!str_detect(tolower(Line), "other")) %>%
    mutate(
      Recovery = if_else(Line == "Aviation", 0, Recovery)
    )
  
  
})

#######################______________________________________ thêm Direct lấy function từ file a. ketqua.R
parse_sheet_date <- function(sheet_name) {
  
  # "31.12.2025"
  parts <- strsplit(sheet_name, "\\.")[[1]]
  
  day   <- as.integer(parts[1])
  month <- as.integer(parts[2])
  year  <- parts[3]
  
  quarter <- dplyr::case_when(
    month %in% 1:3  ~ "Q1",
    month %in% 4:6  ~ "Q2",
    month %in% 7:9  ~ "Q3",
    month %in% 10:12 ~ "Q4"
  )
  
  list(
    year = year,
    quarter = quarter,
    quarter_label = paste0(year, quarter)  # "2025Q4"
  )
}
observeEvent(input$tbh_run, {
  req(df_tmp())
  req(input$sheet)
  
  result <- tibble(
    Line = c(
      "Healthcare", "Personal Accident", "Motor Vehicles",
      "Engineering", "Fire and Misc.", "General Liability",
      "Cargo in transit", "Hull & PI"
    )
  )
  
  
  q = parse_sheet_date(input$sheet)
    
    tq <- parse_quarter(q$quarter_label)
    
    result[["Direct"]] <-  calc_all_nv(tq$year, tq$quarter,  "osc")
    # c(
    #   calc_health(tq$year, tq$quarter, "osc"),
    #   calc_pa(tq$year, tq$quarter,"osc" ),
    #   calc_xcg(tq$year, tq$quarter, "osc"),
    #   calc_eng(tq$year, tq$quarter,"osc" ),
    #   calc_fire(tq$year, tq$quarter,"osc" ),
    #   calc_misc(tq$year, tq$quarter,"osc"),
    #   calc_cargo(tq$year, tq$quarter, "osc"),
    #   calc_marine(tq$year, tq$quarter,"osc" )
    # )
  
  
  final_df <- df_tmp() %>%  left_join(result, by = "Line")
    
  # ==========================
  # PA & HEALTH & TRAVEL
  # ==========================
  pa_health_row <- final_df %>%
    filter(Line %in% c("Personal Accident", "Travel", "Healthcare")) %>%
    summarise(
      Line = "PA & Health & Travel",
      Direct = sum(Direct, na.rm = TRUE),
      Recovery = sum(Recovery, na.rm = TRUE),
      Inward = sum(Inward, na.rm = TRUE),
      Retrocession = sum(Retrocession, na.rm = TRUE)
      
    )
  
  # ==========================
  # TOTAL
  # ==========================
  total_row <- final_df %>%
    summarise(
      Line = "Total",
      Direct = sum(Direct, na.rm = TRUE),
      Recovery = sum(Recovery, na.rm = TRUE),
      Inward = sum(Inward, na.rm = TRUE),
      Retrocession = sum(Retrocession, na.rm = TRUE)
    )
  
  final_df <- bind_rows(final_df, pa_health_row, total_row)
  
  
  line_order <- c(
    "Cargo in transit",
    "Hull & PI",
    "Oil and Gas",
    "Aviation",
    "Engineering",
    "Fire and Misc.",
    "General Liability",
    "Motor Vehicles",
    "PA & Health & Travel",
    "Agriculture",
    "Personal Accident",
    "Travel",
    "Healthcare",
    "Total"
  )
  
  final_df <- final_df %>%
    mutate(Line = factor(Line, levels = line_order)) %>%
    arrange(Line)
  
  final_df <- final_df %>%
    select(
      Line,
      any_of(c("Direct", "Inward", "Recovery", "Retrocession")),
      everything()
    )
  final_df_rv(final_df)
  
  
})



output$tbh_download <- downloadHandler(
  filename = function() {
    paste0("OSC_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    openxlsx::write.xlsx(final_df_rv(), file, asTable = TRUE)
  }
)

# ---- 4. Output ----
output$result_table <- renderDT({
  req(final_df_rv())
  
  datatable(
    final_df_rv(),
    options = list(pageLength = 20, scrollX = TRUE)
  ) %>%
    formatCurrency(
      columns = 2:ncol(final_df_rv()),
      currency = "",
      interval = 3,
      mark = ",",
      digits = 0
    ) %>%
    formatStyle(
      columns = 2:ncol(final_df_rv()),
      `text-align` = "right"
    )
})





# # ---- 4. Output ----
# output$result_table <- renderDT({
#   req(final_df_rv())
#   datatable(final_df_rv(),
#             options = list(pageLength = 20, scrollX = TRUE))
# })
