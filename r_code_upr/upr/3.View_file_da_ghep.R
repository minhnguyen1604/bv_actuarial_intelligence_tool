#rds_folder <- paste0("www/cur_data_",date_now())

# Giả sử date_now là reactiveVal hoặc reactive
rds_folder <- reactive({
  paste0("www/cur_data_", date_now())
})

# Danh sách mong đợi
expected_files <- c(
  "Eng_LT", "Eng_ST",
  "Marine_LT", "Marine_ST",
  "Fire_LT", "Fire_ST", "Vietjet",
  "Misc_LT", "Misc_ST",
  "XCG_CWVN_LT", "XCG_LT", "XCG_ST",
  "PA_LT","PA_NNTX_LT","PA_NNTX_ST",
  "Travel_BHTT","PA_ST","PA_TTTBVV",
  "Travel_CTTV")

# Theo dõi các file trong cur_data
watch_rds_files <- reactivePoll(
  intervalMillis = 2000,
  session = session,
  checkFunc = function() {
    list.files(rds_folder(), pattern = "\\.rds$", full.names = FALSE)
  },
  valueFunc = function() {
    tools::file_path_sans_ext(
      list.files(rds_folder(), pattern = "\\.rds$", full.names = FALSE)
    )
  }
)


# Tạo bảng trạng thái (Term dùng để chia bảng, nhưng không hiển thị)
file_status <- reactive({
  existing_files <- watch_rds_files()
  
  data.frame(
    File = expected_files,
    Term = ifelse(grepl("LT", expected_files), "Long", "Short"),
    Available = ifelse(expected_files %in% existing_files, "✅", "❌"),
    stringsAsFactors = FALSE
  )
})

# Bảng Long Term (ẩn cột Term)
output$table_lt <- renderDT({
  DT::datatable(
    subset(file_status(), Term == "Long")[, c("File", "Available")],
    rownames = FALSE,
    options =  list(scrollY = "300px", paging = FALSE) #list(dom = 't', pageLength = 15)
  )
})

output$table_st <- renderDT({
  DT::datatable(
    subset(file_status(), Term == "Short")[, c("File", "Available")],
    rownames = FALSE,
    options = list(scrollY = "300px", paging = FALSE) #list(dom = 't', pageLength = 15)
  )
})

observeEvent(input$refresh_data, {
  
  # Lấy danh sách file .rds
  rds_files <- list.files(rds_folder(), pattern = "\\.rds$", full.names = TRUE)
  
  # Xoá từng file
  if (length(rds_files) > 0) {
    file.remove(rds_files)
    showNotification("Đã xoá toàn bộ file .rds trong cur_data", type = "message")
  } else {
    showNotification("Không có file .rds nào trong cur_data để xoá", type = "warning")
  }
})