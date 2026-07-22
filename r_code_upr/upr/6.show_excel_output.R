folder_path <- "www/output_excel"

# reactiveVal lưu danh sách file
files_rv <- reactiveVal(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
# Render bảng
output$file_table <- renderDT({
  files <- files_rv()
  if (length(files) == 0) {
    return(datatable(data.frame(Message = "Không có file nào"), 
                     options = list(dom = 't'), rownames = FALSE))
  }
  dat <- data.frame(
    File = files,
    Download = sprintf("<a href='output_excel/%s' download>⬇ Tải về</a>", files),
    stringsAsFactors = FALSE
  )
  datatable(
    dat,
    escape = FALSE,
    rownames = FALSE,
    selection = "multi",
    options = list(
      dom = 't',
      paging = FALSE,
      scrollY = '300px',
      scrollCollapse = TRUE
    )
  )
})

# Download handler
observe({
  files <- files_rv()
  lapply(files, function(file_name) {
    output[[paste0("dl_", file_name)]] <- downloadHandler(
      filename = function() file_name,
      content = function(file) {
        file.copy(file.path(folder_path, file_name), file)
      }
    )
  })
})



observeEvent(input$delete_selected, {
  sel <- input$file_table_rows_selected
  if (length(sel) == 0) {
    showNotification("⚠ Chưa chọn file để xóa!", type = "warning")
    return()
  }
  
  files <- files_rv()
  files_to_delete <- files[sel]
  paths_to_delete <- file.path(folder_path, files_to_delete)
  
  # In debug
  print(paths_to_delete)
  print(file.exists(paths_to_delete))
  
  # Xóa file
  success <- file.remove(paths_to_delete)
  
  if (all(success)) {
    showNotification(paste("✅ Đã xóa", length(files_to_delete), "file!"), type = "message")
  } else {
    showNotification("❌ Không thể xóa một số file. Kiểm tra quyền hoặc trạng thái file.", type = "error")
  }
  
  # Cập nhật danh sách
  files_rv(list.files(folder_path, pattern = "\\.xlsx$", full.names = FALSE))
})


# Xóa toàn bộ file
observeEvent(input$delete_all, {
  if (length(files_rv()) == 0) {
    showNotification("⚠ Không có file nào để xóa!", type = "warning")
    return()
  }
  
  # Modal xác nhận
  showModal(modalDialog(
    title = "⚠ Xác nhận xóa toàn bộ",
    "Bạn có chắc chắn muốn xóa tất cả các file? Hành động này không thể hoàn tác.",
    easyClose = FALSE,
    footer = tagList(
      modalButton("Hủy"),
      actionButton("confirm_delete_all", "Xóa hết", class = "btn-danger")
    )
  ))
})

# Khi xác nhận xóa hết
observeEvent(input$confirm_delete_all, {
  removeModal()
  all_files <- files_rv()
  file.remove(file.path(folder_path, all_files))
  files_rv(character(0))  # Cập nhật reactiveVal
  showNotification("✅ Đã xóa toàn bộ file!", type = "message")
})





output$download_all <- downloadHandler(
  filename = function() {
    paste0("all_results_", Sys.Date(), ".zip")
  },
  content = function(file) {
    shinyjs::show("loading_msg")
    
    folder_path <- "www/output_excel"
    
    files <- list.files(
      folder_path,
      pattern = "\\.xlsx$",
      full.names = TRUE
    )
    
    if (length(files) == 0) {
      shinyjs::hide("loading_msg")
      stop("Không có file nào để tải.")
    }
    
    zip::zipr(zipfile = file, files = files)
    
    shinyjs::hide("loading_msg")
  }
)

# output$download_all <- downloadHandler(
#   filename = function() {
#     paste0("all_results_", Sys.Date(), ".zip")
#   },
#   content = function(file) {
#     # Hiện thông báo chờ
#     shinyjs::show("loading_msg")
#    
#     #files<- files_rv()
#     if (length(files) == 0) {
#       stop("Không có file nào để tải.")
#     }
#     
#     zip_files <- file.path(folder_path, files)
#     
#     # Đảm bảo các file tồn tại
#     existing_files <- zip_files[file.exists(zip_files)]
#     if (length(existing_files) == 0) {
#       stop("Không tìm thấy file nào để nén.")
#     }
#     
#     # Nén file
#     zip::zipr(zipfile = file, files = existing_files)
#     # Ẩn loading sau khi xong
#     shinyjs::hide("loading_msg")
#   }
# )
















