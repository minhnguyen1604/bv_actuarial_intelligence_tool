library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(janitor)
library(DT)
library(lubridate)
library(waiter)
library(shinycssloaders)
library(shinyjs)
library(knitr)
library(kableExtra)
library(tidyr)
library(readr)
library(writexl)
library(ggplot2)


options(shiny.maxRequestSize = 1000 * 1024^2)  # 100 MB limit




convert_excel_or_string_date <- function(x) {
  # x có thể là số Excel (42979) hoặc chuỗi "dd/mm/YYYY" hoặc "YYYY-mm-dd"
  res <- rep(as.Date(NA), length(x))
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  
  is_num <- !is.na(x_num)
  # Excel serial -> Date
  res[is_num] <- as.Date(x_num[is_num], origin = "1899-12-30")
  # Chuỗi ngày -> try dmy, ymd, fallback to as.Date
  res[!is_num] <- suppressWarnings(
    ifelse(
      !is.na(dmy(x_chr[!is_num])),
      as.Date(dmy(x_chr[!is_num])),
      as.Date(ymd(x_chr[!is_num]))
    ))
  # better: try dmy then ymd with tryCatch if needed; above is simple
  return(res)
}





# Hàm chuyển 3 cột _Ngay / _Thang / _Nam thành 1 cột Date mới
combine_date_cols <- function(df) {
  # Tìm các nhóm cột có hậu tố _Ngay, _Thang, _Nam
  base_names <- df %>%
    names() %>%
    str_extract("^[A-Za-z0-9_]+(?=_Ngay$)") %>%  # Lấy phần gốc trước "_Ngay"
    na.omit() %>%
    unique()
  
  for (base in base_names) {
    day_col   <- paste0(base, "_Ngay")
    month_col <- paste0(base, "_Thang")
    year_col  <- paste0(base, "_Nam")
    
    # Kiểm tra xem 3 cột có tồn tại không
    if (all(c(day_col, month_col, year_col) %in% names(df))) {
      new_col <- base  # tên cột mới (ví dụ: "NgayTonThat")
      
      df <- df %>%
        mutate(
          # Chuyển sang số an toàn (NA nếu không hợp lệ)
          across(all_of(c(day_col, month_col, year_col)), ~ suppressWarnings(as.numeric(.x))),
          
          # Tạo cột mới dạng Date
          !!new_col := suppressWarnings(
            make_date(
              year = .data[[year_col]],
              month = .data[[month_col]],
              day = .data[[day_col]]
            )
          )
        )
    }
  }
  return(df)
}


get_valid_sheet <- function(path) {
  info <- excel_sheets(path)
  for (s in info) {
    # đọc 1 dòng đầu thôi
    tmp <- suppressMessages(read_excel(path, sheet = s, n_max = 10))
    if (ncol(tmp) > 10) return(s)
  }
  return(NULL)
}



# Hàm làm sạch dữ liệu
clean_df <- function(df) {
  colnames_clean <- c(
    "stt", "nv","phan_cap", "so_ho_so_1", "so_ho_so_2","so_ho_so_3","so_ho_so_4","so_ho_so_5", "so_don_bao_hiem", "nguoi_duoc_bao_hiem",
    "ten_ton_that", "don_hieu_luc_tu_ng", "don_hieu_luc_tu_th", "don_hieu_luc_tu_nam",
    "don_hieu_luc_den_ng", "don_hieu_luc_den_th", "don_hieu_luc_den_nam",
    "so_tien_bao_hiem_vnd", "so_tien_bao_hiem_usd",
    "don_vi_cap_don",
    "ngay_ton_that_ng", "ngay_ton_that_th", "ngay_ton_that_nam",
    "ngay_thong_bao_ng", "ngay_thong_bao_th", "ngay_thong_bao_nam",
    "doi_tuong_bi_ton_that", "nguyen_nhan", "tich_tu","thanh_toan",
    "ngay_thanh_toan_dang_ky_ng", "ngay_thanh_toan_dang_ky_th", "ngay_thanh_toan_dang_ky_nam",
    "so_tien_net_vnd", "so_tien_net_usd",
    "ty_le_dbh_cua_bv", "cong_ty_lead",
    "du_phong_boi_thuong_vnd", "du_phong_boi_thuong_usd",
    "boi_thuong_da_tra_vnd", "boi_thuong_da_tra_usd",
    "boi_thuong_con_lai_vnd", "boi_thuong_con_lai_usd",
    "boi_thuong_con_lai_cua_bao_viet_vnd","boi_thuong_con_lai_cua_bao_viet_usd",
    "giam_dinh","du_chi_phi_giam_dinh_net_vnd", "du_chi_phi_giam_dinh_net_usd",
    "nguoi_theo_doi",
    "ngay_dong_ho_so_ng", "ngay_dong_ho_so_th", "ngay_dong_ho_so_nam",
    "phi_giam_dinh_da_tra_vnd", "phi_giam_dinh_da_tra_usd",
    "phi_giam_dinh_con_lai_vnd", "phi_giam_dinh_con_lai_usd",
    "du_chi_phi_giam_dinh_con_lai_cua_bao_viet_vnd",
    "du_chi_phi_giam_dinh_con_lai_cua_bao_viet_usd"
  )
  t <- which(grepl("tt", df[[1]], ignore.case = TRUE)) + 2
  df <- df[t:nrow(df), ]
  if (ncol(df) > length(colnames_clean)) {
    df <- df[, 1:length(colnames_clean)]
  }
  colnames(df) <- colnames_clean[1:ncol(df)]
  df
}


#_________________________________________________ cargo
combine_date <- function(dd, mm, yy) {
  ifelse(
    !is.na(dd) & dd != "",
    sprintf("%02d/%02d/%04d", as.numeric(dd), as.numeric(mm), as.numeric(yy)),
    ifelse(
      !is.na(mm) & mm != "",
      sprintf("%02d/%04d", as.numeric(mm), as.numeric(yy)),
      NA
    )
  )
}

process_sheet <- function(file_path, sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name, col_types = "text")
  # Nếu sheet trống hoặc có ít hơn 1 cột, bỏ qua
  if (ncol(df) < 1 || nrow(df) < 1 || all(is.na(df))) {
    return(NULL)
  }
  
  if (ncol(df) <= 20) return(NULL)
  a = which(is.na(df[[1]]))[1]
  b = which(grepl("STT|^NO", df[[1]], ignore.case = TRUE))[1]
  c = which(grepl("^1", df[[1]], ignore.case = TRUE))[1]
  c1 = which(grepl("^2", df[[1]], ignore.case = TRUE))[1]
  
  # if (nrow(df) < 10) return(NULL)
  d= min(a,b)
  #cat(sheet_name, ":  a =",a,  "\n", "b =",b,  "\n", "c =",c,  "\n", "d =",d,  "\n", "c1 =", c1,  "\n")
  header_rows <- df[d:(c-1), ]
  
  new_names <- apply(header_rows, 2, function(col) {
    col <- col[!is.na(col)]
    paste(col, collapse = " | ")
  })
  new_names <- make.unique(new_names)
  
  if (!is.na(c) && !is.na(c1) ) {
    if (c == c1 - 1){
    c <- c - 1
    }
    
  }
  
  df <- df[-(1:c), ]
  
  colnames(df) <- new_names
  # Xóa các dòng mà cả cột 1 và cột 2 đều NA
  df <- df[!(is.na(df[[1]]) & is.na(df[[2]])), , drop = FALSE]
  
  df= clean_names(df)
  
  
  pos <- grep("note", names(df))
  #cat(pos, " ", ncol(df) ,"\n")
  if (length(pos) == 0) {
    pos <- ncol(df)
  }
  
  df <- df[, 1:pos]
  
  new_names <- colnames(df)
  new_names[grepl("^nam_n.*v*", new_names, ignore.case = TRUE)] <- "nam_nv"
  new_names[grepl("so_nb|so_don_bh", new_names, ignore.case = TRUE)] <- "so_nb"
  new_names[grepl("hsbt|so_ho_so", new_names, ignore.case = TRUE)] <- "so_hsbt"
  new_names[grepl("so_tien_bao_hiem|stbh", new_names, ignore.case = TRUE)] <- "stbh"
  new_names[grepl("so_tien_boi_thuong_da_tra", new_names, ignore.case = TRUE)] <- "stbt_da_tra"
  new_names[grepl("tong_so_tien_boi_thuong_quy_doi", new_names, ignore.case = TRUE)] <- "tong_so_tien_boi_thuong_quy_doi_vnd"
  
  #new_names[grepl("dong.*bao.*hiem.*ty.*le|dbh", new_names, ignore.case = TRUE)] <- "dbh"
  colnames(df) = new_names
  
  
  # Tìm vị trí các cột kết thúc bằng "_dd"
  dd_pos <- grep("_dd$", names(df))
  to_drop =c()
  # Lặp qua các cột dd và ghép
  for (i in dd_pos) {
    dd_col <- names(df)[i]
    mm_col <- names(df)[i + 1]
    yy_col <- names(df)[i + 2]
    
    new_col <- sub("_dd$", "", dd_col)   # Tạo tên cột mới
    
    df[[new_col]] <- combine_date(
      df[[dd_col]],
      df[[mm_col]],
      df[[yy_col]]
    )
    
    # Ghi nhận các cột cũ để xóa sau
    to_drop <- c(to_drop, dd_col, mm_col, yy_col)
  }
  
  # Xóa các cột cũ (chỉ xóa những cột thực sự tồn tại)
  to_drop <- intersect(to_drop, names(df))
  df <- df[ , setdiff(names(df), to_drop), drop = FALSE]
  
  
  df$source = sheet_name
  return(df)
}

#_________________________________________________ marine


process_marine <- function(file_path, sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name, col_types = "text")
  
  # Nếu số cột <= 20 thì bỏ qua
  if (ncol(df) <= 20) return(NULL)
  if (ncol(df) == 0) return(NULL)
  
  d = which(grepl("STT|^NO", df[[1]], ignore.case = TRUE))[1]
  c = which(grepl("^1$", df[[1]], ignore.case = TRUE))[1]
  c1 = which(grepl("^2$", df[[1]], ignore.case = TRUE))[1]
  
 
  if (is.na(c)) return(NULL)
  #   cat(sheet_name, ":  c =",c,  "\n", "d =",d,  "\n")
  
  header_rows <- df[d:(c-1), ]
  
  new_names <- apply(header_rows, 2, function(col) {
    col <- col[!is.na(col)]
    paste(col, collapse = " | ")
  })
  new_names <- make.unique(new_names)
  
  if (!is.na(c) && !is.na(c1) && c == c1 - 1) {
    c <- c - 1
  }
  
  df <- df[-(1:c), ]
  
  colnames(df) <- new_names
  df= clean_names(df)
  
  colnames(df) <- str_remove_all(colnames(df), "_\\d+_*\\d*")
  names(df) <- make.unique(names(df))
  
  # Xóa các dòng mà cả cột 1 và cột 2 đều NA
  df <- df[!(is.na(df[[1]]) & is.na(df[[2]])), , drop = FALSE]
  new_names <- colnames(df)
  new_names[grepl("stt|^no", new_names, ignore.case = TRUE)] <- "stt"
  new_names[grepl("nghiep_vu", new_names, ignore.case = TRUE)] <- "nghiep_vu"
  new_names[grepl("so_don|policy_number", new_names, ignore.case = TRUE)] <- "so_nb"
  new_names[grepl("so_khieu_nai|claim_urn", new_names, ignore.case = TRUE)] <- "so_khieu_nai_ij"
  new_names[grepl("thoi_han.*tu|from|ngay_hieu_luc", new_names, ignore.case = TRUE)] <- "thoi_han_bao_hiem_tu"
  new_names[grepl("ngay_het_hieu_luc|den$|to$", new_names, ignore.case = TRUE)] <- "thoi_han_bao_hiem_den"
  new_names[grepl("so_tien_bao_hiem", new_names, ignore.case = TRUE)] <- "STBH"
  new_names[grepl("ngay_ton_that|ngay_tai_nan|date.*loss", new_names, ignore.case = TRUE)] <- "ngay_ton_that"
  new_names[grepl("ten_tau|vessel", new_names, ignore.case = TRUE)] <- "ten_tau"
  new_names[grepl("so_tien_bao_hiem|sum_insured", new_names, ignore.case = TRUE)] <- "STBH"
  #____add
  new_names[grepl("^insured|nguoi_duoc_bao_hiem$|ten_khach_hang", new_names, ignore.case = TRUE)] <- "nguoi_duoc_bao_hiem"
  new_names[grepl("^age|tuoi_tau", new_names, ignore.case = TRUE)] <- "tuoi_tau"
  new_names[grepl("nguyen_nhan_ton_that|cause_of_loss", new_names, ignore.case = TRUE)] <- "nguyen_nhan_ton_that"
  
  colnames(df) = new_names
  
  df
}

# Hàm xử lý 1 file
process_file <- function(file_path) {
  sheets <- excel_sheets(file_path)
  data_list <- lapply(sheets, function(sh) process_marine(file_path, sh))
  names(data_list) <- sheets
  # Bỏ sheet NULL
  data_list <- data_list[!sapply(data_list, is.null)]
  data_list
}


safe_date <- function(y, m, d) {
  
  # 👉 ép numeric
  y <- suppressWarnings(as.numeric(y))
  m <- suppressWarnings(as.numeric(m))
  d <- suppressWarnings(as.numeric(d))
  
  # 👉 điều kiện hợp lệ
  valid <- !is.na(y) & !is.na(m) & !is.na(d)
  
  result <- rep(as.Date(NA), length(y))
  
  # 👉 tạo date an toàn
  result[valid] <- as.Date(
    sprintf("%04d-%02d-%02d", y[valid], m[valid], d[valid]),
    format = "%Y-%m-%d"
  )
  
  return(result)
}

safe_col <- function(df, col) {
  if (col %in% names(df)) {
    return(df[[col]])
  } else {
    return(rep(NA, nrow(df)))
  }
}

safe_date_char <- function(x) {
  
  x[x == ""] <- NA  # xử lý chuỗi rỗng
  
  # thử nhiều format phổ biến
  as.Date(x, tryFormats = c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d"
  ))
}
























































