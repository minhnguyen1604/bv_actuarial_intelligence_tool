# library(shiny)
# library(dplyr)
# library(tidyr)
# library(DT)
# library(stringr)
# library(stringi)
# library(openxlsx)
# library(rhandsontable)
# library(later)
# library(future)
# library(lubridate)
# library(RDCOMClient)
# library(shinydashboard)
# library(RDCOMClient)
# library(promises)
# library(shinyjs)
# library(readxl)
# Danh sách các gói cần dùng
packages <- c(
  "shiny", "dplyr", "tidyr", "DT", "stringr", "stringi", "openxlsx",
  "rhandsontable", "later", "future", "lubridate", "RDCOMClient",
  "shinydashboard", "promises", "shinyjs", "readxl"
)

# Kiểm tra gói đã cài chưa
installed_packages <- rownames(installed.packages())

# Tìm các gói còn thiếu
missing_packages <- packages[!(packages %in% installed_packages)]

# Cài các gói còn thiếu
if(length(missing_packages)) {
  install.packages(missing_packages)
}

# Load tất cả các gói
lapply(packages, library, character.only = TRUE)

options(shiny.maxRequestSize = 100 * 1024^2)  # Tăng giới hạn upload lên 30 MB

# Hàm lấy quý trước
pre_quarter <- function(now_str) {
  # Tách quý và năm
  parts <- strsplit(now_str, "_")[[1]]
  q <- as.integer(sub("Q", "", parts[1]))
  year <- as.integer(parts[2])
  
  # Lùi quý
  if (q == 1) {
    q <- 4
    year <- year - 1
  } else {
    q <- q - 1
  }
  
  # Trả về theo định dạng bạn muốn
  paste0("Q", q, "_", year)
}

last_values <- if (file.exists("dpnv_last_values.rds")) {
  readRDS("dpnv_last_values.rds")
} else {
  list(ngay = 30, thang = 6, nam = 2025) # giá trị mặc định nếu chưa có file
}

convert_to_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  
  results <- sapply(seq_along(x), function(i) {
    s <- x[i]
    if (is.na(s) || trimws(s) == "") return(NA_real_)
    s0 <- gsub("[[:space:]]", "", s)
    
    # xử lý dấu âm hoặc ngoặc
    sign <- 1
    if (grepl("^\\(.*\\)$", s0)) {
      sign <- -1
      s0 <- sub("^\\(", "", s0)
      s0 <- sub("\\)$", "", s0)
    }
    if (grepl("^-", s0)) {
      sign <- -1
      s0 <- sub("^-", "", s0)
    }
    
    # nếu không có dấu , hoặc .
    if (!grepl("[.,]", s0)) {
      val <- suppressWarnings(as.numeric(s0))
      return(sign * val)
    }
    
    has_dot <- grepl("\\.", s0)
    has_comma <- grepl(",", s0)
    
    if (has_dot && has_comma) {
      # có cả 2 loại dấu: dấu cuối cùng là thập phân
      seps <- unlist(gregexpr("[.,]", s0))
      last_pos <- tail(seps, 1)
      chars <- unlist(strsplit(s0, ""))
      out <- character(0)
      for (j in seq_along(chars)) {
        ch <- chars[j]
        if (ch %in% c(".", ",")) {
          if (j == last_pos) {
            out <- c(out, ".")
          } else next
        } else {
          out <- c(out, ch)
        }
      }
      clean <- paste0(out, collapse = "")
    } else {
      
      # chỉ có 1 loại dấu
      sep <- if (has_dot) "\\." else ","
      # pos <- regexpr(sep, s0)
      # after <- substr(s0, pos + 1, nchar(s0))
      if (has_dot) {
        pos_list <- unlist(gregexpr("\\.", s0))
      } else {
        pos_list <- unlist(gregexpr(",", s0))
      }
      pos_list <- pos_list[pos_list > 0]
      last_pos <- tail(pos_list, 1)
      after <- substring(s0, last_pos + 1)
      # nếu có 1-2 chữ số sau dấu → thập phân, nếu >=3 thì dấu nghìn
      if (grepl("^[0-9]{3}$", after)) {
        # ngăn nghìn
        if(sep == ","){
        clean <- gsub(sep, "", s0)
        }else clean <- s0
      } else {
        
        # thập phân
        if (sep == ",") clean <- sub(",", ".", s0) else clean <- s0
      }
    }
    
    val <- suppressWarnings(as.numeric(clean))
    sign * val
  }, USE.NAMES = FALSE)
  
  # In các giá trị không chuyển được
  bad_idx <- which(is.na(results) & !is.na(x) & trimws(x) != "")
  if (length(bad_idx) > 0) {
    cat("⚠️ Các giá trị không chuyển được (NA):\n")
    print(data.frame(index = bad_idx, value = x[bad_idx]))
  }
  print(bad_idx)
  return(results)
}


kiem_tra_so_tien <- function(x) {
  
  if(is.na(x) || trimws(x) == "") {
    return("Hợp lệ")
  }
  
  if(grepl("[A-Za-z]", x)) {
    return("Chứa chữ")
  }
  
  if(grepl("[^0-9,.-]", x)) {
    return("Ký tự không hợp lệ")
  }
  
  x_num <- suppressWarnings(as.numeric(gsub(",", "", x)))
  
  if(is.na(x_num)) {
    return("Không chuyển được sang số")
  }
  return("Hợp lệ")
}













# convert_to_numeric<- function(x) {
#   if (is.factor(x)) x <- as.character(x)
#   if (is.numeric(x)) return(x)
#   x <- as.character(x)
#   
#   sapply(x, function(s) {
#     if (is.na(s) || trimws(s) == "") return(NA_real_)
#     s0 <- gsub("[[:space:]]", "", s)
#     
#     # xử lý dấu âm hoặc ngoặc
#     sign <- 1
#     if (grepl("^\\(.*\\)$", s0)) {
#       sign <- -1
#       s0 <- sub("^\\(", "", s0)
#       s0 <- sub("\\)$", "", s0)
#     }
#     if (grepl("^-", s0)) {
#       sign <- -1
#       s0 <- sub("^-", "", s0)
#     }
#     
#     # nếu không có dấu , hoặc .
#     if (!grepl("[.,]", s0)) {
#       return(sign * suppressWarnings(as.numeric(s0)))
#     }
#     
#     has_dot <- grepl("\\.", s0)
#     has_comma <- grepl(",", s0)
#     
#     if (has_dot && has_comma) {
#       # có cả 2 loại dấu: dấu cuối cùng là thập phân
#       seps <- unlist(gregexpr("[.,]", s0))
#       last_pos <- tail(seps, 1)
#       chars <- unlist(strsplit(s0, ""))
#       out <- character(0)
#       for (i in seq_along(chars)) {
#         ch <- chars[i]
#         if (ch %in% c(".", ",")) {
#           if (i == last_pos) {
#             out <- c(out, ".")  # dấu thập phân
#           } else {
#             next  # bỏ ngăn nghìn
#           }
#         } else {
#           out <- c(out, ch)
#         }
#       }
#       clean <- paste0(out, collapse = "")
#     } else {
#       # chỉ có 1 loại dấu → tất cả là ngăn nghìn
#       clean <- gsub("[.,]", "", s0)
#     }
#     
#     val <- suppressWarnings(as.numeric(clean))
#     sign * val
#   }, USE.NAMES = FALSE)
# }
