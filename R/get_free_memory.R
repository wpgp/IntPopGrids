#' get_free_memory
#'
#' A complete cross-platform function that checks the OS, retrieves the free memory
#' 
#' @return 
#' The free memory (MB).
#' 
#' @examples
#' free_mem <- get_free_memory()
#' 
#' @export

get_free_memory <- function() {
  os <- Sys.info()['sysname']
  
  if (os == "Linux") {
    # Extract free memory for Linux
    free_mem <- system("free -m | awk '/Mem:/ {print $4}'", intern = TRUE)
    free_mem <- as.numeric(free_mem)
    
  } else if (os == "Darwin") {  # macOS
    # Extract free memory for macOS and convert from pages to MB
    free_mem <- system("vm_stat | awk '/free/ {print $3}'", intern = TRUE)
    free_mem <- as.numeric(gsub("\\.", "", free_mem)) * 4096 / 1024^2  # Convert pages to MB
    
  } else if (os == "Windows") {
    # Extract free memory for Windows
    free_mem <- system("wmic OS get FreePhysicalMemory /Value", intern = TRUE)
    free_mem <- free_mem[grep("FreePhysicalMemory", free_mem)]  # Extract the correct line
    free_mem <- as.numeric(gsub("FreePhysicalMemory=", "", free_mem)) / 1024  # Convert KB to MB
    
  } else {
    stop("Unsupported OS")
  }
  
  return(free_mem)
}

