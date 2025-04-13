#' int_grid
#'
#' Convert Floating Population Estimates into Integers
#'
#' @param POP_path The file path of non-integer population estimates.
#' @param admin_path The file path of administrative units. Each cell with a unique admin unit id.
#' @param result_folder The folder to save results.
#' @param workers Optional. The count of available workers (CPU cores).
#' 
#' @return The integer population raster will be saved in specified results folder.
#' 
#' @examples
#' POP_path <- "pop.tif" # the file path of population raster
#' pop_int <- int_grid(POP_path) # if admin units are unavailable
#' admin_path <- "admin.tif" # the file path of administrative units raster
#' pop_int <- int_grid(POP_path, admin_path)
#' 
#' @importFrom utils read.csv
#' @export
int_grid <- function(POP_path,admin_path,result_folder,workers) {
  if (missing(POP_path)) {
    stop("Error: Parameter 'POP_path' is required and must be provided.")
  }
  if (missing(result_folder)) {
    result_folder <- getwd()
  }
  cat("* It may take some time to process.\n* In the meantime, it might be a good opportunity to relax and enjoy a cup of tea.\n")
  cat("** Processing.....\n")
  if (missing(admin_path)) {
    POP_float <- rast(POP_path)
    pop_float_admin <- values(POP_float)
    pop_int_admin <- int_vector(pop_float_admin)
    value(POP_float) <- pop_int_admin
  } else {
    admin_id <- terra::unique(rast(admin_path))
    admin_id <- as.matrix(admin_id)
    if (missing(workers)) {
      plan(multisession, workers = min(availableCores() - 1, nrow(admin_id)))
    } else {
      plan(multisession, workers = workers)
    }
    result_list <- future_lapply(admin_id, function(admin_id_i) {
      POP_float <- rast(POP_path)
      admin <- rast(admin_path)
      admin_index <- admin == admin_id_i
      pop_float_admin <- POP_float[admin_index]
      pop_int_admin <- int_vector(pop_float_admin)
      rm(POP_float, admin, admin_index, pop_float_admin)
      gc()
      return(pop_int_admin)
    })
    plan(sequential)
    POP_float <- rast(POP_path)
    admin <- rast(admin_path)
    for (i in 1:length(result_list)) {
      POP_float[admin == admin_id[i]] <- result_list[[i]]
    }
  }
  output_dir <- file.path(result_folder, "popint", names(POP_float))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("** Writing int pop rasters in:\n**", output_dir)
  NAflag(POP_float) <- -1
  writeRaster(POP_float,
              file.path(output_dir, paste0(names(POP_float),"_int.tif")),
              gdal=c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"), 
              overwrite=TRUE,
              datatype="INT4U",
              NAflag=-1,
              verbose=FALSE)
  invisible()
}
