#' int_groups
#'
#' Generate corresponding integer population count on each grid cell 
#' for each group based on a vector of proportions for the interest groups
#' 
#' @param POP_path The file path of non-integer population estimates.
#' @param admin_path The file path of administrative units. Each cell with a unique admin unit id.
#' @param group_path The file path of proportions for each group. If there are multiple admin units, the variable name must be set as 'id'.
#' @param result_folder The folder to save results.
#' @param method The method used to distribute population. "random" (the default) put people based on the decimal value and input population count.
#' @param workers Optional. The count of available workers (CPU cores).
#' 
#' @return 
#' A list of SpatRaster each with a single layer corresponding to each group.
#' 
#' @examples
#' POP_path <- "pop.tif" # the file path of population raster
#' admin_path <- "admin.tif" # the file path of administrative units raster
#' group_path <- "group.csv" #the file path of proportions of each group 
#' example of group data: group <- data.frame(id = 1:2,
#'                                            group_1 = runif(10),
#'                                            group_2 = 1 - group_1)
#' pop_int <- int_groups(POP_path, admin_path, group_path)
#' 
#' @importFrom utils read.csv
#' @export
int_groups <- function(POP_path,group_path,admin_path,result_folder,method,workers) {
  if (missing(POP_path)) {
    stop("Parameter 'POP_path' is required and must be provided.")
  }
  if (missing(group_path)) {
    stop("Parameter 'group_path' is required and must be provided.")
  }
  if (missing(admin_path)) {
    stop("Parameter 'admin_path' is required and must be provided. If you only have one set of proportions for the whole raster, please provide a .tif file and assign a unique value to all non-NA pixels. The value should be the id value within the group file.")
  }
  if (missing(method)) {
    method <- "random"
  }
  if (missing(result_folder)) {
    result_folder <- getwd()
  }
  output_dir <- file.path(result_folder, "popint", sub("\\.[^.]*$", "", basename(POP_path)))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  temp_dir <- file.path(result_folder, "popint/tmp")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  cat("* It may take some time to process.\n* In the meantime, it might be a good opportunity to relax and enjoy a cup of tea.\n")
  groups <- read.csv(group_path)
  if (missing(workers)) {
    plan(multisession, workers = min(availableCores() - 1, nrow(groups)))
  } else {
    plan(multisession, workers = workers)
  }
  cat("** Processing.....\n")
  result_list <- future_lapply(groups$id, function(admin_id_i) {
    POP_float <- rast(POP_path)
    admin <- rast(admin_path)
    admin_index <- admin == admin_id_i
    pop_float_admin <- POP_float[admin_index]
    pop_int_admin <- int_vector(pop_float_admin)
    rm(POP_float, admin, admin_index, pop_float_admin)
    gc()
    group_i <- as.matrix(groups[groups$id == admin_id_i, !(names(groups) %in% "id")])
    pop_int_admin_group <- disaggregate_int_vector(pop_int_admin,group_i,method)
    rm(pop_int_admin, group_i)
    gc()
    temp_file <- file.path(temp_dir, paste0(admin_id_i,".rds"))
    saveRDS(pop_int_admin_group, temp_file)
    return(temp_file)
  }, future.seed = NULL)
  cat("** Writing int pop rasters in:\n**", output_dir)
  if (missing(workers)) {
    plan(multisession, workers = min(availableCores() - 1, ncol(groups) - 1))
  } else {
    plan(multisession, workers = min(workers))
  }
  future_lapply(1:(ncol(groups)-1), function(i) {
    pop_rast <- rast(POP_path) * NA
    admin <- rast(admin_path)
    for (j in 1:nrow(groups)) {
      pop_rast[admin == groups$id[j]] <- readRDS(result_list[[j]])[, i]
    }
    writeRaster(pop_rast,
                file.path(output_dir, paste0(sub("\\.[^.]*$", "", basename(POP_path)),"_",names(groups)[i+1],".tif")),
                gdal=c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"), 
                overwrite=TRUE,
                datatype="INT4U",
                verbose=FALSE)
  })
  plan(sequential)
  invisible()
}


