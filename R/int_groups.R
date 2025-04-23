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
int_groups <- function(POP_path, group_path, admin_path,
                       result_folder = getwd(), method = "random", workers = NULL) {
  stopifnot(file.exists(POP_path),
            file.exists(group_path),
            file.exists(admin_path))
  output_dir <- file.path(result_folder, "popint", tools::file_path_sans_ext(basename(POP_path)))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  temp_dir <- file.path(result_folder, "popint", "tmp")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  groups    <- read.csv(group_path)
  admin_ids <- groups$id
  if (is.null(workers)) workers <- max(1, availableCores() - 1)
  plan(multisession, workers = workers)
  cat("* It may take some time to process.\n* In the meantime, it might be a good opportunity to relax and enjoy a cup of tea.\n")
  cat("** Processing.....\n")
  handlers("txtprogressbar")
  result_list <- with_progress({
    p <- progressor(along = admin_ids)
    future_lapply(seq_along(admin_ids), function(i) {
      admin_id <- admin_ids[i]
      POP_float <- rast(POP_path)
      admin <- rast(admin_path)
      cell_idx <- terra::cells(admin, admin_id)[[1]]
      pop_int_admin <- int_vector(as.matrix(POP_float[cell_idx]))
      group_i       <- as.matrix(groups[i, -1])
      pop_int_admin_group <- disaggregate_int_vector(pop_int_admin, group_i, method)
      out_mat  <- cbind(matrix(cell_idx,ncol=1), pop_int_admin_group)
      dimnames(out_mat)[[2]][1] <- "cell"
      tmp_file <- file.path(temp_dir, paste0(admin_id, ".rds"))
      saveRDS(out_mat, tmp_file, compress = TRUE)
      p()
    }, future.seed = TRUE)
  })
  plan(sequential)
  cat("** Writing int pop rasters in:\n**", output_dir)
  POP_float <- rast(POP_path)
  admin     <- rast(admin_path)
  ncell_tot  <- ncell(admin)
  group_names <- names(groups)[-1]
  ng <- length(group_names)
  ff_path <- file.path(result_folder, "popint", "pop_matrix.ffdata")
  if (file.exists(ff_path)) unlink(ff_path,force = T)
  mat_ff  <- ff(vmode = "integer",
                dim   = c(ncell_tot, ng),
                filename = ff_path,
                initdata = NA)
  handlers("txtprogressbar")
  with_progress({
    p <- progressor(along = admin_ids)
    for (id in admin_ids) {
      dat <- readRDS(file.path(temp_dir, paste0(id, ".rds")))
      mat_ff[dat[, 1], ] <- dat[, -1]
      gc()
      p()
    }
  })
  close(mat_ff)
  bs <- blocks(admin)
  nc <- ncol(admin)
  with_progress({
    p <- progressor(steps = ng*bs$n)
    for (g in seq_along(group_names)) {
      out   <- rast(POP_path) * NA
      fname <- file.path(output_dir,paste0(tools::file_path_sans_ext(basename(POP_path)),"_",group_names[g],".tif"))
      writeStart(out, filename = fname,
                 gdal = c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                 overwrite = TRUE,
                 datatype = "INT4U",
                 verbose = FALSE)
      for (b in 1:bs$n) {
        row0 <- bs$row[b]
        nr   <- bs$nrows[b]
        cell_idx <- ((row0 - 1L) * nc + 1L) : ((row0 + nr - 1L) * nc)
        vals <- mat_ff[cell_idx, g]
        writeValues(out, v = vals, start = bs$row[b], nrows = bs$nrows[b])
        p()
      }
      writeStop(out)
    } 
  })
  if (file.exists(ff_path)) unlink(ff_path,force = T)
  invisible()
  cat("** Done!")
}
