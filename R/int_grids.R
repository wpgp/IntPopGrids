#' int_grid
#'
#' Convert Floating Population Estimates into Integers
#'
#' @param POP_path The file path of non-integer population estimates.
#' @param admin_path The file path of administrative units. Each cell with a unique admin unit id.
#' @param result_folder The folder to save results.
#' @param workers Optional. The count of available workers (CPU cores).
#' @param method The rounding method to use: either 'worldpop' (floor + redistribute) or 'landscan' (round + adjust).
#'
#' @return The integer population raster will be saved in specified results folder.
#'
#' @examples
#' POP_path <- "pop.tif" # the file path of population raster
#' pop_int <- int_grid(POP_path, method = "worldpop")
#' admin_path <- "admin.tif" # the file path of administrative units raster
#' pop_int <- int_grid(POP_path, admin_path, method = "landscan")
#'
#' @importFrom utils read.csv
#' @export
int_grid <- function(POP_path, admin_path = NULL, result_folder = getwd(), workers = NULL, method) {
  
  if (missing(method)) {
    cat("Please select a integerisation method:\n")
    cat("  1: WorldPop (floor + redistribute)\n")
    cat("  2: LandScan (round + adjust)\n")
    selection <- readline("Enter 1 or 2: ")
    if (selection == "1") {
      method <- "worldpop"
    } else if (selection == "2") {
      method <- "landscan"
    } else {
      stop("Invalid selection. Please enter 1 or 2.")
    }
  }
  if (missing(POP_path)) {
    stop("Error: Parameter 'POP_path' is required and must be provided.")
  }
  output_dir <- file.path(result_folder, tools::file_path_sans_ext(basename(POP_path)))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  temp_dir <- file.path(result_folder, "tmp")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  fname <- file.path(output_dir,paste0(tools::file_path_sans_ext(basename(POP_path)),"_",method,"_int.tif"))
  
  cat("** Processing.....\n")
  POP_float <- rast(POP_path)
  if (is.null(admin_path)) {
    pop_float_values <- values(POP_float)
    if (method == "worldpop") {
      pop_int_values <- int_vector(pop_float_values)
      values(POP_float) <- pop_int_values
    } else {
      n_cells <- length(pop_float_values)
      pop_int_values <- round(pop_float_values)
      pop_int_sum <- sum(pop_int_values, na.rm=T)
      pop_sum <- sum(pop_float_values, na.rm=T)
      diff <- round(pop_sum - pop_int_sum)
      idx <- order(pop_float_values - pop_int_values, decreasing = (diff < 0))
      adjustment <- rep(0, n_cells)
      adjustment[idx[seq_len(abs(diff))]] <- sign(diff)
      pop_int_values <- pop_int_values + adjustment
      values(POP_float) <- pop_int_values
    }
    cat("** Writing int pop rasters in:\n**", output_dir, "\n")
    writeRaster(POP_float,
                filename = fname,
                gdal = c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                overwrite = TRUE,
                verbose = FALSE)
    invisible()
    cat("** Done!")
  } else {
    admin_rast <- rast(admin_path)
    pop_rast <- c(POP_float, admin_rast)
    df_int <- tryCatch({
      df <- as.data.frame(pop_rast, cells = TRUE, na.rm = TRUE)
      colnames(df) <- c("cell", "pop", "admin")
      if (method == "worldpop") {
        df %>%
          mutate(pop_int = floor(pop),
                 pop_dec = pop - pop_int) %>%
          group_by(admin) %>%
          arrange(desc(pop_dec), .by_group = TRUE) %>%
          mutate(pop_int_final = pop_int + c(rep(1, round(sum(pop_dec))), rep(0, n() - round(sum(pop_dec))))) %>%
          ungroup()
      } else {
        df %>%
          mutate(pop_int = round(pop),
                 pop_dec = abs(pop - pop_int)) %>%
          group_by(admin) %>%
          arrange(desc(pop_dec), .by_group = TRUE) %>%
          group_modify(~ {
            this <- .x
            n_cells <- nrow(this)
            pop_int_sum <- sum(this$pop_int)
            pop_sum <- sum(this$pop)
            diff <- round(pop_sum - pop_int_sum)
            idx <- order(this$pop - this$pop_int, decreasing = (diff < 0))
            adjustment <- rep(0, n_cells)
            adjustment[idx[seq_len(abs(diff))]] <- sign(diff)
            this$pop_int_final <- this$pop_int + adjustment
            return(this)
          }) %>%
          ungroup()
      }
    }, error = function(e) {
      warning("Falling back to memory-efficient method due to: ", conditionMessage(e))
      NULL
    })
    if (!is.null(df_int)) {
      POP_float[df_int$cell] <- df_int$pop_int_final
      cat("** Writing int pop rasters in:\n**", output_dir, "\n")
      writeRaster(POP_float,
                  filename = fname,
                  gdal = c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                  overwrite = TRUE,
                  verbose = FALSE)
      invisible()
      cat("** Done!")
    } else {
      admin_ids <- unique(values(admin_rast))
      admin_ids <- admin_ids[!is.na(admin_ids)]
      temp_dir <- file.path(result_folder, "popint", "tmp")
      if (is.null(workers)) {
        plan(multisession, workers = min(availableCores() - 1, length(admin_ids)))
      } else {
        plan(multisession, workers = min(availableCores() - 1, length(admin_ids), workers))
      }
      result_list <- future_lapply(admin_ids, function(admin_id_i) {
        POP_sub <- rast(POP_path)
        admin_sub <- rast(admin_path)
        mask_index <- terra::cells(admin_sub, admin_id_i)[[1]]
        pop_vals <- POP_sub[mask_index]
        if (method == "worldpop") {
          pop_int_vals <- int_vector(pop_vals)
        } else {
          n_cells <- length(pop_vals)
          pop_int_values <- round(pop_vals)
          pop_int_sum <- sum(pop_int_values, na.rm=T)
          pop_sum <- sum(pop_vals, na.rm=T)
          diff <- round(pop_sum - pop_int_sum)
          idx <- order(pop_vals - pop_int_values, decreasing = (diff < 0))
          adjustment <- rep(0, n_cells)
          adjustment[idx[seq_len(abs(diff))]] <- sign(diff)
          pop_int_vals <- pop_int_values + adjustment
        }
        out_mat  <- cbind(matrix(mask_index,ncol=1), matrix(pop_int_vals,ncol=1))
        tmp_file <- file.path(temp_dir, paste0(admin_id_i, ".rds"))
        saveRDS(out_mat, tmp_file, compress = TRUE)
      })
      plan(sequential)
      POP_float <- rast(POP_path)
      admin     <- rast(admin_path)
      ncell_tot  <- ncell(admin)
      ff_path <- file.path(result_folder, "popint", "pop_matrix.ffdata")
      if (file.exists(ff_path)) unlink(ff_path,force = T)
      mat_ff  <- ff(vmode = "integer",
                    dim   = c(ncell_tot, 1),
                    filename = ff_path,
                    initdata = NA)
      handlers("txtprogressbar")
      with_progress({
        p <- progressor(along = admin_ids)
        for (id in admin_ids) {
          dat <- readRDS(file.path(temp_dir, paste0(id, ".rds")))
          mat_ff[dat[, 1]] <- dat[,2]
          gc()
          p()
        }
      })
      cat("** Writing int pop rasters in:\n**", output_dir)
      bs <- blocks(admin)
      nc <- ncol(admin)
      out   <- rast(POP_path) * NA
      writeStart(out, filename = fname,
                 gdal = c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
                 overwrite = TRUE,
                 verbose = FALSE)
      for (b in 1:bs$n) {
        row0 <- bs$row[b]
        nr   <- bs$nrows[b]
        cell_idx <- ((row0 - 1L) * nc + 1L) : ((row0 + nr - 1L) * nc)
        writeValues(out, v = mat_ff[cell_idx], start = row0, nrows = nr)
      }
      writeStop(out)
      close(mat_ff)
      unlink(temp_dir, recursive = TRUE, force = TRUE)
      cat("** Done!")
      return(invisible())
    }
  }
  unlink(temp_dir, recursive = TRUE, force = TRUE)
}
