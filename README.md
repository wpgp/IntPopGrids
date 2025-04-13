# Integer Population Grids

**integer-population-grids** is an R package that provides tools for translating floating-point population grids into integer values. This package is designed for users who need a more communicatable and realistic population data in a grid format for spatial analysis, demographic studies, and related applications.

## Installation

To install the package from GitHub, first ensure you have the `devtools` package installed:

```r
install.packages("devtools")  # Install devtools if not already installed
devtools::install_github("wpgp/IntPopGrids")
```

## Usage

Once installed, load the package in your R session:

```r
library(IntPopGrids)
```

### Functions avaliable
- `disaggregate_int_vector(x,r,method="random")`
  
Disaggregate an integer vector `x` into a integer matrix. `r` is a vector showing porpotions of subgroups. Each row of the matrix corresponding to the element of input vector. `length(r)` is the number of rows of the matrix.
```r
x <- c(10, 4, 15)
r <- c(0.2, 0.8)
# disaggregate x into subgroups based on r directly
x %o% r
[  2     8
 0.8   3.2 
   3    12]
# disaggregate x into subgroups based on r through our function
generate_population_grid(x, r)
[2   8
 1   3 
 3  12]
```

- `int_vector(x)`

Generate corresponding integer vector with smallest difference to the input vector.

- `int_grid(POP_path, admin_path)`

Convert gridded floating population estimates into integers, while the total population per admin units are maintained.
``` r
POP_path <- "pop.tif" # the file path of population raster
pop_int <- int_grid(POP_path) # if admin units are unavailable
admin_path <- "admin.tif" # the file path of administrative units raster
pop_int <- int_grid(POP_path, admin_path)
```

- `int_groups(POP_path, admin_path, group_path)`

Generate corresponding integer population count on each grid cell for each group based on a vector of proportions for the interest groups.
The variable name for admin units should be `id` in the csv file. The corresponding variable values are the same as in the admin units raster.
```r
POP_path <- "pop.tif" # the file path of population raster
admin_path <- "admin.tif" # the file path of administrative units raster
group_path <- "group.csv" #the file path of proportions of each group 
#' example of group data: group <- data.frame(id = 1:2,
#'                                            group_1 = runif(10),
#'                                            group_2 = 1 - group_1)
pop_int <- int_groups(POP_path, admin_path, group_path)
```


## Dependencies

This package requires the following R packages:
- `terra`
- `future`
- `future.apply`

Ensure they are installed before using the package:

```r
install.packages(c("terra", "future", "future.apply"))
```

## Contributing

Contributions are welcome! If you'd like to improve this package, please:
1. Fork the repository.
2. Make changes in a new branch.
3. Submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use IntPopGrids in your research, please cite:

```bibtex
@software{IntPopGrids,
  author = {Zhang W., Sorichetta A., Yetman G., Hilton J., Tatem A., Bondarenko M.},
  title = {IntPopGrids: R package to translate floating-point population grids into integer values.},
  publisher = {GitHub},
  url = {https://github.com/wpgp/IntPopGrids}
}
```

## Getting Help

- Issues & Support: https://github.com/wpgp/IntPopGrids/issues
- WorldPop SDI: https://sdi.worldpop.org