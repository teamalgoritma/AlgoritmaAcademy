# AlgoritmaAcademy

Algoritma Academy Internal Package

## Installation

Use the following command to install the package:

```
devtools::install_github("teamalgoritma/AlgoritmaAcademy")
```

## Developer Notes: How to Contribute?

Make sure to install these packages to develop the R package:

```
install.packages(c("devtools", "usethis", "roxygen2"))
```

### Create New Function

Step-by-step on how to create a new function:

1. Download the `AlgoritmaAcademy` package
2. Open RStudio via `AlgoritmaAcademy.Rproj`
3. Create new function using `usethis::use_r(name = "function_name")`
4. Code the function inside the newly created R script. The path of the script is in the `R` folder
5. Documentate the function inside the R script:
    - Hover over the function
    - Click "Code" -> "Insert roxygen skeleton"
    - Complete the documentation, then save
    - Use `devtools::document()` so that the documentation can be seen
    - If the script uses an external library, make sure the library is listed in the `DESCRIPTION` file
6. Use `devtools::load_all()` to find out if the created function is running or not
7. Test the function
8. If okay, update the documentation of the function by using `devtools::document()`
9. To check whether there is an error/warning in the package, you can use `devtools::check()`
10. To install package to global environment (your laptop) use `devtools::install()`
11. Restart RStudio, load the package by using `library(AlgoritmaAcademy)`, and then try to use the created function
12. If okay, push the code to GitHub

### Update Function

Step-by-step on how to update a function:

1. Download the `AlgoritmaAcademy` package
2. Open RStudio via `AlgoritmaAcademy.Rproj` and open the R script you want to edit inside the `R` folder
3. Update the function accordingly. Make sure to update the documentation also
4. Use `devtools::load_all()` to find out if the updated function is running or not
5. Test the function
6. If okay, update the documentation of the function by using `devtools::document()`
7. To check whether there is an error/warning in the package, you can use `devtools::check()`
8. To install package to global environment (your laptop) use `devtools::install()`
9. Restart RStudio, load the package by using `library(AlgoritmaAcademy)`, and then try to use the updated function
10. If okay, push the code to GitHub

### Learning Resources

- [R Packages: The whole game](https://r-pkgs.org/whole-game.html)