# hydrusImport

> Import HYDRUS 2D/3D simulation results into R using functions made available in this R package.

## Installation

```r
devtools::install_github("jknappe/hydrusImport")
```

## Usage

### Load the package

```r
library(hydrusImport)
```

### Export data from HYDRUS 2D/3D

After successfully running a simulation in HYDRUS 2D/3D, export the results to ASCII using the HYDRUS 2D/3D GUI.

<img src = "http://www.janknappe.com/assets/img/github/hydrusImport-savingToAscii.gif" alt = "HYDRUS: Save results to ASCII"/>

### Import mesh data

- Use `import_nodes()` to import mesh node coordinates.

### Import simulation results

- Use `import_head()` to import pressure head results.
- Use `import_vwc()` to import volumetric water content (VWC) results.
- Use `import_velocity()` to import velocity results.
