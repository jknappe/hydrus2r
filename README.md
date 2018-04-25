# hydrus2r

> Import HYDRUS 2D/3D simulation results into R using functions made available in this R package.

## Installation

```r
devtools::install_github("jknappe/hydrus2r")
```

## Usage

### Load the package

```r
library(hydrus2r)
```

### Export data from HYDRUS 2D/3D

After successfully running a simulation in HYDRUS 2D/3D, export the results to ASCII using the HYDRUS 2D/3D GUI.

<img src = "http://www.janknappe.com/hydrus2r/docs/hydrus2r-savingToAscii.gif" alt = "HYDRUS: Save results to ASCII"/>

### `h2d_`-functions for HYDRUS 2D

- Use `h2d_nodes()` to import mesh node coordinates.
- Use `h2d_edges()` to import mesh edge information.
- Use `h2d_triangles()` to import mesh triangle  information (**not yet implemented**).

- Use `h2d_head()` to import pressure head results.
- Use `h2d_vwc()` to import volumetric water content (VWC) results.
- Use `h2d_velocity()` to import velocity results.
- Use `h2d_concentration()` to import concentraion results.
- Use `h2d_temperature()` to import temperature results.
- Use `h2d_particle()` to import particle trajectory results (**not yet implemented**).

### `h3d_`-functions for HYDRUS 3D

**not yet implemented**
