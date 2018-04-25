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

### Import mesh data

- Use `h2d_nodes()` to import mesh node coordinates.
- Use `h2d_edges()` to import mesh edge information.
- `h2d_triangles()`
- `h3d_nodes()`
- `h3d_edges()`
- `h3d_triangles()`

### Import simulation results

- Use `h2d_head()` to import pressure head results.
- Use `h2d_vwc()` to import volumetric water content (VWC) results.
- Use `h2d_velocity()` to import velocity results.

## To do

- mesh data: `h2d_edges()`, `h2d_triangles()`, 3D meshes (3D-Standard, 3D-Lite)
- sim data: `h2d_concentration()`, `h2d_temperature()`, `h2d_particles()`
