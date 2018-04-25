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

- Use `import_nodes()` to import mesh node coordinates.

### Import simulation results

- Use `import_head()` to import pressure head results.
- Use `import_vwc()` to import volumetric water content (VWC) results.
- Use `import_velocity()` to import velocity results.

## To do

- mesh data: `import_edges()`, `import_triangles()`, 3D meshes (3D-Standard, 3D-Lite)
- sim data: `import_concentration()`, `import_temperature()`, `import_particles()`
- improvements: use import flow from velocities also in H, TH (maybe nodes) to prevent small numbers being renered wrong (e-00X numbers)
