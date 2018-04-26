#' Import velocity data from HYDRUS 2D output
#'
#' This function imports velocity data from a simulation successfully run in HYDRUS 2D.
#' Use 'h3d_' functions to import HYDRUS 3D results.
#' Simulation results ('Mesh Information' and 'Velocities') have to be exported to ASCII
#' prior to running this function using the HYDRUS 2D/3D GUI ('Results' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT' and 'V.TXT'.
#' @keywords
#'   IO
#' @return
#'   Returns a tibble with 6 columns.
#'   'timestep': 'Print Times' in units defined in HYDRUS 'Time Information',
#'   'x': x-coordinate of mesh node,
#'   'y': y-coordinate of mesh node,
#'   'parameter': 'velocity' for velocities,
#'   'value': numerical value for magnitude of velocity vector in units defined in HYDRUS [L T^{-1}],
#'   'direction' numerical value for direction of velocity vector.
#' @examples
#'   h2d_velocity(path = "data")
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @import
#'   dplyr tidyr stringr readr tibble
#' @export

h2d_velocity <- function(path) {
  #
  # Preamble
  # ~~~~~~~~~~~~~~~~
  #
  # path name of mesh file
  meshFile <-
    if (substring(path, nchar(path)) == "/") {
      # path provided with trailing '/'
      paste0(path, "MESHTRIA.TXT")
    } else {
      # path provided without trailing '/'
      paste0(path, "/MESHTRIA.TXT")
    }
  #
  # path name of results file
  velocityFile <-
    if (substring(path, nchar(path)) == "/") {
      # path provided with trailing '/'
      paste0(path, "V.TXT")
    } else {
      # path provided without trailing '/'
      paste0(path, "/V.TXT")
    }
  #
  # Error handling
  # ~~~~~~~~~~~~~~~~
  #
  # project folder must exists
  if (!dir.exists(path)) {
    stop("Can't find path to HYDRUS project. Does variable 'path' point to an existing HYDRUS project?")
  }
  # MESHTRIA.TXT must exist in the project folder
  if (!file.exists(meshFile)) {
    stop("HYDRUS project folder does not contain mesh information. Export mesh information through the HYDRUS GUI.")
  }
  # simulation results must exist in the project folder
  if (!file.exists(velocityFile)) {
    stop("HYDRUS project folder does not contain velocity data. Export simulation results through the HYDRUS GUI.")
  }
  #
  # Function
  # ~~~~~~~~~~~~~~~~
  #
  # import node coordinates
  nodeCoords <-
    h2d_nodes(path = path)
  #
  # import data
  velocityImport <-
    # read HYDRUS output
    velocityFile %>%
    readChar(., nchars = file.info(.)$size) %>%
    str_replace_all(pattern = " ", "\r\n") %>%
    read_csv(col_names = "value") %>%
    # extract timestep information
    mutate(timestep = ifelse(value %in% "Time", lead(value, 2), NA)) %>%
    fill(timestep) %>%
    # extract velocity vector components
    mutate(component = case_when(
      value %in% "first" ~ "value",
      value %in% "second" ~ "direction",
      TRUE ~ NA_character_)) %>%
    fill(component) %>%
    # remove non-data rows
    mutate(remove = ifelse(lag(value, 2) %in% "Time", TRUE, FALSE),
           remove = ifelse(value %in% c("Velocity", "Time", "first", "second", "component", "-", "="), TRUE, remove))  %>%
    filter(!remove) %>%
    select(-remove) %>%
    # add nodeID information
    group_by(timestep, component) %>%
    mutate(nodeID = row_number(timestep)) %>%
    ungroup() %>%
    # spread components
    spread(key = component, value = value) %>%
    # parse to numeric
    mutate(timestep = as.numeric(timestep),
           value = as.numeric(value),
           direction = as.numeric(direction),
           parameter = "velocity")
  #
  # join with node coordinates
  velocityData <-
    velocityImport %>%
    left_join(nodeCoords,
              by = "nodeID") %>%
    select(timestep, x, y, parameter, value, direction)
  #
  velocityData
}
#~~~~~~~~
