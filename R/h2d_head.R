#' Import pressure head data from HYDRUS 2D output
#'
#' This function imports pressure head data from a simulation successfully run
#' in HYDRUS 2D. Use 'h3d_' functions to import HYDRUS 3D results. Simulation
#' results ('Mesh Information' and 'Pressure Heads') have to be exported to
#' ASCII prior to running this function using the HYDRUS 2D/3D GUI ('Results'
#' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT' and 'H.TXT'.
#' @return
#'   Returns a tibble with 5 columns.
#'   'timestep': 'Print Times' in units defined in HYDRUS 'Time Information',
#'   'x': x-coordinate of mesh node,
#'   'y': y-coordinate of mesh node,
#'   'parameter': 'head' for pressure head,
#'   'value': numerical value in units defined in HYDRUS [L].
#' @family
#'   HYDRUS 2D functions
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @import
#'   dplyr tidyr stringr readr tibble
#' @export
#' @examples
#' h2d_head(path = "data")
#'
#' \dontrun{
#' h2d_head(path = "H.TXT")
#' h2d_head(path = "Project/H.TXT")
#' }

h2d_head <- function(path) {
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
  headFile <-
    if (substring(path, nchar(path)) == "/") {
      # path provided with trailing '/'
      paste0(path, "H.TXT")
    } else {
      # path provided without trailing '/'
      paste0(path, "/H.TXT")
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
  if (!file.exists(headFile)) {
    stop("HYDRUS project folder does not contain pressure head data. Export simulation results through the HYDRUS GUI.")
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
  headImport <-
    # read HYDRUS output
    headFile %>%
    readChar(., nchars = file.info(.)$size) %>%
    str_replace_all(pattern = " ", "\r\n") %>%
    read_csv(col_names = "value") %>%
    # extreact timestep information and move into new column
    mutate(timestep = ifelse(value %in% "Time", lead(value, 2), NA)) %>%
    fill(timestep) %>%
    # remove non-data rows
    mutate(remove = ifelse(value %in% "Time", TRUE, FALSE),
           remove = ifelse(lag(value, 1) %in% "Time", TRUE, remove),
           remove = ifelse(lag(value, 2) %in% "Time", TRUE, remove))  %>%
    filter(!remove) %>%
    select(-remove) %>%
    # parse to numeric
    mutate(timestep = as.numeric(timestep),
           value = as.numeric(value),
           parameter = "head") %>%
    # add nodeID information %>%
    group_by(timestep) %>%
    mutate(nodeID = row_number(timestep)) %>%
    ungroup()
  #
  # join with node coordinates
  headData =
    headImport %>%
    left_join(nodeCoords,
              by = "nodeID") %>%
    select(timestep, x, y, parameter, value)
  #
  headData
}
#~~~~~~~~
