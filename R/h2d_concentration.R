#' Import concentration data from HYDRUS 2D output
#'
#' This function imports concentration data from a simulation successfully run in HYDRUS 2D.
#' Use 'h3d_' functions to import HYDRUS 3D results.
#' Simulation results ('Mesh Information' and 'Concentrations') have to be exported to ASCII
#' prior to running this function using the HYDRUS 2D/3D GUI ('Results' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT' and 'CONC<n>.TXT' with <n> being 1, 2, ..., 7.
#' @keywords
#'   IO
#' @return
#'   Returns a tibble with 5 columns.
#'   'timestep': 'Print Times' in units defined in HYDRUS 'Time Information',
#'   'x': x-coordinate of mesh node,
#'   'y': y-coordinate of mesh node,
#'   'parameter': 'concentrationX' for concentration (concentration1, concentration2, ..., concentration7),
#'   'value': numerical value in units defined in HYDRUS [-].
#' @examples
#'   h2d_concentration(path = "data")
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @import
#'   dplyr tidyr stringr readr tibble purrr
#' @export

h2d_concentration <- function(path) {
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
  # correct path if provided without trailing '/'
  if (!(substring(path, nchar(path)) == "/")) {
    path <- paste0(path, "/")
  }
  #
  # find all concentration files in path (max = 7)
  files <-
    list.files(path = path, pattern = "Conc[1-7].TXT")
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
   if (!length(files == 0)) {
     stop("HYDRUS project folder does not contain concentration data. Export simulation results through the HYDRUS GUI.")
   }
  #
  # Function
  # ~~~~~~~~~~~~~~~~
  #
  # import node coordinates
  nodes <-
    h2d_nodes(path = path)
  #
  # import data
  input <-
    files %>%
    map(function(x) { # x = concentration files
      readChar(paste0(path, x), nchars = file.info(paste0(path, x))$size) %>%
        str_replace_all(pattern = " ", "\r\n") %>%
        read_csv(col_names = "value") %>%
        # flag parameter from source file
        mutate(parameter = paste0("concentration", str_extract(x, "[1-7]"))) %>%
        # extract timestep information
        mutate(timestep = ifelse(value %in% "Time", lead(value, 2), NA)) %>%
        fill(timestep) %>%
        # remove non-data rows
        mutate(remove = ifelse(value %in% "Time", TRUE, FALSE),
               remove = ifelse(lag(value, 1) %in% "Time", TRUE, remove),
               remove = ifelse(lag(value, 2) %in% "Time", TRUE, remove)) %>%
        filter(!remove) %>%
        select(-remove) %>%
        # parse to numeric
        mutate(timestep = as.numeric(timestep),
               value = as.numeric(value)) %>%
        # add nodeID information
        group_by(timestep) %>%
        mutate(nodeID = row_number(timestep)) %>%
        ungroup()
    }) %>%
    bind_rows()
  #
  # join with node coordinates
  result <-
    input %>%
    left_join(nodes,
              by = "nodeID") %>%
    select(timestep, x, y, parameter, value)
  #
  result
}
#~~~~~~~~
