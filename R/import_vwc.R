#' Import water content data from HYDRUS output
#'
#' This function imports volumentric water content (VWC) data from a simulation successfully run in HYDRUS 2D/3D.
#' Simulation results ('Mesh Information' and 'Water Contents') have to be exported to ASCII
#' prior to running this function using the HYDRUS 2D/3D GUI ('Results' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D/3D project containing 'MESHTRIA.TXT' and 'TH.TXT'.
#' @keywords
#'   IO
#' @return
#'   Returns a tibble with 5 columns.
#'   'timestep': 'Print Times' in units defined in HYDRUS 'Time Information'.
#'   'x': x-coordinate of HYDRUS mesh node.
#'   'y': y-coordinate of HYDRUS mesh node.
#'   'parameter': 'vwc' for volumetric water content.
#'   'value': numerical value in units defined in HYDRUS (dimensionless for VWC)
#' @examples
#'   import_nodes(path = "C://HYDRUS_Project/Project_Name")
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @export

import_vwc <- function(path) {
  #
  # Preamble
  # ~~~~~~~~~~~~~~~~
  #
  # load required packages
  library(readr)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  #
  # file name of mesh file
  vwcFile <-
    if (substring(path, nchar(path)) == "/") {
      # path provided with trailing '/'
      paste0(path, "TH.TXT")
    } else {
      # path provided without trailing '/'
      paste0(path, "/TH.TXT")
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
  if (!file.exists(vwcFile)) {
    stop("HYDRUS project folder does not contain mesh information. Export mesh information through the HYDRUS GUI.")
  }
  #
  # Function
  # ~~~~~~~~~~~~~~~~
  #
  # import node coordinates
  nodeCoords <-
    hydrusImport::import_nodes(path = path)
  #
  # import VWC data
  vwcImport <-
    # read HYDRUS output file and split by word into tibble
    list(value = (readr::read_file(vwcFile) %>%
                    stringr::str_split(stringr::boundary("word")))[[1]]
    ) %>%
    tibble::as_tibble() %>%
    # extreact timestep information and move into new column
    dplyr::mutate(timestep = ifelse(value %in% "Time", dplyr::lead(value), NA)) %>%
    tidyr::fill(timestep) %>%
    # remove non-data rows
    dplyr::mutate(remove = ifelse(value %in% "Time", TRUE, FALSE),
           remove = ifelse(remove, TRUE, dplyr::lag(remove)))  %>%
    dplyr::filter(!remove) %>%
    dplyr::select(-remove) %>%
    # parse to numeric
    dplyr::mutate(timestep = as.numeric(timestep),
           value = as.numeric(value),
           parameter = "vwc") %>%
    # add nodeID information %>%
    dplyr::group_by(timestep) %>%
    dplyr::mutate(nodeID = dplyr::row_number(timestep)) %>%
    dplyr::ungroup()
  #
  # join with node coordinates
  vwcData =
    vwcImport %>%
    dplyr::left_join(nodeCoords,
                     by = "nodeID") %>%
    select(timestep, x, y, parameter, value)
}
#~~~~~~~~
