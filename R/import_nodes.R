#' Import node coordinates from HYDRUS mesh
#'
#' This function imports node coordinates from a simulation successfully run in HYDRUS 2D/3D.
#' Simulation results (including mesh information) have to be exported to ASCII
#' prior to running this function using the HYDRUS 2D/3D GUI ('Results' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D/3D project containing 'MESHTRIA.TXT'.
#' @keywords
#'   IO
#' @return
#'   Returns a tibble with 3 columns: 'nodeID', 'x', 'y' with 'x' and 'y'
#'   being the cartesian coordinates of node 'nodeID'.
#' @examples
#'   import_nodes(path = "C://HYDRUS_Project/Project_Name")
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @export

import_nodes <- function(path) {
  #
  # Preamble
  # ~~~~~~~~~~~~~~~~
  #
  # file name of mesh file
  meshFile <-
    if (substring(path, nchar(path)) == "/") {
      # path provided with trailing '/'
      paste0(path, "MESHTRIA.TXT")
    } else {
      # path provided without trailing '/'
      paste0(path, "/MESHTRIA.TXT")
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
  #
  # Function
  # ~~~~~~~~~~~~~~~~
  #
  # create column names
  nodeCols <-  c("nodeID", "x", "y")
  #
  # import meshFile
  suppressWarnings( # ignore parsing warnings
    nodeInput <-
      read_table2(meshFile, skip = 1, col_names = nodeCols,
                  col_types = cols(
                    nodeID = col_character(),
                    x = col_character(),
                    y = col_character()
                  )) )
  # extract node corrdinates
  nodeCoords <-
    nodeInput %>%
    mutate(nonNodes = ifelse(nodeID == "Edges", FALSE, NA)) %>%
    fill(nonNodes) %>%
    filter(is.na(nonNodes)) %>%
    select(-nonNodes) %>%
    mutate(nodeID = as.integer(nodeID),
           x      = as.numeric(x),
           y      = as.numeric(y))
  #
  nodeCoords
}
#~~~~~~~~
