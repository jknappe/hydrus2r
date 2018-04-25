#' Import edge coordinates from HYDRUS mesh
#'
#' This function imports edge coordinates from a simulation successfully run in HYDRUS 2D.
#' Use 'h3d_' functions to import HYDRUS 3D results.
#' Simulation results (including mesh information) have to be exported to ASCII
#' prior to running this function using the HYDRUS 2D/3D GUI ('Results' --> 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT'.
#' @keywords
#'   IO
#' @return
#'   Returns a tibble with 7 columns.
#'   'edgeID': edge number,
#'   'startNode': number of the beginning node of the edge,
#'   'endNode': number of the ending node of the edge,
#'   'loppNode': number of the left opposite node,
#'   'roppNode': number of the right opposite node,
#'   'lTriangle': number of the left triangle,
#'   'rTriangle': number of the right triangle.
#'   Note: 'roppNode', 'lTriangle', and 'lTriangle' are not always given.
#' @examples
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @export

h2d_edges <- function(path) {
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
  edgeCols <-  c("edgeID", "startNode", "endNode", "loppNode", "roppNode", "lTriangle", "rTriangle")
  #
  # import meshFile
  suppressWarnings( # ignore parsing warnings
    edgeInput <-
      read_table2(meshFile, skip = 1, col_names = edgeCols,
                  col_types = cols(
                    nodeID = col_character(),
                    x = col_character(),
                    y = col_character()
                  )) )
  # extract node corrdinates
  edgeCoords <-
    edgeInput %>%
    mutate(type = case_when(edgeID == "Edges"     ~ "edge",
                            edgeID == "***"       ~ "end",
                            edgeID == "Triangles" ~ "triangle",
                            edgeID == "Nodes"     ~ "nodes",
                            TRUE ~ NA_character_)) %>%
    fill(type) %>%
    filter(type %in% "edge") %>%
    slice(4:nrow(.)) %>%
    select(-type) %>%
    mutate_if(is.character, as.integer)
  #
  edgeCoords
}
#~~~~~~~~
