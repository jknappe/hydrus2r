#' Import node coordinates from HYDRUS 2D mesh
#'
#' This function imports node coordinates from a simulation successfully run in
#' HYDRUS 2D. Use 'h3d_' functions to import HYDRUS 3D results. Simulation
#' results (including mesh information) have to be exported to ASCII prior to
#' running this function using the HYDRUS 2D/3D GUI ('Results' -->
#' 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT'.
#' @return
#'   Returns a tibble with 3 columns.
#'   'nodeID': nodal number,
#'   'x': x-coordinate of mesh node,
#'   'y': y-coordinate of mesh node.
#' @family
#'   HYDRUS 2D functions
#' @references
#'   https://www.pc-progress.com/downloads/Pgm_Hydrus3D2/HYDRUS3D%20User%20Manual.pdf
#' @author
#'   Jan Knappe, \email{jan.knappe@@gmail.com}
#' @export
#' @examples
#' h2d_nodes(path = "data")
#'
#' \dontrun{
#' h2d_nodes(path = "MESHTRIA.TXT")
#' h2d_nodes(path = "Project/MESHTRIA.TXT")
#' }

h2d_nodes <- function(path) {
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
  # import meshFile
  mesh <-
    # read HYDRUS output
    meshFile %>%
    readChar(., nchars = file.info(.)$size) %>%
    str_replace_all(pattern = " ", "\r\n") %>%
    read_csv(col_names = "value",
             col_types = cols(value = col_character())) %>%
    slice(6:nrow(.))
  #
  # extract node information
  nodes <-
    mesh %>%
    mutate(remove = ifelse(value == "Edges", TRUE, NA)) %>%
    fill(remove, .direction = "down") %>%
    filter(is.na(remove)) %>%
    select(-remove) %>%
    mutate(columnDummy = rep(c("node", "x", "y"), length.out = n()),
           nodeID = cumsum(columnDummy == "node")) %>%
    filter(columnDummy %in% c("x", "y")) %>%
    spread(key = columnDummy, value = value) %>%
    mutate(nodeID = as.integer(nodeID)) %>%
    mutate_if(is.character, as.numeric)
  #
  nodes
}
#~~~~~~~~
