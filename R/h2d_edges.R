#' Import edge coordinates from HYDRUS 2D mesh
#'
#' This function imports edge coordinates from a simulation successfully run in
#' HYDRUS 2D. Use 'h3d_' functions to import HYDRUS 3D results. Simulation
#' results (including mesh information) have to be exported to ASCII prior to
#' running this function using the HYDRUS 2D/3D GUI ('Results' -->
#' 'Convert Output to ASCII').
#' @param path Path to HYDRUS 2D project containing 'MESHTRIA.TXT'.
#' @return
#'   Returns a tibble with 7 columns.
#'   'edgeID': edge number,
#'   'startNode': number of the beginning node of the edge,
#'   'endNode': number of the ending node of the edge,
#'   'loppNode': number of the left opposite node,
#'   'roppNode': number of the right opposite node,
#'   'lTriangle': number of the left triangle,
#'   'rTriangle': number of the right triangle.
#'   Note: 'roppNode', 'lTriangle', and 'lTriangle' are not always defined
#' @examples
#'   h2d_edges(path = "data")
#' @family
#'   HYDRUS 2D functions
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
  # extract edge information
  edges <-
    mesh %>%
    mutate(remove = ifelse(value == "Edges", TRUE, NA)) %>%
    fill(remove, .direction = "up") %>%
    filter(is.na(remove)) %>%
    select(-remove) %>%
    slice(5:(nrow(.) - 5)) %>%
    mutate(columnDummy = rep(c("edge", "startNode", "endNode", "loppNode"), length.out = n()),
           edgeID = cumsum(columnDummy == "edge")) %>%
    filter(columnDummy %in% c("startNode", "endNode", "loppNode")) %>%
    spread(key = columnDummy, value = value) %>%
    mutate(edgeID = as.integer(edgeID)) %>%
    mutate_if(is.character, as.numeric)
  #
  edges
}
#~~~~~~~~
