#' LappetMoths data set, a list of 8 data frames.
#'
#' A dataset containing the sequences IDs of species,
#' coordinates of species sampled, and other attributes
#'
#'
#' @format list of 8 data frames.
#'
#' \describe{
#'   \item{barcode.identi.result}{data frame,species identifications by other
#'         methods or barocodes,containing query IDs, species identified,
#'         and corresponding probablities.}
#'   \item{que.env}{data frame, containing query sampleIDs,and a set of
#'         corresponding environmental variables collected by users.}
#'   \item{que.infor}{data frame, query samples,containing sample IDs,longitude
#'         and latitude of each sample.}
#'   \item{que.seq}{query sequences in binary format stored in a matrix}
#'   \item{ref.env}{data frame, containing reference sampleIDs, species names,
#'         and a set of environmental variables collected by users.}
#'   \item{ref.infor}{data frame, reference dataset containing sample IDs, taxon
#'         information,longitude and latitude of each sample.}
#'   \item{ref.seq}{reference sequences in binary format stored in a matrix}
#'   \item{ref.add}{data frame, additional reference dataset containing taxon
#'         information, longitude and latitude of each species.}
#' }
"LappetMoths"


#' en.vir data set, a class of RasterBrick.
#'
#' A dataset containing 5 of the 19 bioclimatic variables downloaded from WorldClim
#' (version 1.4 with 2.5 arc minute resolution; Hijmans et al. 2005)).
#'
#'
#' @format a class of RasterBrick.
#'  \describe{
#'  \item{en.vir}{class: RasterBrick;
#'               dimensions : 6, 2160, 12960, 5  (nrow, ncol, ncell, nlayers);
#'              resolution : 0.1666667, 0.1666667  (x, y);
#'              extent: -180, 180, -60, 90  (xmin, xmax, ymin, ymax);
#'              crs:+proj=longlat +datum=WGS84;
#'              source:memory;
#'              names: layer.1, layer.2, ..., .
#'  }
#' }
#' @source \url{http://www.worldclim.org/}
"en.vir"


#' bak.vir data set, a class of matrix.
#'
#' A dataset containing 5 of the 19 bioclimatic variables randomly genereated as
#' background points.
#'
#'
#' @format a class of matrix.
#'  \describe{
#'  \item{bak.vir}{5000*5 matrix.}
#' }
#' @source \url{http://www.worldclim.org/}
"bak.vir"
