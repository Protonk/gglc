#' Format Lorenz curve data for plotting
#'
#' Normalize formats across Lc objects to make plot handling
#' a little easier. Theoretical Lorenz curves can be passed in
#' but only if accompanied by one Lc object.
#'
#' @param Lorenz A list of objects created by \code{\link{Lc}},
#' preferably named.
#' @return A data frame with values mapped to x and y, appropriate for 
#' passing into \code{\link{ggplot}}.
#' 
#' @export

LcLong <- function(Lorenz) {
  args.class <- sapply(Lorenz, class)
  #### Check inputs
  if(!all(args.class %in% c("Lc", "theorLc"))) {
    stop("Input must be the correct class")
  }
  # Theoretical Lorenz curves need to be plotted
  # with respect to their input parameters
  # tricky because they aren't passed along with
  # the output object. So we assume a single mapping
  if(any(args.class == "theorLc") &
     sum(args.class == "Lc") > 1) {
    stop("Theoretical Lorenz curves need consistent mappings")
  }
  
  # Name unnamed objects (makes plotting easier)
  rename <- function() {
    # List objects inserted without names are
    # given the empty string as a name 
    unnamed <- names(Lorenz) == ""
    if(any(unnamed)) {
      john.doe <- paste0("jd", 1:sum(unnamed))
      # the order of subsetting matters
      # subset the name vector, not the list!
      names(Lorenz)[unnamed] <- john.doe
    } 
    return(Lorenz)
  }
  Lorenz <- rename()
  
  
  # Doesn't really have to be a function but lets
  # us modify an object easily
  flatten <- function(element, name) {
    # We will map all the theorLc objects to one
    # Lc object
    if(is.null(element$L)) {
      element$L <- Lorenz[names(Lorenz) != ""][[1]][["L"]]
    }
    out.df <- data.frame(x = element$p,
                         y = element$L,
                         Name = name)
    return(out.df)
  }
  out.list <- vector(mode = "list", length = length(Lorenz))
  names(out.list) <- names(Lorenz)
  # Iterators in R make getting position frustrating
  for(i in seq_along(Lorenz)) {
    out.list[[i]] <- flatten(Lorenz[[i]],
                             names(Lorenz)[i])
  }
  long.df <- do.call(rbind, out.list)
  rownames(long.df) <- as.character(1:nrow(long.df))
  return(long.df)
}

