
.ls.objects <- function # List (and/or sort) the largest objects and to occassionally rm() some of them. 
### To see the objects within a function, use: lsos(pos = environment()), 
### otherwise it'll only show global variables. 
### To write to standard error: write.table(lsos(pos=environment()), stderr(), quote=FALSE, sep='\t')
##seealso<< This has been packaged in the multilevelPSA package . The package is designed for something else, but you can use the function from there without loading the package by saying requireNamespace(multilevelPSA); multilevelPSA::lsos(...). Or in the Dmisc package  (not on CRAN).
## Just to note that data.table package's tables() seems to be a pretty good replacement for Dirk's .ls.objects() custom function (detailed in earlier answers), although just for data.frames/tables and not e.g. matrices, arrays, lists.
##references<< Based on postings by Petr Pikal and David Hinds to the r-help list in 2004]
## Tricks to manage the available memory in an R session at 
## \link{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
  ( pos = 1, pattern, order.by,
    decreasing=FALSE, head=FALSE, n=5) 
{
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  # couldn't think of any other way to get the output from print(...) and so used capture.output(), which I'm sure is very inefficient
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

lsos <- structure(function
#### shorthand for \code{\link{.ls.objects}}
(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
},ex=function(){
  lsos()
  ## Type   Size PrettySize Rows Columns
  ## pca.res                 PCA 790128   771.6 Kb    7      NA
  ## DF               data.frame 271040   264.7 Kb  669      50
})

