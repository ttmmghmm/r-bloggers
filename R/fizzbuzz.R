fizzbuzz <- function(f = 3L, b = 5L, n = 1e2L)
### description goes here 
{ stopifnot(
  is.numeric(f), length(f) == 1, f > 0, f <= n,
  is.numeric(b), length(b) == 1, b > 0, b <= n,
  is.numeric(n), length(n) == 1, n > 0
  )
  ans <- seq.int(n)
  aa  <- which( ! ans %% f) 
  bb  <- which( ! ans %% b)
  ans[aa] <- 'fuzz'
  ans[bb] <- 'bang'
  ans[intersect(bb, aa)] <- 'fizzbuzz'
  structure(ans, class = 'fizzbuzz')
}
print.fizzbuzz <- function(x, ..., n = 16) 
{ cat(paste(head(x, n = n), collapse = '\n'))
  invisible(x) 
}
fizzbuzz()
fizzbuzz(f = 1, b = 1, n = 2)
fizzbuzz(f = 2, b = 1, n = 2)
