#' Title
#'
#' @param Z
#' @param Y
#' @param va
#' @param L
#'
#' @return
#' @export
#'
#' @examples
calculate_x <- function(Z, Y, va, L) {
  if(!is.null(dim(Y))) Y <- apply(Y, 1, sum) # if Y is matrix
  if(missing(L)) {
    # check if mass balanced
    if(!all.equal(apply(Z, 1, sum) + Y, apply(Z, 2, sum) + va)) {
      stop("IO system is not mass balanced !!")
    }
    # calculate output
    x <- apply(Z, 1, sum) + Y
  } else {
    x <- L %*% Y
  }
  return(x)
}


#' Title
#'
#' @param Z
#' @param x
#'
#' @return
#' @export
#'
#' @examples
calculate_A <- function(Z, x) {
  # calculate A-matrix
  # A <- Z/x[col(Z)]
  A <- Rfast::eachrow(Z, x,'/')
  A[is.na(A)] <- 0
  return(A)
}
#' Title
#'
#' @param A
#'
#' @return
#' @export
#'
#' @examples
calculate_L <- function(A) {
  # calculate Leontief inverse
  L <- solve(diag(nrow(A)) - A)
  return(L)
}


#' Title
#' #todo> check
#' @param Z
#'
#' @return
#' @export
#'
#' @examples
calculate_B <- function(Z, x) {
  B <- Z / x
  B[is.na(B)] <- 0
  return(B)
}

#' Title
#' #TODO: check
#' @param Z
#'
#' @return
#' @export
#'
#' @examples
calculate_G <- function(B) {
  return(solve(diag(nrow(B)) -(B)))
}

#' Title
#'
#' @param E
#' @param x
#'
#' @return
#' @export
#'
#' @examples
calculate_S <- function(E, x) {
  # calculate Stressor matrix
  # x_hat <- diag(1/x)
  # x_hat[is.infinite(x_hat)] <- 0
  # S <- E %*% x_hat
  S <- Rfast::eachrow(E, x,'/')
  S[!is.finite(S)] <- 0
  return(S)
}

#' Title
#'
#' @param Z
#' @param Y
#' @param va
#' @param E
#'
#' @return
#' @export
#'
#' @examples
IO_creator <- function(Z, Y, va, E) {
  x <- calculate_x(Z, Y, va)
  A <- calculate_A(Z, x)
  S <- calculate_S(E, x)
  L <- calculate_L(A)
  return(list("A" = A, "L" = L, "S" = S))
}

#' Title
#'
#' @param S
#' @param L
#' @param Y
#' @param B
#' @param d
#' @param f
#' @param detailed
#'
#' @return
#' @export
#'
#' @examples
IO_calculator <- function(S, L, Y, B, d, f, detailed = TRUE) {
  if(missing(Y)) Y <- (B %*% d) * as.numeric(f)
  x <- as.numeric(L %*% Y)
  if(detailed) B <- S %*% diag(x)
  else B <- S %*% x
  return(B)
}

#' Title
#'
#' @param n.industries
#' @param n.emissions
#' @param n.fdcats
#' @param A
#'
#' @return
#' @export
#'
#' @examples
create_random_IOtable <- function(n.industries, n.emissions, n.fdcats, A = FALSE) {
  x0 <- list("S" = matrix(runif(n.industries*n.emissions), n.emissions, n.industries),
             "L" = matrix(runif(n.industries^2), n.industries, n.industries),
             "Y" = matrix(runif(n.industries * n.fdcats), n.industries, n.fdcats))
  if(A) x0[["A"]] <- matrix(runif(n.industries^2), n.industries, n.industries)
  return(x0)
}

#' Title
#'
#' @param A_mat
#' @param n
#'
#' @return
#' @export
#'
#' @examples
leontief_series_expansion <- function(A_mat, n) {
  list <- vector(mode = "list", length = n)
  list[[1]] <- diag(1, nrow = nrow(A_mat), ncol = ncol(A_mat))
  for(i in 2:n) {
    list[[i]] <- list[[i-1]] %*% A_mat
  }
  return(list)
}


#' Aggregates the Y matrix for specfic columns.
#' e.g. by country, final demand category
#'
#' @param Y the final demand matrix
#' @param groupings a vector with the groupings, same length as ncol(Y)
#'
#' @return
#' @export
#'
#' @examples
aggregate_Y <- function(Y, groupings) {
  if (length(groupings) != ncol(Y)) stop('groupings need to have the same length as nrow(Y)')
  grouping_levels <- unique(groupings)
  n_groups <- length(grouping_levels)
  Ynew <- matrix(0, nrow = nrow(Y), ncol = n_groups)
  colnames(Ynew) <- grouping_levels
  for (i in 1:n_groups) {
    Ynew[,i] <- rowsums(Y[, groupings == grouping_levels[i]])
  }
  return(Ynew)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_A <- function(path, metadata = FALSE) {
  A <- data.table::fread(path, skip = 3, drop = c(1,2)) %>%
    as.matrix
  
  if (isTRUE(metadata)) {
    meta <- read_EB3_A_meta(path)
    attr(A, 'colnames') <- meta$colnames
    attr(A, 'rownames') <- meta$rownames
  }
  
  return(A)
  
}


#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_A_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1,2)) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'sector'))
  
  rownames <- fread(file.path(path), skip = 3,
                    select = c(1,2)) %>%
    setnames(new = c('region', 'sector'))
  
  return(list(colnames = colnames, rownames = rownames))
  
}



#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_Y <- function(path, metadata = FALSE) {
  Y <- data.table::fread(path, skip = 3, drop = c(1,2)) %>%
    as.matrix
  if (isTRUE(metadata)) {
    meta <- read_EB3_Y_meta(path)
    attr(Y, 'colnames') <- meta$colnames
    attr(Y, 'rownames') <- meta$rownames
  }
  return(Y)
}


#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_Y_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1,2)) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'category'))
  
  rownames <-fread(file.path(path), skip = 3,
                   select = c(1,2)) %>%
    setnames(new = c('region', 'sector'))
  
  return(list(colnames = colnames, rownames = rownames))
  
}


#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_va <- function(path, metadata = FALSE) {
  va <- fread(file.path(path),
              skip = 2, header = FALSE, drop = 1)[1:9] %>%
    as.matrix
  if (isTRUE(metadata)) {
    meta <- read_EB3_va_meta(path)
    attr(va, 'colnames') <- meta$colnames
    attr(va, 'rownames') <- meta$rownames
  }
  return(va)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_va_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1,2), header = FALSE) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'sector'))
  
  rownames <-fread(file.path(path), skip = 3,
                   select = c(1))[1:9,] %>%
    as.data.table %>%
    setnames(new = c('category'))
  
  return(list(colnames = colnames, rownames = rownames))
  
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_S <- function(path, metadata = FALSE) {
  S <- fread(file.path(path),
             skip = 25, header = FALSE, drop = 1) %>%
    as.matrix
  if (isTRUE(metadata)) {
    meta <- read_EB3_S_meta(path)
    attr(S, 'colnames') <- meta$colnames
    attr(S, 'rownames') <-  meta$rownames
  }
  
  return(S)
  
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_S_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1), header = FALSE) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'sector'))
  
  rownames <- fread(file.path(path), skip = 26,
                    select = c(1)) %>%
    setnames(new = c('category'))
  
  return(list(colnames = colnames, rownames = rownames))
  
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_F <- function(path, metadata = FALSE) {
  Fmat <- fread(file.path(path),
                skip = 3, header = FALSE, drop = 1) %>%
    as.matrix
  if (isTRUE(metadata)) {
    meta <- read_EB3_F_meta(path)
    attr(Fmat, 'colnames') <- meta$colnames
    attr(Fmat, 'rownames') <-  meta$rownames
  }
  
  return(Fmat)
  
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_EB3_F_meta <- function(path) {
  colnames <- fread(file.path(path), nrows = 2,
                    drop = c(1), header = FALSE) %>%
    t %>%
    as.data.table %>%
    setnames(new = c('region', 'sector'))
  
  rownames <- fread(file.path(path), skip = 3,
                    select = c(1)) %>%
    setnames(new = c('category'))
  
  return(list(colnames = colnames, rownames = rownames))
  
}


#' Title
#'
#' @param path
#' @param U
#' @param V
#' @param Y
#' @param va
#' @param metadata
#'
#' @return
#' @export
#'
#' @examples
parse_EB3_SUT <- function(path, U = TRUE, V = TRUE, Y = TRUE, va = TRUE,
                          metadata = FALSE,
                          path2meta = NULL) {
  SUT <- list()
  if (isTRUE(U)) SUT[['U']] <- read_EB3_A(file.path(path, "use.csv"),
                                          metadata = metadata)
  if (isTRUE(V)) SUT[['V']] <- read_EB3_A(file.path(path, "supply.csv"),
                                          metadata = metadata)
  if (isTRUE(Y)) SUT[['Y']] <- read_EB3_Y(file.path(path, "final_demand.csv"),
                                          metadata = metadata)
  if (isTRUE(va)) SUT[['va']] <- read_EB3_va(file.path(path, "value_added.csv"),
                                             metadata = metadata)
  
  if (isTRUE(metadata)) {
    if (is.null(path2meta)) {
      path2meta <- list.files(dirname(path), pattern = 'IOT_[[:digit:]]{4}_[ixi|pxp]',
                              full.names = TRUE) %>%
        stringr::str_subset('\\.zip$', negate = TRUE)
      
      if (length(path2meta) < 1) stop('please provide path2meta or set metadata to FALSE')
      path2meta <- path2meta[1]
    }
    SUT[['metadata']] <- parse_EB3_metadata(path2meta)
  }
  return(SUT)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
parse_EB3_metadata <- function(path, indices = TRUE) {
  meta <- list()
  meta[['products']] <- fread(file.path(path, 'products.txt'))
  meta[['industries']] <- fread(file.path(path, 'industries.txt'))
  meta[['unit']] <- fread(file.path(path, 'unit.txt'))
  meta[['finaldemands']] <- fread(file.path(path, 'finaldemands.txt'))
  
  if (isTRUE(indices)) {
    meta[['indices_A']] <- read_EB3_A_meta(file.path(path, 'A.txt'))
    meta[['indices_Y']] <- read_EB3_Y_meta(file.path(path, 'Y.txt'))
    meta[['indices_S']] <- read_EB3_S_meta(file.path(path, 'satellite', 'F.txt'))
    meta[['indices_va']] <- read_EB3_va_meta(file.path(path, 'satellite', 'F.txt'))
    
  }
  
  return(meta)
}


