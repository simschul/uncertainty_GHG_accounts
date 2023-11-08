# functions

#' Set up configurations and log stuff. 
#' 
#' Run this function at the beginning of every script of this project
#'
#'
#' @return the configurations as set up in `config.yaml` (extendend by an `path2output` parameter)
#' @export
#'
#' @examples
setup_config_and_log <- function() {
  
  # read config
  config <- config::get()
  
  # read filename
  filename = get_current_filename()
  
  # settings
  options("datatable.print.class" = TRUE)
  theme_set(config$ggtheme)
  config$path2output <- file.path('intermediate_results', paste0('V', config$version))
  
  # create directory for script output
  if (!dir.exists(config$path2output)) {
    dir.create(config$path2output)
    dir.create(file.path(config$path2output, 'scripts'))
    dir.create(file.path(config$path2output, 'plots'))
  } 
  
  # store copy of current script (for transparency)
  file.copy(from = paste0(filename, '.R'), file.path(config$path2output, 'scripts', 
                                                     paste0(filename, '.R')))
  
  # open log file
  logr::log_open(file.path('intermediate_results', 
                     paste0('V', config$version), 
                     paste0(filename, '.log')))
  logr::log_print("-------- Config --------------")
  logr::log_print(config)
  
  return(config)
  
}



#' Title
#'
#' @param node 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
isNonEmptyLeaf <- function(node, attribute) {
  return(isLeaf(node) & !is.null(node[[attribute]]) & !is.na(node[[attribute]]))
}

is.node <- function(x) {
  'Node' %in% class(x)
}

is.node.list <- function(x) {
  unlist(lapply(x, is.node))
  
}
is.empty.list <- function(x) {
  is.null(unlist(x)) 
}

#' Title
#'
#' @param node 
#' @param attribute attribute that stores the SD
#' @param fun 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
propagate_uncertainty <- function (node, attribute, 
                                   fun = function(x) sqrt(sum(x^2)), ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(node)) 
    return(GetAttribute(node, attribute, ...))
  values <- sapply(node$children, function(x) {
    v <- GetAttribute(x, attribute, format = identity, ...)
    if (length(v) > 0 && !is.na(v)) 
      return(v)
    propagate_uncertainty(x, attribute,  ...)
  })
  result <- unname(fun(values))
  return(result)
}

#' Title
#'
#' @param node 
#' @param emissions 
#' @param uncertainty 
#' @param uncertainty_new 
#' @param N 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
propagate_uncertainty_backwards <- function (node, 
                                             emissions,
                                             uncertainty,
                                             uncertainty_new,
                                             N = 100, ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(node)) return(NULL)
  
  # node != leaf
  try(node$children$`NA` <- NULL)
  
  result <- rep(NA, length(node$children))
  values <- rep(NA, length(node$children))
  i <- 1
  for (child in node$children) {
    myvalue <- GetAttribute(child, uncertainty, format = identity)
    if (length(myvalue) == 0 || is.na(myvalue)) {
      # child has no uncertainty value
      values[i] <- GetAttribute(child, emissions, format = identity)
    } else {
      # child already has uncertainty value
      result[i] <- myvalue
    }
    i <- i + 1
  }
  
  if (sum(!is.na(result)) == length(node$children)) {
    # all children have uncertainty values
    return(result)
  }
  
  if (length(result[is.na(result)]) == 1) {
    # only ONE child has no uncertainty value: take uncertainty value from parent
    # TODO: as currently implemented this only makes sense for length(node$children) == 1
    # if node as several children but only one without uncertainty value, there should be a different procedure 
    # (--> maximum entropy approach)
    
    result[is.na(result)] <- GetAttribute(node, uncertainty_new, format = identity)
  }
  
  # otherwise: propagate uncertainty using dirichlet
  # TODO: problem when there is a big divergence in emission between sectors. then dirichlet creates bullshit: see: gtools::rdirichlet(1E5, c(8.025588e-09, 1.000000e+00))
  
  
  sample_parent <- truncnorm::rtruncnorm(N, 
                                         a = 0, 
                                         mean = node[[emissions]], 
                                         sd = node[[uncertainty_new]] * node[[emissions]])
  
  alphas <- values / sum(values, na.rm = TRUE)
  alphas <- alphas[!is.na(alphas)]
  sample_children <- gtools::rdirichlet(N, alphas)
  sample_combined <- sample_children * sample_parent
  
  temp <- apply(sample_combined, 2, function(x) {
    sd(x) / mean(x)
  }) 
  temp[temp == 0] <- 10 # TODO: warning: this a quick and very dirty fix !! see above
  
  result[is.na(result)] <- temp
  
  #cat('has more detailed emissions data: ', node$name, '\n')
  
  return(result)
}


#' Check if a vector is numeric and has no NAs
#'
#' @param x A vector
#' @return A logical value indicating whether the vector is numeric and has no NAs
#'
#' @examples
#' x <- c(1, 2, 3)
#' is_numeric_no_na(x)
#' # TRUE
#'
#' y <- c(1, 2, NA)
#' is_numeric_no_na(y)
#' # FALSE
#'
is_numeric_no_na <- function(x) {
  is.numeric(x) & all(!is.na(x))
}

#' Check whether a vector has at least one NA but is not all NA
#'
#' @param x A vector of values
#' @return A logical value indicating whether the input vector has at least one NA but is not all NA
#'
#' Roxygen header created by ChatGPT, handle with care
has_some_na_not_all <- function(x) {
  na_count = sum(is.na(x))
  return(na_count > 0 & na_count < length(x))
}



#' Disaggregate emissions from a tree structure
#'
#' @param tree A tree structure object
#' @param emissions Character string indicating the name of the emission attribute
#' @param proxy_data Character string indicating the name of the proxy data attribute
#' @return The input tree structure with updated emissions values
#'
#' @examples
#' tree <- create_tree()
#' emissions <- "emissions"
#' proxy_data <- "proxy_data"
#' tree_disaggregated <- disaggregate_emissions3(tree, emissions, proxy_data)
#'
#'Roxygen header created by ChatGPT, handle with care
disaggregate_emissions3 <- function(tree, emissions, 
                                    proxy_data) {
  
  # TODO: does not work if the last node with CRF is *two* levels higher than the last node with NIR emissions 
  
  tree$Do(function(node) {
    child_emissions <- sapply(node$children, GetAttribute, 
                              attribute = emissions)
    child_proxies <- sapply(node$children, GetAttribute, 
                              attribute = proxy_data)
    my_emissions <- GetAttribute(node, emissions)
    if (all(is.na(child_emissions)) & is_numeric_no_na(child_proxies) &
        !is.na(my_emissions)) {
      # all childs are without CRF emissions
      # all childs have NIR emissions
      # node has CRF emissions
      
      child_emissions_new <- (child_proxies / sum(child_proxies)) * my_emissions
      for (i in seq_along(node$children)) {
        node$children[[i]][[emissions]] <- child_emissions_new[[i]]
      }
      print(paste0('node ', node$name, ': replaced child emissions ', 
                   child_proxies, '-->', child_emissions_new))
    }
  }, filterFun = isNotLeaf)
  return(tree)
}


#' Title
#'
#' @param node 
#' @param emissions 
#' @param proxy_data 
#'
#' @return
#' @export
#'
#' @examples
#' 
#emissions = 'emissions_CRF'
#proxy_data <- 'emissions_NIR'

disaggregate_emissions <- function(node, 
                                   emissions, 
                                   proxy_data) {
  if (isLeaf(node)) return(NULL)
  try(node$children$`NA` <- NULL)
  
  result <- rep(NA, length(node$children))
  proxy_shares <- rep(NA, length(node$children))
  i <- 1
  for (child in node$children) {
    myvalue <- GetAttribute(child, emissions, format = identity)
    if (length(myvalue) == 0 || is.na(myvalue)) {
      # child has no emissions value
      proxy_shares[i] <- GetAttribute(child, proxy_data, format = identity)
    } else {
      # child already has emissions value
      result[i] <- myvalue
    }
    i <- i + 1
  }
  
  if (sum(!is.na(result)) == length(node$children)) {
    # all children have emissions values
    return(result)
  }
  my_emissions <- GetAttribute(node, emissions, format = identity)
  proxy_shares <- proxy_shares[!is.na(proxy_shares)]
  emissions_new <- (proxy_shares / sum(proxy_shares)) * my_emissions
  print(emissions_new)
  print(result)
  result[is.na(result)] <- emissions_new
  #cat('has more detailed uncertainty data: ', node$name, '\n')
  return(result)
}



#' Title
#'
#' @param node 
#' @param emissions 
#' @param proxy_data 
#'
#' @return
#' @export
#'
#' @examples
disaggregate_emissions2 <- function(node, 
                                    emissions, 
                                    proxy_data) {
  if (isLeaf(node)) return(NULL)
  try(node$children$`NA` <- NULL)
  
  
  proxy_shares <- create_named_list(names(node$children))  
  for (child in node$children) {
    proxy_shares[[child$name]] <- create_named_list(child$get_classifications())
    for (classification in child$get_classifications()) {
      if (!child$has_values(emissions, classification)) {
        proxy_shares[[child$name]][[classification]] <-  child$data[classification == classification][[proxy_data]] 
        
      }
    }
  }
  
  result <- vector('list', length(node$children))
  proxy_shares <- vector('list', length(node$children))
  i <- 1
  for (child in node$children) {
    for (classification in child$get_classifications()) {
      myvalue <- child$data[classification == classification, emissions, with = FALSE]  
      if (length(myvalue) == 0 || is.na(myvalue)) {
        # child has no emissions value
        proxy_shares[[i]] <- child$data[classification == classification, proxy_data, 
                                        with = FALSE] 
      } else {
        # child already has emissions value
        result[[i]] <- myvalue
      }
      i <- i + 1  
    }
    
    
  }
  
  if (sum(!is.na(result)) == length(node$children)) {
    # all children have emissions values
    return(result)
  }
  my_emissions <- GetAttribute(node, emissions, format = identity)
  proxy_shares <- proxy_shares[!is.na(proxy_shares)]
  emissions_new <- (proxy_shares / sum(proxy_shares)) * my_emissions
  result[is.na(result)] <- emissions_new
  #cat('has more detailed uncertainty data: ', node$name, '\n')
  return(result)
}


#' Title
#'
#' @param x 
#' @param by 
#' @param sep 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
merge_list <- function(x, by, sep = '.', ...) {
  
  if (!inherits(x, "list")) stop('x must be a list')
  
  
  results <- x[[1]]
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      results <- merge(results, x[[i]],
                       by = by,
                       suffixes = c(paste0(sep, i-1), paste0(sep, i)), 
                       ...)
    }  
  }
  return(results)
}

# library(tidyverse)
# data(acme)
# dt <- ToDataFrameTree(acme, "pathString", "p", "cost")
# dt <- as.data.table(dt)
# dt[, a := list(lapply(1:.N, function(x) sample(letters, 5)))]
# dt[, b := list(lapply(1:.N, function(x) 1:5))]
# dt[, c := list(lapply(1:.N, function(x) sample(letters, 1)))]
# x <- as.Node(dt)
# 
# test <- ToDataTableTree(x, attributes = 'all')
# x$Get('c', simplify = TRUE)
# test$a
# test$b
# test$a <- (x$Get('a'))
# test$a
# 
# x$Get('cost')
# attributes = 'all'
# pathString = FALSE
# filterFun = NULL
# pruneFun = NULL

#' modifies the data.tree::ToDataFrameTree function so that the attributes can be passed via the `attributes` arguement, 
#' instead of having to be specifiied individually.  
#' TODO: iteratively calling cbind() is not very smart
#' @param x 
#' @param attributes either 'all' (default), or character vector specifying which attributes of `x` to include in the data frame
#' @param listColumns are any of the attributes of type list and should be handeled as such in the resulting data.table? Problem atm: if set to TRUE all (!!) columns of the returned data.table are of type list!
#' @param ...  
#'
#' @return data.table
#' @export
#'
#' @examples
ToDataTableTree <- function(x, ...,  attributes = NULL, pathString = FALSE, 
                            filterFun = NULL, pruneFun = NULL, 
                            listColumns = FALSE) {
  # if attribute is not specified, call usual function
  if (is.null(attributes)) {
    df <- ToDataFrameTree(x, ..., pruneFun = pruneFun, filterFun = filterFun)
    return(as.data.table(df))
  }
  
  # otherwise iterate through all attributeds
  if (length(attributes) == 1 && attributes == 'all') attributes <- x$attributesAll
  if (isTRUE(pathString)) attributes <- c(attributes, 'pathString')
  
  df <- ToDataFrameTree(x, pruneFun = pruneFun, filterFun = filterFun)
  df <- as.data.table(df)
  
  if (length(attributes) >= 1) {
    #attributes_class <- get_attr_class(x ,attributes = attributes)
    for (i in 1:length(attributes)) {
      # print(attributes_class)
      # if (attributes_class[[i]] %in% c('numeric', 'character', 'logical')) {
      #   # special treatment for type list columns
      #   newcol <- x$Get(attributes[i], simplify = TRUE)
      # } else {
      #   newcol <- x$Get(attributes[i], simplify = FALSE)
      # }
      
      newcol <- x$Get(attributes[i], simplify = !listColumns)  
      df <- cbind(df, newcol, 
                  pruneFun = pruneFun, 
                  filterFun = filterFun)
      # df <- cbind(df, ToDataFrameTree(x, attributes[i], 
      #                                 pruneFun = pruneFun, 
      #                                 filterFun = filterFun)[,-1])
    }  
  }
  
  # special case: an attribute is of type list
  setnames(df, c('levelName', attributes))
  return(df)
}

 #' overwrites data.tree::ToDataFrameTree because the original function
#' has a bug if an attribute is of type 'list'
#'
#' @param x 
#' @param ... 
#' @param pruneFun 
#'
#' @return
#' @export
#'
#' @examples
# ToDataFrameTree <- function(x, ..., pruneFun = NULL) {
#   attrs <- list(...)
#   classes <- get_attr_class(x, unlist(attrs))
#   attrs_type_list <- attrs[isTRUE(sapply(classes, function(x) 'list' %in% x))]
#   attrs_type_data.table <- attrs[isTRUE(sapply(classes, function(x) 'data.table' %in% x))]
#   df <- data.tree::ToDataFrameTree(x = x, ..., pruneFun = pruneFun)
#   for (i in attrs_type_list) {
#     df[[i]] <- x$Get(i)
#   }
#   for (i in attrs_type_data.table) {
#     df[[i]] <- x$Get(i, simplify = FALSE)
#   }
# 
# 
#   return(df)
# }
# 


#' Get the classes of all attributes of a data.tree
#'
#' @param tree 
#'
#' @return a named list
#' @export
#'
#' @examples
#' x1 <- data.table(
#' pathString = c(
#'   'A',  'A/b'# /i', 'A/a/ii', 'A/b/i'
#' ),
#' foo = list(list(m = runif(1), n = rnorm(1))),
#' bar = runif(2)
#' )
#' 
#' y1 <- as.Node(x1)

get_attr_class <- function(tree, attributes = 'all') {
  if (identical(attributes, 'all')) attributes <- tree$attributesAll
  
  class_list <- lapply(attributes, function(x) {
    classes <- tree$Get(function(node) class(node[[x]]))
    classes[is.null(classes)] <- NULL
    classes <- unique(na.omit(classes))
    classes <- classes[classes != 'NULL']
    #classes <- classes[classes == 'NULL']
    if (length(classes) != 1) warning('different class for same attribute')
    return(classes)
  })
  names(class_list) <- attributes
  return(class_list)
}

get_attr_length <- function(tree, attributes = 'all') {
  if (identical(attributes, 'all')) attributes <- tree$attributesAll
  
  class_list <- lapply(attributes, function(x) {
    lengths <- tree$Get(function(node) length(na.omit(node[[x]])), 
                        filterFun = function(node) has_attribute_tree(node, x))
    #    lengths <- length[length > 0]
    lengths <- unique(na.omit(lengths))
    lengths <- lengths[lengths != 'NULL']
    #lengths <- lengths[lengths == 'NULL']
    #if (length(lengths) != 1) warning('different class for same attribute')
    return(lengths)
  })
  names(class_list) <- attributes
  return(class_list)
}


#' Merge two data.trees by their path name (and, optionally, by other arguments).
#' If data.trees have the same attributes they get suffixes: 
#' foo.1, foo.2, etc. if x is a list
#' foo.x, foo.y if x and y are data.trees
#' Suffixes can be changed by setting the separetor character via `sep` (if x is a list), 
#' `suffixes` (if x and y are data.trees, see: `?data.table::merge`)
#'
#' @param x a data.tree or a list of data.trees
#' @param y a 2nd data.tree
#' @param attributes_x attributes of `x` to consider 
#' @param attributes_y attributes of `y` to consider
#' @param ... 
#' @param by should include 'levelName'. Be careful when specifying other attributes
#' @param by.x should include 'levelName', if specified (default = NULL)
#' @param by.y should include 'levelName', if specified (default = NULL)
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'x1 <- data.frame(
#'pathString = c(
#'  'A',  'A/b'# /i', 'A/a/ii', 'A/b/i'
#'), 
#'foo = 'bar' 
#')
#'y1 <- as.Node(x1)
#'x2 <- data.frame(
#'  pathString = c('A',  'A/c'),
#'  foo = 'bar2' )
#'  y2 <- as.Node(x2)
#'  test <- merge_trees(list(y1, y2))

merge_trees <- function(x, y, 
                        by = 'pathString', 
                        by.x = NULL,
                        by.y = NULL,
                        attributes_x = 'all', 
                        attributes_y = 'all',
                        ...) {
  #stop('currently not working !!!!')
  if (!('pathString' %in% by)) stop('argument "by" needs to include "pathString"')
  if (!is.null(by.x) 
      & !is.null(by.y) 
      & !('pathString' %in% by.x 
          & 'pathString' %in% by.y)) {
    stop('if specified, both arguments "by.x" and "by.y" need to include "pathString"')
  }
  
  if (missing(x)) stop('argument x must be provided')
  
  
  if (inherits(x, "list")) {
    xnew <- vector('list', length(x))
    for (i in 1:length(x)) {
      xnew[[i]] <- ToDataTableTree(x[[i]], 
                                   attributes = attributes_x, 
                                   pathString = TRUE)
    }
    result <- merge_list(xnew, by = by, sort = FALSE, all = TRUE, ...)
    
  } else {
    # both x and y are provided
    xnew <- ToDataTableTree(x, attributes = attributes_x, pathString = TRUE)
    ynew <- ToDataTableTree(y, attributes = attributes_y, pathString = TRUE)
    result <- merge(xnew, ynew, by = by, ..., 
                    all = TRUE, sort = FALSE)
  }
  # combine the differetn pathStrings into one
  pathStrings <- result[, grep('pathString', names(result)), with = FALSE]
  pathStrings <- apply(pathStrings, 1, function(x) {
    if (length(unique(x)) == 1) return(x[1])
    return(unique(na.omit(x)))
  })
  result <- result[, -grep('pathString', names(result)), with = FALSE]
  result <- cbind(result, pathString = pathStrings)
  
  return(as.Node(result, pathName = 'pathString', pathDelimiter = '/'))
}


# x <- nir_tree2
# y <- crf_tree2
# attributes_x = c('category_code', 
#                  'gas', 
#                  'party', 
#                  'year', 
#                  'classification',
#                  'emissions_NIR', 
#                  'sd_NIR', 
#                  'cv_NIR') 
# attributes_y = c('party', 
#                  'year', 
#                  'category_code', 
#                  'gas',
#                  'classification',
#                  'emissions_CRF') 
# by = c('pathString', 
#        'party', 
#        'year', 
#        'gas',
#        'category_code', 
#        'classification')
# n_cores = 3


merge_trees_nested <- function(x, y, 
                               parties = NULL, 
                               gases = NULL, 
                               years = NULL, 
                               classifications = NULL,
                               attributes_x, attributes_y, 
                               by,
                               n_cores = 1, 
                               ...
) {
  
  if (is.null(parties)) {
    parties_x <- unique(names(x))
    parties_y <- unique(names(y))
    parties <- parties_x[parties_x %in% parties_y]
    
  } 
  if (is.null(gases)) {
    gases_x <- unique(unlist(lapply(x, names)))
    gases_y <- unique(unlist(lapply(y, names)))
    gases <- gases_x[gases_x %in% gases_y]
  }
  if (is.null(years)) {
    years_x <- unique(unlist(lapply(x, function(i) lapply(i, names))))
    years_y <- unique(unlist(lapply(y, function(i) lapply(i, names))))
    years <- years_x[years_x %in% years_y]
  }
  if (is.null(classifications)) {
    classifications_x <- unique(unlist(lapply(x, function(i) lapply(i, function(j) lapply(j, names)))))
    classifications_y <- unique(unlist(lapply(y, function(i) lapply(i, function(j) lapply(j, names)))))
    classifications <- classifications_x[classifications_x %in% classifications_y]
  }
  if (is.null(classifications)) {
    # classifcation level not there 
    merged_tree <- nested_list_named(list(party = parties, 
                                          gas = gases, 
                                          year = years) )
    parcomb <- expand.grid(party = parties, 
                           gas = gases, 
                           year = years)
  } else {
    merged_tree <- nested_list_named(list(party = parties, 
                                          gas = gases, 
                                          year = years, 
                                          classification = classifications) )
    parcomb <- expand.grid(party = parties, 
                           gas = gases, 
                           year = years, 
                           classification = classifications, 
                           stringsAsFactors = FALSE)
    
  }
  
  
  for (i in 1:nrow(parcomb)) {
    iparty <- parcomb[i,]$party
    igas <- parcomb[i,]$gas
    iyear <- parcomb[i,]$year
    if (!is.null(classifications)) {
      iclassification <- parcomb[i,]$classification
      #print(paste(iparty, igas, iyear))
      ix <- x[[iparty]][[igas]][[iyear]][[iclassification]]
      iy <- y[[iparty]][[igas]][[iyear]][[iclassification]]
    } else {
      ix <- x[[iparty]][[igas]][[iyear]]
      iy <- y[[iparty]][[igas]][[iyear]]
    }
    if(!is.null(ix) & !is.null(iy)) {
      itree <-  merge_trees(ix, iy,
                            attributes_x = attributes_x, 
                            attributes_y = attributes_y, 
                            by = by)
      if (!is.null(classifications)) {
        merged_tree[[iparty]][[igas]][[iyear]][[iclassification]] <- itree      
      } else {
        merged_tree[[iparty]][[igas]][[iyear]] <- itree
      }
      
    }
  }
  return(merged_tree)
} 



#' NOT NEEDED anymore. Learned that the each data.tree has an attribute called 
#' `pathString` that does what the funciton here tries to mimic. 
#' 
#' Recreates the pathName attribute that gets lost when converting a data.frame 
#' to a data.tree object using `ToDataFrameTree()`. Can be helpful when converting 
#' as data.frame to a data.tree and and then back to data.frame
#'
#' @param x a data.tree object
#' @param name 
#' @param pathDelimiter 
#'
#' @return
#' @export
#'
#' @examples
recreate_pathName <- function(x, name = 'pathName', pathDelimiter = '/') {
  x$Do(function(node){
    if (isRoot(node)) {
      node[[name]] <- GetAttribute(node, 'name')
    } else {
      parent_name <- GetAttribute(node$parent, name)
      self_name <- GetAttribute(node, 'name')
      node[[name]] <- paste(parent_name, self_name, sep = pathDelimiter)    
    }
  }, traversal = 'pre-order')
  return(x)
}


#' Checks which elements of a vector of path strings are leafs and which not
#'
#' @param x a character vector containing the path (e.g. 1.A.1.b, 2.B)
#' @param pathDelimiter the delimiter used to seperate nodes in x, default is '.'
#'
#' @return a boolean vector of same size as `x`
#' @export
#'
#' @examples

is_leaf_pathString <- function(x, pathDelimiter = '.') {
  x <- data.frame(pathString = x)
  tree <- as.Node(x = x, 
                  pathDelimiter = pathDelimiter)
  isleaf <- ToDataFrameTree(tree, 'isLeaf', 'pathString')
  isleaf$pathString <- gsub('/', pathDelimiter, isleaf$pathString)
  
  isleaf <- merge(x, isleaf, by = 'pathString', all.x = TRUE, sort = FALSE)
  
  return(isleaf$isLeaf)
}



# Recursively apply function to all trees in a nested list
#' Title
#'
#' @param object 
#' @param fun 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
nested_lapply <- function(object, fun, ...) {
  if (inherits(object, "data.frame") | inherits(object, 'Node')) {
    return(fun(object, ...))
  } else  if (inherits(object, "list")) {
    return(lapply(object, function(x) nested_lapply(x, fun, ...)))
  } else  if (is.null(object)) return(NULL)
  stop("List element must be either a data frame or another list")
}



#' 
#'
#' @param object 
#' @param idcols 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples

nested_rbindlist <- function(object, idcols = NULL, ...) {
  if (is.null(idcols)) temp_colname <- NULL
  else {
    temp_colname <- random_name(10)
    while(temp_colname %in% colnames(object)) {
      temp_colname <- random_name(10)
    }
  }
  
  #object <- do.call('c', unlist2(object, class = 'data.table'))
  object <- unlist2(object, class = 'data.table')
  object <- rbindlist(object, idcol = temp_colname, ...)
  
  if (!is.null(idcols)) {
    cols <- as.data.table(str_split(object[[temp_colname]], 
                                    '\\.', simplify = TRUE))
    setnames(cols, idcols)
    object <- cbind(cols, object)
    object[, (temp_colname) := NULL]
  }
  return(object[])
}


#' The same as unlist(recursive = TRUE), but for nested list where the leafs are 
#' data.tables. 
#' (with base unlist data.table are unlisted as well)
#'
#' @param x 
#' @param class 
#'
#' @return
#' @export
#'
#' @examples
unlist2 <- function(x, class = 'data.table') {
  if (is.list(x) && length(x) > 0 & !(class %in% class(x[[1]]))) {
    x <- unlist(x, recursive = FALSE)
    unlist2(x)
  } else {
    return(x)
  }
}


#' More generic than `base::sum`:
#'  works for: 
#'  - numeric/units vectors
#'  - list of numeric/units scalars
#'  - list of numeric/units vectors
#'  
#' Features:
#' - Preserves units (as opposed to all `*apply` functions) https://github.com/r-quantities/units/issues/103
#' - allows to remove NA's (as opposed to `Reduce("+", x)`)
#'
#' @param x 
#' @param na.rm 
#'
#' @return
#' @export
#'
#' @examples
sum2 <- function(x, na.rm = TRUE) {
  if (isTRUE(na.rm)) {
    x <- lapply(x, function(i) replace(i, is.na(i), 0))
    # if (!is.numeric(x)){
    #   print(typeof(x))
    #   x <- as.numeric(x)
    # } 
  }
  
  Reduce("+", x)
}


#' Title
#'
#' @param dt 
#' @param cols 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
nest_dt <- function(dt, cols, by, newcol = 'V1') {
  dt <- dt[, list(newcol=list(.SD)), by = by, .SDcols = cols]
  setnames(dt, 'newcol', newcol)
  return(dt[])
}


#' Title
#'
#' @param dt 
#' @param col 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
unnest_dt <- function(dt, col, by) {
  dt[ , get(col)[[1]], by = by]
}




#' Create an empty named nested list
#' 
#' taken from :https://stackoverflow.com/questions/17567172/nested-lists-how-to-define-the-size-before-entering-data
#'
#' @param listnames 
#'
#' @return
#' @export
#'
#' @examples
nested_list_named <- function(listnames) {
  len <- sapply(listnames, length)
  if(length(len) == 1){
    x <- vector("list", len)
    names(x) <- listnames[[1]]
    x
  } else {
    lapply(split(1:len[1],listnames[1]), function(...) nested_list_named(listnames[-1]))
  }
}


#' Flatten a nested list to a one-level list
#'
#' @details
#' The function is essentially a slightly modified version of \code{flatten2}
#' provided by Tommy at \href{https://stackoverflow.com/a/8139959/2906900}{stackoverflow.com} who
#' has full credit of the implementation of this function.
#' @param x \code{list}
#' @param use.names \code{logical}. Should the names of \code{x} be kept?
#' @param classes A character vector of class names, or "ANY" to match any class.
#' @author \href{https://stackoverflow.com/users/662787/tommy}{Tommy}
#' @export
#' @examples
#' p <- list(a=1,b=list(b1=2,b2=3),c=list(c1=list(c11='a',c12='x'),c2=3))
#' list.flatten(p)
#'
#' p <- list(a=1,b=list(x="a",y="b",z=10))
#' list.flatten(p, classes = "numeric")
#' list.flatten(p, classes = "character")
list.flatten <- function(x, use.names = TRUE, classes = "ANY") {
  len <- sum(rapply(x, function(x) 1L, classes = classes))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items)))
    names(y) <- nm
  y
}



#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
all_TRUE <- function(x) {
  if (!is.logical(x)) stop('x must be a vector of type "logical"')
  return(sum(x) == length(x))
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
all_FALSE <- function(x) {
  if (!is.logical(x)) stop('x must be a vector of type "logical"')
  return(sum(x) == 0)
}


#' Random name generator
#'
#' @param length 
#'
#' @return
#' @export
#'
#' @examples
random_name <- function(length) {
  paste(sample(c(LETTERS, letters, 0:9), 
               length, replace = TRUE), 
        collapse = '')
}


#' propagate correspondences upwards the tree. 
#' 
#' in other words: iteratively merge correspondences from all child nodes
#'
#' @param node 
#' @param attribute 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
propagate_correspondences <- function (node, attribute, ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(node) || has_attribute_tree(node, attribute)) 
    return(unlist(GetAttribute(node, attribute, ...)))
  values <- lapply(node$children, function(x) {
    #v <- GetAttribute(x, attribute, format = identity, ...)
    v <- GetAttribute(x, attribute, format = TRUE, ...)
    # v <- x$Get(attribute, ...)
    if (na.omit(length(v)) > 0) return(unlist(v))
    propagate_correspondences(x, attribute,  ...)
  })
  
  result <- sort(unique(unname(unlist(values))))
  return(result)
}





#' Title
#'
#' @param data 
#' @param parties 
#' @param gases 
#' @param years 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
convert_to_tree <- function(data, parties = NULL, gases = NULL, 
                            years = NULL, ...) {
  tree_list <- create_named_list(parties)
  
  if (is.null(parties)) parties <- data$party %>% unique
  if (is.null(gases)) gases <- data$gas %>% unique
  if (is.null(years)) years <- data$year %>% unique
  
  classes <- names(data)[sapply(data, class) == 'list']
  
  for (iparty in parties) {
    cat(iparty, "\t")
    tree_list[[iparty]] <- create_named_list(gases)
    for (igas in gases) {
      cat(igas, "")
      tree_list[[iparty]][[igas]] <- create_named_list(years)
      for (iyear in years) {
        
        # convert idataframe to tree
        tree <- as.Node(x = (data[party == iparty 
                                  & gas == igas 
                                  & year == iyear]), 
                        pathName = "id", 
                        pathDelimiter = ".")
        tree$Do(function(node) node$isNonEmptyLeaf <- isNonEmptyLeaf(node, iyear))
        
        tree$Do(function(node) {
          for (attribute in node$attributes) {
            if (is.list(node[[attribute]]) && length(node[[attribute]][[1]]) == 0) {
              node$RemoveAttribute(attribute)
            }
          }
        })
        
        tree$Do(function(node) {
          myclasses <- node$attributes[node$attributes %in% classes]
          if (!is.null(myclasses)) {
            values <- sapply(myclasses, function(x) node[[x]])
            values <- unlist(values, recursive = FALSE)
            values <- lapply(values, as.data.table)
            values <- rbindlist(values, idcol = 'classification')
            if (nrow(values) > 0) node$data <- values 
          }
        })
        
        
        tree_list[[iparty]][[igas]][[iyear]] <- tree
      }
      
    }
    cat("\n")
  }
  return(tree_list)
}

#' Title
#'
#' @param data 
#' @param parties 
#' @param gases 
#' @param years 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 

# iparty <- 'DEU'
# igas <- 'N2O'
# iyear <- '2015'
convert_to_tree_detailed <- function(data, parties = NULL, 
                                     gases = NULL, years = NULL, ...) {
  tree_list <- create_named_list(parties)
  
  if (is.null(parties)) parties <- data$party %>% unique
  if (is.null(gases)) gases <- data$gas %>% unique
  if (is.null(years)) years <- data$year %>% unique
  
  for (iparty in parties) {
    cat(iparty, "\t")
    tree_list[[iparty]] <- create_named_list(gases)
    for (igas in gases) {
      cat(igas, "")
      tree_list[[iparty]][[igas]] <- create_named_list(years)
      for (iyear in years) {
        classes <- data[party == iparty 
                        & gas == igas 
                        & year == iyear]$classification %>% 
          unique
        classes <- na.omit(classes)
        tree_list[[iparty]][[igas]][[iyear]] <- create_named_list(classes)
        for (iclass in classes) {
          # convert idataframe to tree
          tree <- as.Node(x = (data[party == iparty 
                                    & gas == igas 
                                    & year == iyear
                                    & classification == iclass]), 
                          pathName = "id", 
                          pathDelimiter = ".")
          tree$Do(function(node) node$isNonEmptyLeaf <- isNonEmptyLeaf(node, iyear))
          tree_list[[iparty]][[igas]][[iyear]][[iclass]] <- tree
          
        }
      }
    }
    cat("\n")
  }
  return(tree_list)
}



#' Extracts the last n characters of a character string x
#'
#' @param x a character string
#' @param n the last n elements to extract
#'
#' @return a character string of length n
#' @export
#'
#' @examples
substr_right <- function(x, n){
  if (length(x) > n) warning(paste0('You want to extract the last ', n, 
                                    ' characters of a string of length ', nchar(x), 
                                    '. All characters of x a returned.'))
  nch <- nchar(x)
  substr(x, nch-n+1, nch)
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
normalize_UNFCCC_classfications <- function(x) {
  x <- tolower(x)
  x <- gsub(' ', '_', x)
  x <- gsub('\\/', '-', x)
  return(x)
}




#' Title
#' 
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 


as.sparse.matrix <- function(mat, rownames = NULL, colnames = NULL,
                             na.rm = FALSE,
                             suffices = c('.row', '.col')) {
  
  mat <- reshape2::melt(mat, na.rm = na.rm)
  mat <- as.data.table(mat)
  setnames(mat, c('row', 'col', 'value'))
  if (is.factor(mat$row)) mat$row <- as.character(mat$row)
  if (is.factor(mat$col)) mat$col <- as.character(mat$col)
  
  if (!(is.null(rownames) | is.null(colnames))) {
    # check for duplicates
    dup_rows <- colnames(rownames) %in% colnames(colnames)
    dup_cols <- colnames(colnames) %in% colnames(rownames)
    colnames(rownames)[dup_rows] <- paste0(colnames(rownames)[dup_rows], suffices[1])
    colnames(colnames)[dup_cols] <- paste0(colnames(colnames)[dup_cols], suffices[2])
  }
  
  if (!is.null(colnames)) {
    mat <- merge(mat, cbind(colnames, col = (1:nrow(colnames))), 
                 by = 'col')
    #mat[, col := NULL]
  }
  
  if (!is.null(rownames)) {
    mat <- merge(mat, cbind(rownames, row = (1:nrow(rownames))), 
                 by = 'row')
    #mat[, row := NULL]
  }
  
  
  setcolorder(mat, c('row', 'col', colnames(rownames), colnames(colnames)))
  return(mat[])
}


# mat <- Z
# colnames <- data.table(country = c(LETTERS[1:ncol(mat)]), 
#                        industry = letters[1:ncol(mat)])
# rownames <- data.table(country = c(LETTERS[4 + (1:ncol(mat))]), 
#                        industry = letters[4 + (1:ncol(mat))])
# 
# 
# as.sparse.matrix(Z, colnames = colnames, rownames = rownames, suffices = c('x', 'y'))
# as.sparse.matrix(Z, colnames = colnames[,1], rownames = rownames)

#' Title
#'
#' @param x a sparse matrix in the form a either a data.frame or data.table. Needs to have 3 columns. Default order: row | col | value. If order differs please specify `row`, `col` and `value` arguments. Row and col columns can be either integers (representing the location in the matrix) or character.
#' @param row which column of x represent the row-index? default 1
#' @param col which column of x represent the column-index? default 2
#' @param value which column of x represent the value? default 3
#' @param keep.names only considered if the `row` and `col` columns of `x` are of type character. 
#'
#' @return
#' @export
#'
#' @examples

as.dense.matrix <- function(x, row = 1, col = 2, value = 3, 
                            keep.names = TRUE) {
  if (mode(x) != "data.frame") x <- as.data.frame(x)
  
  mat <- matrix(NA, ncol = length(unique(x[,col])), 
                nrow = length(unique(x[,row])))
  mat[cbind(as.factor(x[,row]), as.factor(x[,col]))] <- x[,value] # as.factor needed to also work with non-integers row/col IDs
  
  if(isTRUE(keep.names) & (is.character(x[,row]) | is.character(x[,col]))) {
    rownames(mat) <- levels(as.factor(x[,row]))
    colnames(mat) <- levels(as.factor(x[,col]))
  }
  
  return(mat)
}



#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
sum_samples <- function(x) {
  list(Reduce(`+`, x))
}


#' Title
#' Determine if range of vector is FP 0.
#'
#' @param x 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
get_current_filename <- function() {
this.path::removeext(basename(this.path::this.path()))
}



#' Save the output of a script in the form [[script_name]][[.type]]. 
#'
#' @param x 
#' @param path default: path2output e.g. ./intermediate_results/V3
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
save_results <- function(x, path = path2output, 
                         filename = get_current_filename(), 
                         type = '.RData', suffix = '', ...) { 
  if (grepl('^([[:digit:]])(?=[[:lower:]])', filename, perl = TRUE) 
      | grepl('^([[:digit:]]_)', filename)) {
    filename <- str_split(filename, '_', n = 2, simplify = TRUE)[,2]  
  }
  if (type == '.RData') {
    saveRDS(x, file.path(path, paste0(filename, suffix, type)))
  } else if (type == '.feather') {
    arrow::write_feather(x, file.path(path, paste0(filename, suffix, type)))
  } else if (type == '.csv') {
    fwrite(x, file.path(path, paste0(filename, suffix, type)), ...)

  }  else {
    stop("type not correcly specified. only implemented for .RData and .feather")
  }
}

#' Save the output of a script in the form [[script_name]][[.type]]. 
#'
#' @param x 
#' @param path default: path2output e.g. ./intermediate_results/V3
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
save_results_xlsx <- function(x, path = path2output, 
                         filename = get_current_filename(), 
                         type = '.xlsx', suffix = '') { 
  if (grepl('^([[:digit:]])(?=[[:lower:]])', filename, perl = TRUE) 
      | grepl('^([[:digit:]]_)', filename)) {
    filename <- str_split(filename, '_', n = 2, simplify = TRUE)[,2]  
  }
  rio::export(x, file.path(path, paste0(filename, suffix, type)))
}


#' Save the output of a script in the form [[script_name]][[.type]]. 
#'
#' @param x 
#' @param path default: path2output e.g. ./intermediate_results/V3
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
save_plot <- function(path = file.path(path2output, 'plots'),
                      plot = last_plot(),
                         filename = get_current_filename(), 
                         type = '.png', suffix = '') { 
  if (grepl('^([[:digit:]])(?=[[:lower:]])', filename, perl = TRUE) 
      | grepl('^([[:digit:]]_)', filename)) {
    filename <- str_split(filename, '_', n = 2, simplify = TRUE)[,2]  
  }
  ggsave(filename = file.path(path, paste0(filename, suffix, type)), 
         plot = plot)
}


#' Title
#'
#' @param x vector
#' @param y vector
#' @param mode either 1: looking for common elements (default), or -1: looking for non-common elements
#'
#' @return
#' @export
#'
#' @examples
common_elements <- function(x, y, mode = 1, ignore.case = FALSE) {
  x <- unique(x)
  y <- unique(y)
  if (isTRUE(ignore.case)) {
    x <- tolower(x)
    y <- tolower(y)
  }
  if (mode == -1) res <- y[y %in% x]
  else res <- x[x %in% y]
  return(res)
}

#' Title
#'
#' @param x 
#' @param y 
#' @param mode 
#'
#' @return
#' @export
#'
#' @examples
#' 
non_common_elements <- function(x, y, mode = 1, ignore.case = FALSE) {
  x <- unique(x)
  y <- unique(y)
  if (isTRUE(ignore.case)) {
    x <- tolower(x)
    y <- tolower(y)
  }
  if (mode == -1) res <- y[!(y %in% x)]
  else res <- x[!(x %in% y)]
  return(res)
}





#' Replace one/several rows of a data.table 
#'
#' @param data the data.table
#' @param rows the row indices to be replaced (use `data[i, which = TRUE]` to retrieve them)
#' @param new_row a data.table with 1 row and same column order than the original data.table
#'
#' @return
#' @export
#'
#' @examples
replace_DT_rows <- function(data, rows, new_row) {
  if (!isTRUE(all.equal(colnames(data), colnames(new_row)))) {
    stop('arguments `data` and `new_row` need to have samw columns names in same order')
  }
  data[rows[1:nrow(new_row)]] <- new_row
  if (length(rows) > nrow(new_row)) data <- data[-rows[(nrow(new_row)+1):length(rows)]]
  return(data)
}

#' Title
#'
#' @param names a vector (character or numeric). Numeric is converted to character.  
#'
#' @return a list of the same length as the names vector
#' @export
#'
#' @examples
#' 
create_named_list <- function(names) {
  vector(mode = "list", length(names)) %>% 
    setNames(names)
  
}


#' Get use or supply shares from SUTs (result of script 0a_prepare_exiobase_SUTs.R)
#' 
#' Extract use/supply shares for 
#' - (a) specific product(s) 
#' - from (a) specific countr(ies)
#' - in specific industries
#' - in specific countries
#' 
#'
#' @param x a supply or use table (result of script 0a_prepare_exiobase_SUTs.R)
#' @param products 
#' @param industries 
#' @param product_countries 
#' @param industry_countries 
#'
#' @return
#' @export
#'
#' @examples 
# sut = use
# products = temp$EXIOBASE_products %>% unlist %>% unique
# industries = temp$EXIOBASE_code %>% unlist %>% unique %>% as.factor()
# industry_countries = c('FIN', 'DEU')
# product_countries <- 'all'

get_SUT_shares <- function(sut, 
                           products = 'all', 
                           industries = 'all', 
                           product_countries = 'all', 
                           industry_countries = 'all') {
  #sut <- copy(sut)
  #print(sut$product_code)
  #print(is.data.table(sut))
  if (length(products) == 1 && products == 'all') {
    products <- sut$product_code
  }
  if (length(industries) == 1 && industries == 'all') industries <- sut$industry_code
  if (length(industry_countries) == 1 && industry_countries == 'all') industry_countries <- sut$country_industry
  SUT_shares <- sut[
    product_code %in% products
    & industry_code %in% industries
    & country_industry %in% industry_countries
    , list(value = sum(value))
    , by = .(industry_code)
  ]
  SUT_shares[, country_total := sum(value)]
  SUT_shares[, share := value / country_total]
  
  
  if (nrow(SUT_shares) == 0) {
    # product not used in any of the industries --> equally share them among all
    # TODO: more elaborate
    SUT_shares <- data.table(industry_code = industries, 
                             share = 1 / length(industries))
  }
  
  return(SUT_shares[, .(industry_code, share)])
  
}


#' Removes the '&'-part from all '&'-CRF-categories (e.g. 1.A.2&3.2 --> 1.A.2)
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
remove_helper_CRF_categories <- function(x) {
  x <- str_split(x, '\\.') 
  
  # check whether & is part of the last level (e.g. 1.A.2&3&5) --> would mean that it cannot be removed, throw error
  check <- sapply(x, function(i) i[length(i)])
  check <- check[grepl('\\&', check)]
  if (length(check) > 0) stop('the &-part of the category code is not supposed to be the most detailed level --> cannot remove it, think of a solution (probably some error here, because there should always be more detailed emissions data from CRF')
  
  x <- lapply(x, function(i) i[!grepl('\\&', i)]) 
  x <- lapply(x, function(i) paste(i, collapse = '.')) 
  x <- simplify(x)
  
  return(x)
}


#' Unnest a data.table with respect to the 'sample' column 
#'
#' @param dt a data.table, must have column 'sample' (list of vectors of same legnth)
#' @param sample not used currently (still hard coded)
#' @param copy 
#'
#' @return
#' @export
#'
#' @examples
#' (dt <- data.table(
#' id = LETTERS[1:10],
#' sample = lapply(1:10, function(x) seq(x, x + 5))
#' ))
#' unnest_sample(dt)
unnest_sample <- function(dt, sample = 'sample', copy = TRUE) {
  if (isTRUE(copy)) dt <- copy(dt)
  # dt[, sample := list(lapply(sample, function(x) data.table(run = 1:length(x), 
  #                                                           value = x)))]
  dt[, sample := list(lapply(sample, function(x) tibble(run = 1:length(x), 
                                                            value = x)))]
  dt <- as.data.table(tidyr::unnest(dt, col = c(sample)))
  
  return(dt)
}



#' Calculate Summary Statistics
#'
#' This function calculates various summary statistics based on a numeric vector column in a data.table.
#'
#' @param dt A data.table object with a column named 'sample' containing numeric vectors of length N.
#' @return The input data.table with additional columns for mean, median, standard deviation, coefficient of variation,
#' relative mean, and confidence intervals.
#' @import data.table
#' @import purrr
#' @keywords data manipulation
#' @export
calculate_summary_statistics <- function(dt) {
  dt[, mean := map_dbl(sample, mean, na.rm = TRUE)]
  dt[, median := map_dbl(sample, median, na.rm = TRUE)]
  dt[, sd := map_dbl(sample, sd, na.rm = TRUE)]
  dt[, cv := sd / mean]
  dt[, mean_rel := mean / sum(mean, na.rm = TRUE), by = gas]
  dt[, CI97.5 := map_dbl(sample, quantile, probs = 0.975, na.rm = TRUE)]
  dt[, CI2.5 := map_dbl(sample, quantile, probs = 0.025, na.rm = TRUE)]
  dt[, sample_norm := mapply(function(x,y) x / y, 
                                         x = sample,
                                         y = mean, 
                                         SIMPLIFY = FALSE)]
  return(dt[])
}


#' Create a hierachical ID (node path) from the CRF categories 
#' and CRF classifictations. 
#' Needed to create combined tree. 
#'
#' @param category_code_id 
#' @param classification_hierarchy 
#'
#' @return
#' @export
#'
#' @examples
create_master_id_crf <- function(category_code_id, classification_hierarchy) {
  id_new <- str_replace(classification_hierarchy, '\\|', '.')
  id_new <- paste0(category_code_id, '.', id_new)
  id_new <- str_replace(id_new, '.total_for_category', '')
  return(id_new)
}





#' makes a UNFCCC data.tree coherent
#' 
#' @param itree should have at least the following identifiers/keys: party, gas, year, classification
#' @param emissions 
#' @param sd 
#' @param cv 
#'
#' @return
#' @export
#'
#' @examples
# make_tree_coherent <- function(itree, 
#                                #keys = c('party', 'year', 'gas', 'classification'),
#                                emissions = NULL, 
#                                sd = NULL, 
#                                cv = 'cv_NIR'
# ) {
#   #itree <- nir_tree$DEU$CO2$`2015`$total_for_category
#   # _a) pass all fixed variables to upper nodes
#   
#   itree$Set(party = unique(na.omit(itree$Get('party'))),
#             year = unique(na.omit(itree$Get('year'))), 
#             gas = unique(na.omit(itree$Get('gas'))), 
#             classification = unique(na.omit(itree$Get('classification'))))
#   
#   itree$Do(function(node) {
#     if (is.null(node$category_code)) {
#       node$category_code = node$pathString %>% 
#         gsub('\\/', '.', .) %>%
#         gsub('I', '', .) %>% 
#         gsub('^\\.', '', .)
#     }
#   })
#   
#   if (!is.null(emissions)) {
#     # _b) Aggregate emissions data
#     itree$Do(function(node) {
#       if (is.null(node[[emissions]])) {
#         node[[emissions]] <- Aggregate(node, attribute = emissions, 
#                                        aggFun = function(x)  { 
#                                          x <- sum(x, na.rm = TRUE)
#                                          x <- ifelse(x == 0, NA, x)
#                                        })
#         
#       }
#     }, 
#     traversal = "post-order")
#     #itree$Get('emissions_NIR')  
#   }
#   
#   if (!is.null(sd)) {
#     # _c) Propagate uncertainty
#     itree$Do(function(node) {
#       if (is.null(node[[sd]])) node[[sd]] <- propagate_uncertainty(node, sd)
#     }, 
#     traversal = "post-order")
#     #itree$Get('sd_NIR')    
#   }
#   
#   if (!isFALSE(cv) & !is.null(sd)) {
#     # _d) caluclate CV
#     itree$Do(function(node) {
#       node[[cv]] <- node[[sd]] / node[[emissions]]
#     }, 
#     traversal = "post-order")
#     #itree$Get('cv_NIR')    
#   }
#   
#   return(itree)
# }




