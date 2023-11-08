# old ones =======
as.Node2 <- function(...) {
  dt <- as.data.table(list(...))
  return(as.Node(dt, 
                 pathName = "id", 
                 pathDelimiter = "."))
}

as.Node3 <- function(..., pathName = 'pathString', 
                     pathDelimiter = '.') {
  dt <- as.data.table(list(...))
  return(as.Node(dt, 
                 pathName = pathName, 
                 pathDelimiter = pathDelimiter))
}




make_tree_coherent <- function(itree,
                               emissions = NULL, 
                               sd = NULL, 
                               cv = 'cv_NIR', 
                               overwrite = FALSE
) {
  
  if (!is.null(emissions)) {
    # _b) Aggregate emissions data
    itree$Do(function(node) {
      if (isTRUE(overwrite) | is.na(GetAttribute(node, emissions))) {
        node[[emissions]] <- Aggregate(node, attribute = emissions, 
                                       aggFun = function(x)  { 
                                         x <- sum(x, na.rm = TRUE)
                                         x <- ifelse(x == 0, NA, x)
                                       })
        
      }
    }, 
    traversal = "post-order")
  }
  
  if (!is.null(sd)) {
    # _c) Propagate uncertainty
    itree$Do(function(node) {
      #print(node[[sd]])
      if (isTRUE(overwrite) | is.na(GetAttribute(node, sd))) node[[sd]] <- propagate_uncertainty(node, sd)
    }, 
    traversal = "post-order")
    #itree$Get('sd_NIR')    
  }
  
  if (!isFALSE(cv) & !is.null(sd)) {
    # _d) caluclate CV
    itree$Do(function(node) {
      node[[cv]] <- node[[sd]] / node[[emissions]]
    }, 
    traversal = "post-order")
    #itree$Get('cv_NIR')    
  }
  
  return(itree)
}





is_coherent_tree <- function(x, attribute, tolerance = 0.01) {
  # TODO: still not perfect
  if (!('Node' %in% class(x))) {
    stop("x must be a Node object (data.tree)")
  }
  
  out <- x$Get(function(node) {
    if (!node$isLeaf) {
      child_values <- sapply(node$children, GetAttribute, attribute = attribute)
      
      # # get child values
      # child_values <- lapply(node$children, GetAttribute, attribute = attribute)  
      # # sum childs
      # node$child_sum <- sum2(child_values, na.rm = TRUE)
      
      if (is.matrix(child_values))  {
        #child_values <- do.call(cbind, child_values)
        child_sum <-  rowSums(child_values, na.rm = TRUE)
      } else if(is.list(child_values)) {
        #print(child_values)
        stop('lkj')
      } else {
        child_sum <- sum(child_values, na.rm = TRUE)
        # TODO: eError in sum(child_values, na.rm = TRUE): invalid 'type' (list) of argument>
      }
      #print(child_values)
      if (identical(NA, unique(child_values))) return(NA)
      else {
        parent_value <- GetAttribute(node, attribute = attribute)
        if ('units' %in% class(parent_value)) parent_value <- drop_units(parent_value)
        
        return(isTRUE(all.equal(child_sum, 
                                parent_value, 
                                tolerance = tolerance)))
      }
    } else return(NA)
  })
  #return(out)
  return(sum(out == FALSE, na.rm = TRUE) == 0)
}


#' checks if Node is coherent
#'  if Node is a leaf (and possess `attribute`): returns TRUE
#'
#' @param node 
#' @param attribute 
#' @param tolerance 
#' @param na.rm if TRUE: NA are replaced with zeros, if FALSE: NA is kept as NA
#'
#' @return
#' @export
#'
#' @examples
is_coherent_node <- function(node, attribute, tolerance = 0.01, 
                             na.rm = TRUE) {
  # if (!has_attribute_tree(node, attribute)) {
  #   if (isTRUE(na.rm)){
  #     
  #   }  return(FALSE)
  # }
  
  if (isLeaf(node)) return(TRUE)
  
  # get node value
  parent_value <- GetAttribute(node, attribute)
  
  # replace NA's with zeros (if required)
  if(isTRUE(na.rm)) parent_value <- replace(parent_value, 
                                            is.na(parent_value), 
                                            0)
  
  # get child values
  child_values <- lapply(node$children, GetAttribute, attribute = attribute)  
  # sum childs
  child_sum <- sum2(child_values, na.rm = na.rm)
  
  return(isTRUE(all.equal(child_sum, 
                          parent_value, 
                          tolerance = tolerance)))
  
}







is_coherent_node2 <- function(node, attribute, tolerance = 0.01, 
                              na_ignore = TRUE) {
  
  if (isLeaf(node)) return(TRUE)
  
  # get node value
  parent_value <- GetAttribute(node, attribute)
  # get child values
  child_values <- lapply(node$children, GetAttribute, attribute = attribute)  
  # sum childs
  child_sum <- sum2(child_values, na.rm = TRUE)
  
  are_equal <- isTRUE(all.equal(child_sum, parent_value, tolerance = tolerance))
  
  if (isTRUE(na_ignore)) return(are_equal | is.na(parent_value))
  else return(are_equal)
}

#' Checks if tree is coherent (i.e. the sum of all childs equals their parent)
#' As comnpared to `is_coherent_tree()` this functions allows (if `na_ignore = TRUE`) 
#' to return `TRUE` also when parent node is `NA`.
#' 
#' @param tree 
#' @param attribute 
#' @param tolerance 
#' @param na_ignore 
#'
#' @return
#' @export
#'
#' @examples
is_coherent_tree4 <- function(tree, attribute, tolerance = 0.01, 
                              na_ignore = TRUE) {
  is_coherent <- tree$Get(function(node) {
    is_coherent_node2(node, attribute = attribute, 
                      tolerance = tolerance, 
                      na_ignore = na_ignore)
  })
  return(all(is_coherent))
}

#' HIghlight nodes that are not coherent (sum of childs != parent)
#' 
#' NOT working with units currently 
#'
#' @param tree 
#' @param attribute 
#' @param tolerance 
#' @param diff 
#' @param collapsed 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
visualize_coherence <- function(tree, attribute = 'value', tolerance = 1E-3, 
                                diff = 'rel', collapsed = FALSE, ...) {
  
  tree$Do(function(node) {
    if (attribute %in% node$attributes) {
      # for leaves: take original value
      if (isLeaf(node)) node$child_sum <- GetAttribute(node, attribute = attribute)
      # for non-leaves: take sum of children's value
      else {
        # get child values
        child_values <- lapply(node$children, GetAttribute, attribute = attribute)  
        # sum childs
        node$child_sum <- sum2(child_values, na.rm = TRUE)
        
      }
      
      # calculate difference between original value and sum of childs
      if (diff == 'abs') node$dif <- node[[attribute]] - node$child_sum
      if (diff == 'rel') node$dif <- (node[[attribute]] - node$child_sum) / node[[attribute]]
      if (isTRUE(all.equal((node$dif), 0, tol = tolerance))) node$dif <- NA
    }
  })
  
  p <- collapsibleTree2(tree, attributes = c(attribute, 'child_sum'), 
                        fillby = 'dif', collapsed = collapsed)
  
  try(tree$Do(function(node) node$RemoveAttribute("child_sum")), 
      silent = TRUE)
  try(tree$Do(function(node) node$RemoveAttribute("dif")), 
      silent = TRUE)
  p
  
}




#' Removes (prunes) all children of a node if their values (`attribute`) are not
#' coherent with the Node value. 
#'
#' @param node 
#' @param attribute 
#' @param tolerance 
#' @param na.rm see `is_coherent_node`
#'
#' @return
#' @export
#'
#' @examples
prune_incoherent_node <- function(node, attribute, tolerance = 0.01, na.rm = TRUE) {
  if (!is_coherent_node(node, attribute, tolerance, na.rm = na.rm)) {
    rm(list = names(node$children), envir = node)
    node$children <- NULL
  }
  return(node)
}

#' marks all children of a node if their values (`attribute`) are not
#' coherent with the Node value. 
#'
#' @param node 
#' @param attribute 
#' @param tolerance 
#' @param na.rm see `is_coherent_node`
#'
#' @return
#' @export
#'
#' @examples
mark_incoherent_node <- function(node, attribute, tolerance = 0.01, na.rm = TRUE) {
  if (!is_coherent_node(node, attribute, tolerance, na.rm = na.rm)) {
    #for (child in node$children) child[['coherent']] <- FALSE
    node[['coherent']] <- FALSE
  } else {
    #for (child in node$children) child[['coherent']] <- TRUE
    node[['coherent']] <- TRUE
  }
  return(node)
}


#' Removes (prunes) all subtrees of all "empty" Nodes, i.e. all which
#' do not possess the `attribute` (either NULL or NA). 
#' 
#' Also removes the Node itself! 
#'
#' @param tree 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
prune_empty_subtrees <- function(tree, attribute) {
  Prune(tree, pruneFun = function(node) {
    has_attribute_tree(node, attribute)
  })
}

#prune_empty_subtrees(acme_new, 'value')
#acme_new


#' Removes (prunes) all subtrees of all "empty" Nodes, i.e. all which
#' do not possess the `attribute` (either NULL or NA). 
#' 
#' Only removes leaves!! (as opposed to `prune_empty_subtree`)
#'
#' Use within tree$Do(...)!
#' @param tree 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
#' data(acme)
#' acme$IT$Outsource$p <- NULL
#' acme$Do(fun = prune_empty_subtrees2,
#'      attribute = 'p',
#'      traversal = 'pre-order')
#' acme
#' 
prune_empty_subtrees2 <- function(tree, attribute) {
  Prune(tree, pruneFun = function(node) {
    prune <- (isLeaf(node) & !has_attribute_tree(node, attribute))
    if (isTRUE(prune))  print(paste0('remove node ', node$pathString))
    return(!prune)
  })
  return(tree)
}



#' Title
#'
#' @param tree 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
prune_empty_leaves <- function(tree, attribute= 'emissions_CRF') {
  tree$Do(function(node) {
    if (isLeaf(node) && is.na(GetAttribute(node, attribute))) {
      print(paste0('remove node ', node$pathString))
      Prune(node, pruneFun = function(x) return(FALSE))
    }
  })
  return(tree)
}


#' Prunes all subtrees if the childrens values (`attribute`) are not
#' coherent with the parent value. 
#' 
#' Keeps the Parent Node !!! as opposed to `prune_empty_subtrees`
#'
#' @param tree 
#' @param attribute 
#' @param tolerance 
#' @param na.rm 
#' @param prune_empty_subtrees 
#'
#' @return
#' @export
#'
#' @examples
prune_incoherent_subtrees <- function(tree, attribute, tolerance = 0.01, 
                                      na.rm = TRUE, prune_empty_subtrees = TRUE) {
  if(isTRUE(prune_empty_subtrees)) prune_empty_subtrees(tree, attribute)
  tree$Do(prune_incoherent_node, attribute = attribute, 
          tolerance = tolerance, na.rm = na.rm)
  return(tree)
}


#' Prunes all subtrees if the childrens values (`attribute`) are not
#' coherent with the parent value. 
#' 
#' Keeps the Parent Node !!! as opposed to `prune_empty_subtrees`
#'
#' @param tree 
#' @param attribute 
#' @param tolerance 
#' @param na.rm 
#' @param prune_empty_subtrees 
#'
#' @return
#' @export
#'
#' @examples
mark_incoherent_subtrees <- function(tree, attribute, tolerance = 0.01, 
                                     na.rm = TRUE, prune_empty_subtrees = TRUE) {
  if(isTRUE(prune_empty_subtrees)) prune_empty_subtrees(tree, attribute)
  tree$Do(mark_incoherent_node, attribute = attribute, 
          tolerance = tolerance, na.rm = na.rm)
  return(tree)
}



#' Prunes all branches which have NA value (and not contribute to total)
#'
#' @param tree 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
prune_dead_branches <- function(tree, attribute = 'emissions_CRF', 
                                tolerance = 0.01) {
  tree$Do(function(node) {
    if (is_coherent_node(node, attribute, tolerance = tolerance)) {
      # node shoulde be coherent (otherwise kicking out branch might be not a good idead)
      child_emissions <- sapply(node$children, GetAttribute, 
                                attribute = attribute)
      if (has_some_na_not_all(child_emissions)) {
        # at least on child is without CRF emissions but not all
        for (i in which(is.na(child_emissions))) {
          # remove those childs
          print(paste0('remove node ', node$children[[i]]$pathString))
          rm(list = names(node$children)[[i]], envir = node)
          node$children[[i]] <- NULL
        }
      }  
    }
  }, filterFun = isNotLeaf, traversal = 'post-order')
  return(tree)  
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
#' 
#' 



# propagate_uncertainty_backwards_list <- function(tree, 
#                                                  emissions = 'emissions_CRF',
#                                                  uncertainty = 'cv_NIR',
#                                                  uncertainty_new = 'sample',
#                                                  N = 100) {
#   tree$Do(function(node) {
#     #if (isRoot(node)) node[[uncertainty_new]] <- node[[uncertainty]]
#     if (!isLeaf(node)) {
#       values <- propagate_uncertainty_backwards(node,
#                                                 emissions = emissions,
#                                                 uncertainty = uncertainty,
#                                                 uncertainty_new = uncertainty_new,
#                                                 N = N)
#       i <- 1
#       for (child in node$children) {
#         child[[uncertainty_new]] <- values[i]
#         i <- i + 1
#       }
#     }
#     
#     
#   },
#   traversal = "pre-order")
#   
#   
# }

# propagate_uncertainty_backwards <- function (node, 
#                                              emissions,
#                                              uncertainty,
#                                              uncertainty_new,
#                                              N = 100, ...) 
# {
#   if ("cacheAttribute" %in% names(list(...))) 
#     stop("cacheAttribute not supported anymore! Please use Do instead.")
#   if (isLeaf(node)) return(NULL)
#   
#   # node != leaf
#   try(node$children$`NA` <- NULL)
#   
#   result <- rep(NA, length(node$children))
#   values <- rep(NA, length(node$children))
#   i <- 1
#   for (child in node$children) {
#     myvalue <- GetAttribute(child, uncertainty, format = identity)
#     if (length(myvalue) == 0 || is.na(myvalue)) {
#       # child has no uncertainty value
#       values[i] <- GetAttribute(child, emissions, format = identity)
#     } else {
#       # child already has uncertainty value
#       result[i] <- myvalue
#     }
#     i <- i + 1
#   }
#   
#   if (sum(!is.na(result)) == length(node$children)) {
#     # all children have uncertainty values
#     return(result)
#   }
#   
#   if (length(result[is.na(result)]) == 1) {
#     # only ONE child has no uncertainty value: take uncertainty value from parent
#     # TODO: as currently implemented this only makes sense for length(node$children) == 1
#     # if node as several children but only one without uncertainty value, there should be a different procedure 
#     # (--> maximum entropy approach)
#     
#     result[is.na(result)] <- GetAttribute(node, uncertainty_new, format = identity)
#   }
#   
#   # otherwise: propagate uncertainty using dirichlet
#   # TODO: problem when there is a big divergence in emission between sectors. then dirichlet creates bullshit: see: gtools::rdirichlet(1E5, c(8.025588e-09, 1.000000e+00))
#   
#   
#   sample_parent <- truncnorm::rtruncnorm(N, 
#                                          a = 0, 
#                                          mean = node[[emissions]], 
#                                          sd = node[[uncertainty_new]] * node[[emissions]])
#   
#   alphas <- values / sum(values, na.rm = TRUE)
#   alphas <- alphas[!is.na(alphas)]
#   sample_children <- gtools::rdirichlet(N, alphas)
#   sample_combined <- sample_children * sample_parent
#   
#   #cat('has more detailed emissions data: ', node$name, '\n')
#   
#   return(sample_combined)
# }




has_attribute_tree <- function(node, attribute) {
  attr <- GetAttribute(node, attribute, nullAsNa = TRUE)
  attr <- unlist(attr)
  !(length(attr) == 1 && is.na(attr))
  #!is.na(attr) & !is.null(unlist(attr)) 
}

sample_nodes_without_uncertain_childs <- function(tree, 
                                                  emissions = 'emissions_CRF',
                                                  uncertainty = 'cv_NIR',
                                                  sample = 'sample',
                                                  N) {
  tree$Do(function(node) {
    if (!is.na(GetAttribute(node, uncertainty, nullAsNa = TRUE)) & 
        sum(unlist(lapply(node$children, has_attribute_tree, uncertainty))) == 0) {
      node[[sample]] <- truncnorm::rtruncnorm(N, a = 0, mean = node[[emissions]], 
                                              sd = node[[emissions]] * node[[uncertainty]])
    }
  })
  return(tree)
}

sample_leafs_without_uncertainy <- function(tree, 
                                            emissions = 'emissions_CRF',
                                            uncertainty = 'cv_NIR',
                                            sample = 'sample',
                                            N) {
  tree$Do(function(node) {
    if (has_attribute_tree(node, uncertainty)) {
      node[[sample]] <- truncnorm::rtruncnorm(N, a = 0, mean = node[[emissions]], 
                                              sd = node[[emissions]] * node[[uncertainty]])
    }
  }, filterFun = isLeaf)
  return(tree)
}



error_propagation_analytical <- function(x, na.rm = FALSE) {
  sqrt(sum(x^2, na.rm = na.rm))
}
error_propagation_analytical_backwards <- function(x, y, na.rm = FALSE) {
  sqrt(y^2 - sum(x^2, na.rm = na.rm))
}

# emissions = 'emissions_CRF'
# uncertainty = 'cv_NIR'
# sample = 'sample'
# N = 100


propagate_uncertainty_backwards <- function (tree, 
                                             emissions = 'emissions_CRF',
                                             uncertainty = 'cv_NIR',
                                             sample = 'sample',
                                             N = 100, 
                                             local_opts = list( "algorithm" = "NLOPT_LD_MMA", # optim options
                                                                "xtol_rel" = 1.0e-4 ),
                                             opts = list( "algorithm"= "NLOPT_GN_ISRES",
                                                          "xtol_rel"= 1.0e-4,
                                                          "maxeval"= 160000,
                                                          "local_opts" = local_opts,
                                                          "print_level" = 0 ),
                                             verbose = FALSE,
                                             ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(tree)) return(NULL)
  
  # node != leaf
  try(tree$children$`NA` <- NULL)
  
  tree$Do(function(node) {
    
    if (!isLeaf(node)) {
      has_cv <- sapply(node$children,  has_attribute_tree, uncertainty)
      has_sample <- sapply(node$children, has_attribute_tree, sample)
      # print(node$pathString)
      # print(has_cv)
      # print(has_sample)
      # 
      # do I have childs without uncertainty or sample value?
      if (sum(!(has_cv | has_sample)) > 0) {
        # at least one child has uncertainty value --> maxent
        
        leaves_path <- node$Get('pathString', filterFun = function(x) {
          isLeaf(x) & !has_attribute_tree(x, sample)
        })
        leaves_emissions <- node$Get(emissions, filterFun = function(x) {
          isLeaf(x) & !has_attribute_tree(x, sample)
        })
        
        
        if (!has_attribute_tree(node, sample)) {
          sample_parent <- truncnorm::rtruncnorm(N,
                                                 a = 0,
                                                 mean = node[[emissions]],
                                                 sd = node[[uncertainty]] * node[[emissions]])  
        } else {
          sample_parent <- node[[sample]]
        }
        
        
        # if (sum(!(has_cv)) == 1) {
        #   # only ONE child has no uncertainty value --> analytical error propagation
        #   # TODO: write own function for this
        #   childs_cv <- sapply(node$children, GetAttribute, uncertainty)
        #   childs_emissions <- sapply(node$children, GetAttribute, emissions)
        #   childs_sd <- childs_emissions * childs_cv
        #   
        #   mysd <- error_propagation_analytical_backwards(
        #     x = childs_sd,
        #     y = node[[emissions]] * node[[uncertainty]],
        #     na.rm = TRUE
        #   )
        #   mycv <- mysd / childs_emissions[is.na(childs_cv)]
        #   node[[names(has_cv[has_cv == FALSE])]][[uncertainty]] <- mycv
        #   
        #   mysample <- truncnorm::rtruncnorm(N,
        #                                     a = 0,
        #                                     mean = childs_emissions[is.na(childs_cv)],
        #                                     sd = mysd)
        #   
        #   node[[names(has_cv[has_cv == FALSE])]][[sample]] <- mysample
        #   
        # }
        # 
        if (sum(!(has_cv | has_sample)) > 0) {
          # more than one child has no uncertainty value --> maxent
          # TODO: write own function for this
          
          alphas <- leaves_emissions / sum(leaves_emissions, na.rm = TRUE)
          #alphas <- alphas[!is.na(alphas)]
          
          
          
          # find gamma using maxent
          tryCatch({
            res <- find_gamma_maxent2(shares = alphas,
                                      eval_f = eval_f, 
                                      ...)
          }, error = function(e) print(node))
          gamma <- res$solution
          
          node[['optim_output']] <- res
          node[['gamma']] <- res$solution
          
          # sample from dirichlet (sum to 1)
          sample_children <- gtools::rdirichlet(N, alphas * gamma)
          
          # get samples from children that have already
          if (sum(has_sample) > 0) {
            sample_other <- lapply(node$children[has_sample], GetAttribute, sample) %>% 
              do.call(cbind, .)
            sample_other <- rowSums(sample_other)  
          } else sample_other <- 0
          
          sample_combined <- sample_children * (sample_parent - sample_other)
          colnames(sample_combined) <- leaves_path
          
          # distribute samples to leaves
          node$Do(function(node) {
            node$sample <- (sample_combined[,node$pathString])
          }, filterFun =  function(x) {
            isLeaf(x) & !has_attribute_tree(x, sample)
          })
          
          # aggregate if necessary
          node$Do(function(node) {
            if (!has_attribute_tree(node, sample)
                & !has_attribute_tree(node, uncertainty)) {
              #sapply(node$children, GetAttribute, sample)
              
              node[[sample]] <- Aggregate(node, attribute = sample,
                                          aggFun = function(x)  {
                                            rowSums(unlist(x), na.rm = TRUE)
                                          })
              if (length(node[[sample]][node[[sample]] < 0]) > 0) {
                print(node)
                warning(paste0('negative sample ', node$name))
              }
            }
          }, filterFun = isNotLeaf, traversal = 'post-order')
        }
        
      }
    }
  }, traversal = 'pre-order')
  if (isTRUE(verbose)) cat('*')
  return(tree)
}


# aggregate_sample <- function(tree, sample = 'sample') {
#   tree$Do(function(node) {
#     if (is.null(node[[sample]])) {
#       node[[sample]] <- Aggregate(node, sample, 
#                                   aggFun = function(x) {
#                                     if (is.matrix(x)) rowSums(x, na.rm = TRUE)
#                                     else if (is.na(x)) NULL
#                                       else x
#                                   })  
#       if (length(node[[sample]][node[[sample]] < 0]) > 0) {
#         warning(paste0('negative sample ', node$name))
#       }
#     }
#   }, traversal = 'post-order') 
#   return(tree)  
# }


# Aggregate(tree3, 'sample', 
#           aggFun = function(x) {
#             if (is.matrix(x)) rowSums(x, na.rm = TRUE)
#             else if (is.na(x)) NULL
#             else x
#           })  
# 
# 




propagate_uncertainty_backwards_analytical <- function (tree, 
                                                        emissions = 'emissions_CRF',
                                                        uncertainty = 'cv_NIR',
                                                        sample = 'sample',
                                                        N = 100,
                                                        ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(tree)) return(NULL)
  
  # node != leaf
  try(tree$children$`NA` <- NULL)
  
  tree$Do(function(node) {
    
    if (!isLeaf(node)) {
      has_cv <- sapply(node$children, has_attribute_tree, uncertainty)
      
      # do I have childs without uncertainty or sample value?
      if (sum(!(has_cv)) == 1) {
        
        # only ONE child has no uncertainty value --> analytical error propagation
        # TODO: write own function for this
        childs_cv <- sapply(node$children, GetAttribute, uncertainty)
        childs_emissions <- sapply(node$children, GetAttribute, emissions)
        childs_sd <- childs_emissions * childs_cv
        
        # try({
        #   print(childs_sd[!is.na(childs_sd)])
        #   
        #   if (childs_sd[!is.na(childs_sd)] > (node[[emissions]] * node[[uncertainty]])) {
        #     warning(paste0('child sd ',  childs_sd[!is.na(childs_sd)], 
        #                    'larger than node sd ', (node[[emissions]] * node[[uncertainty]])))
        #   }  
        # })
        
        if (length(node$children) == 1) {    
          node$children[[1]][[uncertainty]] <- node[[uncertainty]] 
        } else {
          mysd <- error_propagation_analytical_backwards(
            x = childs_sd, 
            y = node[[emissions]] * node[[uncertainty]], 
            na.rm = TRUE
          )
          mycv <- mysd / childs_emissions[is.na(childs_cv)]
          node[[names(has_cv[has_cv == FALSE])]][[uncertainty]] <- mycv  
        }
        
        
        # mysample <- truncnorm::rtruncnorm(N,
        #                                   a = 0,
        #                                   mean = childs_emissions[is.na(childs_cv)],
        #                                   sd = mysd)
        # node[[names(has_cv[has_cv == FALSE])]][[sample]] <- mysample
        # 
        # # aggregate if necessary
        # node$Do(function(node) {
        #   if (!has_attribute_tree(node, sample) 
        #       & !has_attribute_tree(node, uncertainty)) {
        #     #sapply(node$children, GetAttribute, sample)
        #     
        #     node[[sample]] <- Aggregate(node, attribute = sample, 
        #                                 aggFun = function(x)  {
        #                                   rowSums(unlist(x), na.rm = TRUE)
        #                                 })        
        #   }
        # }, filterFun = isNotLeaf, traversal = 'post-order')  
        
      }
    }
  }, traversal = 'pre-order')
  
  
  return(tree)
}





has_ancestor_with <- function(node, attribute) {
  
  while(!is.null(node$parent)) {
    if (has_attribute_tree(node$parent, attribute)) return(TRUE)
    else node <- node$parent
  }
  return(FALSE)
}




fill_missing_uncertainty <- function (tree, 
                                      emissions = 'emissions_CRF',
                                      uncertainty = 'cv_NIR',
                                      sample = 'sample',
                                      N = 100,
                                      ...) 
{
  if ("cacheAttribute" %in% names(list(...))) 
    stop("cacheAttribute not supported anymore! Please use Do instead.")
  if (isLeaf(tree)) return(NULL) 
  # node != leaf
  try(tree$children$`NA` <- NULL)
  
  tree$Do(function(node) {
    if (is.na(GetAttribute(node, uncertainty)) # node does not have uncertainty info
        #  && !isRoot(node)
        && !(has_ancestor_with(node, uncertainty) # ... no ancestor with uncertainty
             | has_ancestor_with(node, sample))) { # .... or sample
      stop('assuming exp dist')
      node[[sample]] <- rexp(N, rate = 1 / node[[emissions]])
      
      
    }
  }, filterFun = isLeaf)
  return(tree)
  
}




convert_tree_to_dt <- function(tree, attributes = NULL,
                               levelName = TRUE,
                               pathString = FALSE,
                               filterFun = NULL, 
                               simplify = FALSE) {
  
  # TODO: solve for issue
  # Error in `[.data.table`(x, , unlist(data, recursive = FALSE), by = by) : \n  j doesn't evaluate to the same number of columns for each group\n
  if (is.null(attributes)) attributes <- tree$attributesAll
  if (isTRUE(pathString)) attributes <- c(attributes, 'pathString')
  
  dt <- lapply(attributes, function(x) {
    if ((identical(filterFun, isRoot) && unlist(get_attr_length(tree, x) > 1))
        || get_attr_class(tree, x) == 'nloptr') {
      # special treatment for Roots (error message when attr is a vector)
      list(tree[[x]])
    } else {
      tree$Get(x, 
               filterFun = filterFun, 
               simplify = FALSE, 
               traversal = 'pre-order') %>% 
        as.data.frame() %>% 
        as.matrix
    }
    
  }) 
  dt <- setNames(dt, attributes)
  
  if (isTRUE(levelName)) dt <- c(ToDataFrameTree(tree, 
                                                 filterFun = filterFun), 
                                 dt)
  #dt <- lapply(dt, function(x) simplify(x))
  dt <- lapply(dt, function(x) {
    if (is.array(x)) sapply(1:ncol(x), function(y) list(x[,y]))
    else x
  })
  dt <- lapply(dt, data.table)
  dt <- do.call(cbind, dt) 
  
  if (isTRUE(simplify)) {
    # all columns are of type list: --> simplify if possible! 
    # Exception: column 'sample' (always of type list)
    #dt <- dt[, lapply(.SD, simplify), .SDcols = names(dt)[!grepl('^sample', names(dt))]]
    for (col in names(dt)[!grepl('^sample', names(dt))]) {
      set(dt, j = col, value = simplify(dt[[col]]))
    }
  }
  setnames(dt, new = gsub('\\.V1', '', names(dt))) 
  return(dt[])
}

# new ones =================




propagate_sd <- function(tree, 
                         sd = 'sd_NIR') {
  tree$Do(function(node) {
    if (has_attribute_tree(node, sd)
        && length(node$children) == 1 
        && !has_attribute_tree(node$children[[1]], sd)) {
      node$children[[1]][[sd]] <- node[[sd]]
    }
  }, traversal = 'pre-order')
  return(tree)
}





sample_nodes_with_sd <- function(tree, 
                                 emissions = 'emissions_CRF',
                                 sd = 'sd_NIR',
                                 sdlog = 'sdlog', 
                                 meanlog = 'meanlog',
                                 sample = 'sample',
                                 N) {
  tree$Do(function(node) {
    if ((has_attribute_tree(node, sd)
         & !(length(node$children) == 1 && 
             has_attribute_tree(node$children[[1]], sd)))
        | (has_attribute_tree(node, sdlog)
           & !(length(node$children) == 1 && 
               has_attribute_tree(node$children[[1]], sdlog)))
        # node does not have only child which itself has SD
        #&& length(node$children) != 1
        #&& !has_attribute_tree(node$children[[1]], sd)) 
    ){
      if (has_attribute_tree(node, 'dist')) {
        if (node[['dist']] == 'truncnorm') {
          #print(node$dist)
          node[[sample]] <- truncnorm::rtruncnorm(N, a = 0, 
                                                  mean = node[[emissions]], 
                                                  sd = node[[sd]])  
        } else if (node[['dist']] == 'lognorm') {
          #print(node$dist)
          node[[sample]] <- rlnorm(n = N, meanlog = node[[meanlog]], 
                                   sdlog = node[[sdlog]])  
          
        }  
      }
    }
  }, traversal = 'pre-order')
  return(tree)
}

sample_nodes_disaggregate <- function(tree, 
                                      emissions = 'emissions_CRF',
                                      sd = 'sd_NIR',
                                      sample = 'sample',
                                      N, ...) {
  tree$Do(function(node) {
    if (has_attribute_tree(node, sample) && isNotLeaf(node)) {
      #childs_with_sd <- sapply(node$children, has_attribute_tree, sample)
      leaves_without_sample <- !sapply(node$leaves, has_attribute_tree, sample)
      #if (sum(!childs_with_sd, na.rm = TRUE) > 1) {
      if (all(leaves_without_sample)) {
        
        leaves_path <- node$Get('pathString', filterFun = function(x) {
          isLeaf(x) & !has_attribute_tree(x, sample) & has_attribute_tree(x, emissions)
        })
        leaves_emissions <- node$Get(emissions, filterFun = function(x) {
          isLeaf(x) & !has_attribute_tree(x, sample) & has_attribute_tree(x, emissions)
        })
        
        alphas <- leaves_emissions / sum(leaves_emissions, na.rm = TRUE)
        
        # find gamma using maxent
        tryCatch({
          res <- find_gamma_maxent2(shares = alphas,
                                    eval_f = eval_f)
        }, error = function(e) {
          # Code to execute if an error occurs
          print(paste("An error occurred:", conditionMessage(e), alphas))
          
          # Assign a default value to 'res'
          res <- list(error = e, 
                      solution = 1)
          })
        # TODO: <simpleError in fun(node, ...): object 'res' not found> 
        # print(alphas) gives: numeric(0) OR a vector with one 0
        # if (length(alphas) == 0) {
        #   print('here')
        # }
        
        gamma <- res$solution
        
        node[['optim_output']] <- res
        node[['gamma']] <- res$solution
        
        # sample from dirichlet (sum to 1)
        sample_children <- gtools::rdirichlet(N, alphas * gamma)
        
        # get samples from children that have already
        #if (sum(childs_with_sd, na.rm = TRUE) > 0) {
        # if (!all(leaves_without_sample)) {
        #     
        #   sample_other <- lapply(node$children[childs_with_sd], 
        #                          GetAttribute, 
        #                          sample) %>% 
        #     do.call(cbind, .)
        #   sample_other <- rowSums(sample_other)  
        # } else sample_other <- 0
        
        #sample_combined <- sample_children * (node[[sample]] - sample_other)
        sample_combined <- sample_children * (node[[sample]])
        colnames(sample_combined) <- leaves_path
        
        # distribute samples to leaves
        node$Do(function(node) {
          node$sample <- (sample_combined[,node$pathString])
          node[['dist']] <- 'dir'
        }, filterFun =  function(x) {
          isLeaf(x) & !has_attribute_tree(x, sample)
        })
      } 
    }
  }, traversal = 'pre-order')
  return(tree)
}

sample_nodes_missing <- function (tree, 
                                  emissions = 'emissions_CRF',
                                  sd = 'sd_NIR',
                                  sample = 'sample',
                                  N = 100,
                                  sd_default = 'sd_DEFAULT', 
                                  ...) 
{
  
  #if (isLeaf(tree)) return(NULL) 
  
  tree$Do(function(node) {
    if (!has_attribute_tree(node, sd) # node does not have uncertainty info
        & !has_attribute_tree(node, sample)
        #  && !isRoot(node)
        && !(has_ancestor_with(node, sd) # ... no ancestor with uncertainty
             | has_ancestor_with(node, sample))) { # .... or sample
      #if (length(node[[emissions]]) == 0) print(convert_tree_to_dt(node))
      
      if (!has_attribute_tree(node, emissions)) {
        node[[sample]] <- rep(NA, N)
      } else if (node[[emissions]] == 0) {
        node[[sample]] <- rep(0, N) 
      } else {
        #stop("Assuming exponential distribution for node")
        #print(node)
        node[['dist']] <- 'exp' #TODO: remove this comment!
        #node[[sample]] <- rexp(N, rate = 1 / node[[emissions]]) 
        
        node[[sample]] <- truncnorm::rtruncnorm(N, a = 0,
                                               mean = node[[emissions]],
                                               sd = 0.2 * node[[emissions]])
        if (TRUE) {
          if (is.character(sd_default) && has_attribute_tree(node, sd_default)) {
            node[[sample]] <- truncnorm::rtruncnorm(N, a = 0,
                                                    mean = node[[emissions]],
                                                    sd = node[[sd_default]])
          } else if (is.numeric(sd_default)) {
            node[[sample]] <- truncnorm::rtruncnorm(N, a = 0,
                                                    mean = node[[emissions]],
                                                    sd = sd_default)
          } else {
            warning('sd_default needs to be either character (name of the attribute where default SD is stored, or numeric (one default SD value)')
            node[[sample]] <- truncnorm::rtruncnorm(N, a = 0,
                                                    mean = node[[emissions]],
                                                    sd = 0.2 * node[[emissions]]) 
          } 
        }  
      }
    }
  }, filterFun = isLeaf)
  return(tree)
  
}

aggregate_sample <- function(tree, sample = 'sample') {
  tree$Do(function(node) {
    
    if (isNotLeaf(node) & is.null(node[[sample]])) {
      childs_have_sample <- sapply(node$children, has_attribute_tree, sample)
      if(all(childs_have_sample)) {
        childs_sample <- lapply(node$children, GetAttribute, sample)
        node[[sample]] <- rowSums(do.call(cbind, childs_sample), na.rm = FALSE)
        node[['dist']] <- 'agg' #TODO: remove this comment!
      }
    }
  }, traversal = 'post-order')
  return(tree)
}

sample_tree <- function(tree, 
                        emissions = 'emissions_CRF',
                        sd = 'sd_NIR', # TODO: use cv_NIR here and then calculate sd_CRF = cv_NIR * emissions_CRF
                        sample = 'sample',
                        N, 
                        sd_default = 'sd_DEFAULT', 
                        tol = 0.01,
                        ...) {
  
  #tree <- propagate_sd(tree, sd = sd)
  tree <- sample_nodes_with_sd(tree, emissions = emissions, 
                               sd = sd, sample = sample, N = N)
  tree <- sample_nodes_disaggregate(tree, emissions = emissions, 
                                    sd = sd, sample = sample, N = N, 
                                    ...)
  tree <- sample_nodes_missing(tree, emissions = emissions, 
                               sd = sd, sample = sample, N = N, 
                               sd_default = sd_default)
  tree <- aggregate_sample(tree, sample = sample)
  if (!is_coherent_tree(tree, sample, tolerance = tol)) warning('tree is not coherent')
  
  return(tree)
  
}

disaggregate_emissions_tree <- function(x, emissions = 'emissions_CRF', 
                                        proxy_data = 'emissions_NIR') {
  
  x$Do(function(node) {
    # 2. emissions_NIR more detailed than emissions_CRF
    if (!isLeaf(node)) {
      values <- disaggregate_emissions(node, 
                                       emissions = emissions, 
                                       proxy_data = proxy_data)
      i <- 1
      for (child in node$children) {
        child[[emissions]] <- values[i]
        i <- i + 1
      }
    }
  }, 
  traversal = 'pre-order')
  return(x)
}



create_UNFCCC_classification_tree <- function(x, value_var = 'value', 
                                              pathString = 'classification_hierarchy',
                                              by = c('year', 'party', 'gas', 
                                                     'category_code',
                                                     'id')) {
  trees <- x[,
             list(tree = list(
               list(
                 value = get(value_var),
                 #classification = classification,
                 #classification_hierarchy = classification_hierarchy,
                 pathString = get(pathString)
               ) %>%
                 as.data.table %>%
                 as.Node(.,
                         pathDelimiter = "|")
             ))
             , by = by]
  return(trees)
}

create_UNFCCC_category_tree <- function(x, value_var  = 'value',
                                        pathString = 'id',
                                        by = c('year', 'party', 'gas', 
                                               'classification', 
                                               'classification_hierarchy')) {
  
  
  trees <- x[,
             list(tree = list(
               list(
                 value = get(value_var),
                 #category_code = category_code, 
                 #category_name = category_name, 
                 #id = id, 
                 pathString = id
               ) %>%
                 as.data.table %>%
                 as.Node(.,
                         pathDelimiter = ".")
             ))
             , by = by]
  return(trees)
}

create_UNFCCC_classification_tree2 <- function(x, attributes, 
                                               pathString = 'classification_hierarchy',
                                               by = c('year', 'party', 'gas', 
                                                      'category_code',
                                                      'id')) {
  
  # Add pathString to vector of attributes
  attributes <- c(pathString, attributes)
  
  # Create nested data.table
  x <- x[, nest(.SD, tree = all_of(attributes)), 
         by = by, 
         .SDcol = (attributes)]
  
  # Convert nested data column to data.tree
  x[, tree := lapply(tree, as.Node, pathDelimiter = '|', pathName = pathString)]
  
  # set key
  setkeyv(x, by)
  
  return(x)
}
create_UNFCCC_category_tree2 <- function(x, 
                                         attributes,
                                         pathString = 'id',
                                         by = c('year', 'party', 'gas', 
                                                'classification', 
                                                'classification_hierarchy')) {
  
  # Add pathString to vector of attributes
  attributes <- c(pathString, attributes)
  
  # Create nested data.table
  x <- x[, nest(.SD, tree = all_of(attributes)), 
         by = by, 
         .SDcol = (attributes)]
  
  # Convert nested data column to data.tree
  x[, tree := lapply(tree, as.Node, pathDelimiter = '.', pathName = pathString)]
  
  # set key
  setkeyv(x, by)
  
  return(x)
}


create_UNFCCC_combined_tree2 <- function(x, 
                                         attributes,
                                         pathString = 'id2',
                                         by = c('year', 'party', 'gas')) {
  
  # Add pathString to vector of attributes
  attributes <- c(pathString, attributes)
  
  # Create nested data.table
  x <- x[, nest(.SD, tree = all_of(attributes)), 
         by = by, 
         .SDcol = (attributes)]
  
  # Convert nested data column to data.tree
  x[, tree := lapply(tree, as.Node, pathDelimiter = '.', pathName = pathString)]
  
  # set key
  setkeyv(x, by)
  
  return(x)
}


#' Excludes all emissions from biomass that do are not included in category total. 
#' 
#'
#' @param data UNFCCC emissions data, pre-treated (see `clean_UNFCCC_data()`)
#' @param path2ct 
#'
#' @return
#' @export
#'
#' @examples
exclude_biomass <- function(data) {
  
  
  # data2 <- data[, 
  #               list(tree = list(
  #                 list(
  #                   value = value,
  #                   pathString = pathString
  #                 ) %>% 
  #                   as.data.table %>% 
  #                   as.Node(., 
  #                           pathDelimiter = "|")
  #               ))
  #               , by = .(year, party, gas, category_code, id)]
  # 
  
  data[,
       tree := list(list(
         as.data.table(
           list(
             value = value,
             pathString = classification_hierarchy    
           ) 
         )))
       #as.Node(., pathDelimiter = "|")    
       , by = .(year, party, gas, category_code, id)]
  data[classification == 'biomass',
       tree := list(
         lapply(tree, as.Node, pathDelimiter = '|')
       )] 
  
  data[classification != 'biomass', tree := NA]
  
  data[!is.na(tree), 
       biomass_incl := list(sapply(
         tree, FUN = biomass_included
       ))]
  
  # Exclude all biomass rows that are not included in totals
  data <- data[biomass_incl == TRUE | is.na(biomass_incl)] #  
  
  # special treatment for invidivual cases
  data <- data[!(party == 'BGR' & category_code == '1.A.2.g.viii' 
                 & gas == 'CO2' & year == '2015' & classification == 'biomass')]
  
  # remove unecessary cols
  data[, tree := NULL]
  data[, biomass_incl := NULL]
  #data[, pathString := NULL]
  
  return(data)
  
}





# test1 <- data2[gas == 'CH4' & category_code == '1.A.4.b']$tree
# convert_tree_to_dt(test1)
# 
# test2 <- data2[category_code == '1.A.1' & gas == 'CO2']$tree[[1]]
# convert_tree_to_dt(test2)
#test3 <- data2$tree[[1]]

#' Checks, whether within the 'total for category' emissions of a UNFCCC category
#' (e.g. 1.A.1) emissions from "Biomass" are included or not. 
#' 
#' Takes a data.tree as input, e.g.:
#'                levelName
#' 1 total_for_category    
#' 2  ¦--biomass           
#' 3  ¦--gaseous_fuels     
#' 4  ¦--liquid_fuels      
#' 5  ¦--other_fossil_fuels
#' 6  ¦--peat              
#' 7  °--solid_fuels  
#'
#' @param tree a data.tree
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
biomass_included <- function(tree, tol = 0.0001) {
  if (!('biomass' %in% tree$Get('name'))) return(FALSE)
  biomass <- tree$biomass$value
  if (is.null(biomass) || is.na(biomass)) return(FALSE)
  
  total <- tree$value
  if (is.null(total) || is.na(total)) return(FALSE)
  
  if ('units' %in% class(total)) total <- drop_units(total)
  if ('units' %in% class(biomass)) biomass <- drop_units(biomass)
  
  
  tree$Do(function(node) {
    tree$temp <- Aggregate(node, attribute = 'value', aggFun = function(x) sum(x, na.rm = TRUE))    
  }, traversal = 'post-order', filterFun = isRoot)
  
  total_agg <- tree$temp
  tree$RemoveAttribute('temp')
  #tree$Do(function(node) node$RemoveAttribute('temp'))
  
  if (isTRUE(all.equal(total_agg, total, tolerance = tol))) {
    return(TRUE)
  }
  if (isTRUE(all.equal(total_agg - biomass, total, tolerance = tol))) {
    return(FALSE)
  }
  
  return(NA)
  
}

is_CRF_table <- function(x) {
  required_names <- c('year', 'party', 'gas', 'category_code', 
                      'classification', 'classification_hierarchy', 
                      'id') 
  missing <- non_common_elements(required_names, colnames(x))
  if (all(required_names %in% colnames(x))) return(TRUE)
  else {
    warning(paste0("x lacks columns: ", paste(missing, collapse = ', ')))
    return(FALSE)
  }
}



CRF_table2tree <- function(x, attributes = c('value'), 
                           type, pathString){
  if (isFALSE(is_CRF_table(x))) stop('x must be a CRF table')
  
  if (type == 'classification') {
    x <- create_UNFCCC_classification_tree(x, value_var = attributes) 
  } else if (type == 'category') {
    x <- create_UNFCCC_category_tree(x, value_var = attributes)    
  }
  
  return(x)
}


CRF_table2tree2 <- function(x, attributes = c('value'), 
                            type, pathString){
  if (isFALSE(is_CRF_table(x))) stop('x must be a CRF table')
  
  if (type == 'classification') {
    x <- create_UNFCCC_classification_tree2(x, attributes = attributes) 
  } else if (type == 'category') {
    x <- create_UNFCCC_category_tree2(x, attributes = attributes)    
  } else if (type == 'combined') {
    x <- create_UNFCCC_combined_tree2(x, attributes = attributes)    
  } 
  
  return(x[])
}

#x <- CRF_table2tree(testdata, type = 'category')

CRF_tree2table <- function(x, tree_column = 'tree',
                           na.omit = TRUE,  simplify = TRUE) {
  # Extract data from each tree as data.table --> nested data.table
  x[, data := list(lapply(get(tree_column), convert_tree_to_dt, 
                          pathString = TRUE, 
                          levelName = FALSE,
                          simplify = simplify))]
  
  # remove tree column
  x[, (tree_column) := NULL]
  
  # Flatten data.table, depending on which sort of tree it is
  if ('category_code' %in% colnames(x)) {
    type <- 'classification'
    by <- c('year', 'party', 'gas', 'category_code', 'id')
  } else if ('classification' %in% colnames(x)) {
    type <- 'category'
    by <- c('year', 'party', 'gas', 'classification', 'classification_hierarchy')
  }
  #x <- x[, unlist(data, recursive = FALSE), by = by]
  x <- unnest(x, cols = 'data')
  x <- as.data.table(x)
  
  if (type == 'classification') {
    x[, classification_hierarchy := 
        extract_CRF_classification_hierarchy_from_pathString(pathString)]
    x[, classification := extract_CRF_classification_from_pathString(pathString)]
    x[, pathString := NULL]
  }
  if (type == 'category') {
    x[, id := extract_CRF_category_id_from_pathString(pathString)]
    x[, category_code := extract_CRF_category_from_pathString(pathString)]
    x[pathString == 'I/A', category_code := '0.A']
    x[pathString == 'I/B', category_code := '0.B']
    x[, pathString := NULL]
  }
  
  if (isTRUE(na.omit)) x <- na.omit(x)
  setkeyv(x, c('year', 'party', 'gas','id', 'category_code',  
               'classification', 'classification_hierarchy'))
  setcolorder(x)
  setorderv(x, key(x))
  return(x)
}

extract_CRF_classification_hierarchy_from_pathString <- function(x) {
  return(gsub('\\/', '\\|', x))
}
extract_CRF_classification_from_pathString <- function(x) {
  return(str_split(x, '\\/') %>% 
           lapply(function(x) x[length(x)]) %>% 
           simplify)
}

extract_CRF_category_id_from_pathString <- function(x) {
  return(gsub('\\/', '\\.', x))
}

extract_CRF_category_from_pathString <- function(x) {
  x <- gsub('\\/', '\\.', x)
  return(str_replace(x, '\\I.', ''))
}




