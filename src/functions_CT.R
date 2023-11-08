
#' Parses (read + prepares) a correspondence table. 
#'
#' @param file the path to the .xlsx file
#' @param type one of 'UNFCCC' or 'EDGAR'
#'
#' @return
#' @export
#'
#' @examples
parse_CT <- function(file, type) {
  
  # load xlsx file
  ct <- read_excel(file)
  ct <- as.data.table(ct)
  
  # remove line breaks
  ct[proxy_data_source == 'SUPPLY\n', 
     proxy_data_source := 'SUPPLY']
  ct[proxy_data_source == 'USE\n', 
     proxy_data_source := 'USE']
  
  # convert exiobases correspondences from string --> vector/list
  ct[, EXIOBASE_code := str_split(EXIOBASE_code, 
                                  pattern = '(, \n|,\n|\n)') %>% 
       lapply(function(x) {
         x <- gsub('\n', '', x)
         x <- x[nchar(x) > 0]
         return(x)
       }
       )]
  
  ct[, EXIOBASE_name := str_split(EXIOBASE_name, 
                                  pattern = '(, \n|,\n|\n)') %>% 
       lapply(function(x) {
         x <- gsub('\n', '', x)
         x <- x[nchar(x) > 0]
         return(x)
       }
       )]
  
  # convert proxy details (EB products) from string --> vector/list
  if (type == 'UNFCCC') {
    ct[proxy_data_source %in% c('USE', 'SUPPLY'), 
       EXIOBASE_products := str_split(proxy_data_details, pattern = '(, \n|,\n|\n)') %>% 
         lapply(function(x)gsub('\n', '', x))]
    
  } else if (type == 'EDGAR') {
    ct[, EXIOBASE_products := str_split(EXIOBASE_products, pattern = '(, \n|,\n)') %>% 
         lapply(function(x)gsub('\n', '', x))]
  }
  
  
  ct[EXIOBASE_code == 'y01' & !is.na(proxy_data_source), 
     `:=`(proxy_data_source = NA, 
          EXIOBASE_products = NA)]
  
  # 'all' correspondences: replace 'all' by a vector of all EXIOBASE industries
  EB_industries <- ct$EXIOBASE_code %>% 
    unlist %>% 
    unique  %>% 
    grep('^i.', ., value = TRUE)
  
  ct[grepl('^all', EXIOBASE_code), EXIOBASE_code := list(EB_industries)]
  ct[sapply(EXIOBASE_code, function(x) 'all' %in% x),
     EXIOBASE_code := list(c(EB_industries, 'y01'))]
  
  
  # kick out all categories with no GHG emissions at all 
  #ct <- ct[!(is.na(CO2kt) & is.na(CH4kt) & is.na(N2Okt))]
  
  
  
  if (type == 'UNFCCC') {
    # convert countries from string --> vector/list
    ct[, CO2_list := str_split(CO2, pattern = ', ') %>% 
         lapply(function(x)gsub('\n', '', x))]
    ct[, CH4_list := str_split(CH4, pattern = ', ') %>% 
         lapply(function(x)gsub('\n', '', x))]
    ct[, N2O_list := str_split(N2O, pattern = ', ') %>% 
         lapply(function(x)gsub('\n', '', x))] 
    
    # find countries affected by at least one GHG
    ct$parties_affected <- apply(ct, 1, function(x) unique(c(unlist(x$CO2_list), 
                                                             unlist(x$CH4_list),
                                                             unlist(x$N2O_list))))    
    ct <- ct[, .(id, CRF_code, CRF_class, CRF_name, EXIOBASE_code, EXIOBASE_name, 
                 proxy_data_source, EXIOBASE_products, parties_affected)]
  } else if (type == 'EDGAR') {
    ct <- ct[, -c('Comment')]
    
  }
  
  return(ct)
}


#' Makes a parsed root UNFCCC-EXIOBASE correspondence table coherent, i.e.
#' propagates the correspondences to the upper levels of the hierarchy
#'
#' @param ct a correspondence table as parsed via `parse_CT(..., type = 'UNFCCC')`
#'
#' @return
#' @export
#'
#' @examples
make_CT_coherent <- function(ct) {
  col_order <- names(ct)
  
  # ct[is.na(EXIOBASE_code)]
  # ct[is.na(CRF_code)]
  # ct$proxy_data_source %>% unique
  # ct$CRF_code %>% unique
  
  # 1. Propagate correspondences from fuel/animal type to total_for_category =======
  ct <- split(ct, by = 'CRF_code')
  
  ct <- lapply(ct, function(x) {
    if (nrow(x[CRF_class != 'total_for_category']) > 0) { 
      #&& c("USE", 'SUPPLY') %in% x$proxy_data_source) {
      sub_classes <-  x[CRF_class != 'total_for_category'
                        , list(proxy_data_source = (c(na.omit(unlist(unique(proxy_data_source))))), 
                               EXIOBASE_code = list(c(na.omit(unlist(unique(EXIOBASE_code))))), 
                               EXIOBASE_products = list(c(na.omit(unlist(unique(EXIOBASE_products))))), 
                               parties_affected = list(c(na.omit(unlist(unique(parties_affected))))))]
      
      x[CRF_class == 'total_for_category'
        , `:=`(EXIOBASE_code = sub_classes$EXIOBASE_code, 
               proxy_data_source = sub_classes$proxy_data_source, 
               EXIOBASE_products = sub_classes$EXIOBASE_products, 
               parties_affected = sub_classes$parties_affected)]  
    }
    return(x)
    
  })
  
  ct <- rbindlist(ct)
  
  # 2. Propagate correspondences through category hierachy ======================
  
  ct[, pathString := paste0('I.', CRF_code)]
  ct <- split(ct, by = 'CRF_class')
  
  x <- ct$total_for_category
  
  ct <- lapply(ct, function(x) {
    #cat(x$CRF_class %>% unique, '\n')
    tree <- as.Node(x, pathDelimiter = '.')
    
    # Unlist attribute EXIOBASE_product. Problem: nodes without that attribute are a NULL list ([[1]] NULL)
    # no idea why (for other Attributes, e.g. Exiobase_code that's not the case)
    tree$Do(function(node) {
      if (has_attribute_tree(node, 'EXIOBASE_products')) {
        node[['EXIOBASE_products']] <- unlist(node[['EXIOBASE_products']])
        node[['EXIOBASE_code']] <- unlist(node[['EXIOBASE_code']])
        
      }
    })
    #ToDataTableTree(tree, attributes = 'EXIOBASE_products')
    #ToDataTableTree(tree, attributes = 'all')[grepl('1.A.2.g.vii', CRF_code)]
    
    tree$Do(function(node) {
      #if (!has_attribute_tree(node, 'EXIOBASE_code')) {
      node$EXIOBASE_code <- propagate_correspondences(node, 'EXIOBASE_code')
      #}
    }, traversal = 'post-order')
    
    tree$Do(function(node) {
      # if (!has_attribute_tree(node, 'EXIOBASE_code')) {
      node$EXIOBASE_name <- propagate_correspondences(node, 'EXIOBASE_name')
      #  }
    }, traversal = 'post-order')
    
    
    tree$Do(function(node) {
      # if (!has_attribute_tree(node, 'proxy_data_source')) {
      node$proxy_data_source <- propagate_correspondences(node, 'proxy_data_source')
      #}
    }, traversal = 'post-order')
    
    tree$Do(function(node) {
      #if (!has_attribute_tree(node, 'EXIOBASE_products')) {
      node$EXIOBASE_products <- propagate_correspondences(node, 'EXIOBASE_products')
      #}
    }, traversal = 'post-order')
    #ToDataTableTree(tree, attributes = 'all')[grepl('2.C.1', CRF_code)]
    # collapsibleTree2(tree, attributes = c('EXIOBASE_products', 
    #                                       'EXIOBASE_code'))
    tree$Do(function(node) {
      #if (!has_attribute_tree(node, 'EXIOBASE_products')) {
      node$parties_affected <- propagate_correspondences(node, 'parties_affected')
      #}
    }, traversal = 'post-order')
    tree$Do(function(node) {
      if (!has_attribute_tree(node, 'CRF_code')) {
        node$CRF_code <- node$pathString %>% 
          gsub('I\\/', '', .) %>% 
          gsub('\\/', '\\.', .)
      }
    })
    tree$Set(CRF_class = unique(na.omit(x$CRF_class)))
    
    return(ToDataTableTree(tree, attributes = 'all', listColumns = TRUE))
  })
  
  #ct$`total_for_category` %>% View
  ct <- rbindlist(ct, use.names = TRUE, fill = TRUE)
  #ct[grepl('diesel_oil', CRF_class)]
  
  # Format columns (as long as ToDataTableTree does deliver list columns only)
  ct[, CRF_class := unlist(CRF_class)]
  ct[, CRF_code := unlist(CRF_code)]
  ct[sapply(CRF_name, length) == 0, 
     CRF_name := NA]
  ct[, CRF_name := unlist(CRF_name)]
  ct[, id := unlist(id)]
  
  ct <- ct[CRF_code != 'I']
  ct[, id := paste0(CRF_code, '.', gsub(' ', '_', CRF_class))]
  setorder(ct, CRF_code, CRF_class)
  setcolorder(ct, col_order)
  
  ct[
    sapply(proxy_data_source, function(x) {
      (is.vector(x, mode = 'logical') && length(x) == 0) | (length(x) == 1 && x == '-')
    })
    , proxy_data_source := NA
  ]
  
  #ct[sapply(EXIOBASE_code, function(x) 'y01' %in% x)]
  
  return(ct[])
  
}





