
convert_NACErev2_to_tree <- function(x, code) {
  # currently only works until 2nd level (eg. A03, C11, ...)
  x <- copy(x)
  level1_vec <- x[get(code) %in% LETTERS][[code]]
  for (i in level1_vec) {
    x[grepl(i, x[[code]]), code_tree := i]
  }
  # special treatment: households
  x[grepl('HH', x[[code]]), code_tree := 'HH']
  x[grepl('SD', x[[code]]), code_tree := 'SD']
  x[grepl('TOTAL', x[[code]]), code_tree := 'TOTAL']
  
  
  x[nchar(get(code)) > 1 & !(get(code) %in% c('HH', 'SD', 'TOTAL')), 
    code_tree := paste0(code_tree, '.', get(code))]
  x[get(code) != 'TOTAL', code_tree := paste0('TOTAL.', code_tree)]
  tree <- as.Node(x, pathName = 'code_tree', pathDelimiter = '.')
  return(tree)
}


get_NACErev2_leaves <- function(x, code) {
  tree <- convert_NACErev2_to_tree(x, code)
  xnew <- ToDataTableTree(tree, attributes = 'all', filterFun = isLeaf)
  setcolorder(xnew, names(x))
  return(xnew[])
}


is_NACErev2_leaf <- function(x) {
  is_nace_code <- is_NACErev2_code(x) | grepl('HH', x) | grepl('TOTAL', x)
  is_duplicated <- duplicated(x)
  
  tree <- convert_NACErev2_to_tree(x = data.table(nace_r2 = x, 
                                                  position2 = 1:length(x)),
                                   code = 'nace_r2')
  tree$Set(isleaf = FALSE, filterFun = isNotLeaf)
  tree$Set(isleaf = TRUE, filterFun = isLeaf)
  isleaf <- ToDataTableTree(tree, 'nace_r2', 'isleaf', 'position2')
  setorder(isleaf, position2)
  isleaf <- na.omit(isleaf)
  isleaf <- isleaf$isleaf
  
  return(isleaf & is_nace_code & !is_duplicated)
  
}

get_emission_factors <- function(fuel, gas, unit = NULL) {
  # from IPCC guidelines 2006, CO2: V2_1_Ch1_Introduction.pdf, CH4/N2O: V2_3_Ch3_Mobile_Combustion.pdf 
  motor_gasoline <- data.table(
    FUEL = 'motor_gasoline',
    GAS = c('CO2','CH4', 'N2O'), 
    EF = c(set_units(69300, kg/TJ), 
           set_units(33, kg/TJ), 
           set_units(3.2, kg/TJ))
  )
  gas_diesel_oil <- data.table(
    FUEL = 'gas_diesel_oil',
    GAS = c('CO2','CH4', 'N2O'), 
    EF = c(set_units(74100, kg/TJ), 
           set_units(3.9, kg/TJ), 
           set_units(3.9, kg/TJ))
  )
  
  efdb <- rbind(motor_gasoline, gas_diesel_oil)
  return(efdb[FUEL == fuel & GAS == gas]$EF)
}






#' Get definitions of all Eurostat codes for all variables (=columns) of the data set.
#'
#' @param data A data.frame from get_eurostat(..., type = 'code')
#'
#' @return a list of dictionaries (=data.tables) for each variable of the supplied data (except time and value)
#' @export
#'
#' @examples

get_my_eurostat_dic <- function(data) {
  variables <- colnames(data)
  variables <- variables[which(!(variables %in% c('time', 'values')))]
  list <- create_named_list(variables)
  for (i in variables) {
    list[[i]] <- get_eurostat_dic(i) %>% 
      as.data.table %>%
      .[code_name %in% unique(data[[i]])]
    
  }
  return(list)
}


#' Checks which elements of a character vector are capital letters from A to U
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACE_level1 <- function(y) {
  y %in% LETTERS[1:21]
}

#' Checks which elements of a character vector have two digits as their last two characters
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
ends_with_2_numbers <- function(y) {
  !suppressWarnings(is.na(as.numeric(substr_right(y, 2))))
  
}

#' Checks which elements of a character vector have a capital letter from A to U as their first character
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
starts_with_NACE_letter <- function(y) {
  is_NACE_level1(substr(y, 1, 1))
}

#' Checks which elements of a character vector are of form [Capital Letter][Digit][Digit], such as
#' A03, C10, U99
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACE_level2 <- function(y) {
  return(starts_with_NACE_letter(y) & ends_with_2_numbers(y) & nchar(y) == 3)
}

#' Checks which elements of a character vector are combinations of NACErev2 level-2-sectors, such as: 
#' C10-12 or C10_C11
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_combination <- function(y) {
  return(is_combination_underscore(y) | is_combination_bar(y))
}


#' Title
#'
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
is_combination_underscore <- function(y) {
  z <- stringr::str_split(y, '_') 
  z <- lapply(z, function(x) {
    is_nace <- is_NACE_level2(x)
    sum(is_nace) == 2 | (isTRUE(is_nace[1]) & !is.nan(is.numeric(x[2])))
  }) 
  return(unlist(z) & grepl('_', y))
}

#' Title
#'
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
is_combination_bar <- function(y) {
  # 1. first 3 characters are nace rev 2 code
  # 2. contains "-"
  # 3. 5th and 6th characters are numbers
  is_NACE_level2(substr(y, 1,3)) & substr(y, 4,4) == '-'& ends_with_2_numbers(y)
}

#' Checks which elements of a character vector are valid NACE rev2 codes. 
#' Currently includes: 
#' - Level 1: A, B, ..., U
#' - Level 2: A01, C10, U99, ...
#' - Combinations: C10-12, C10_C11
#' 
#'  Not included: Level 3 and 4 codes, such as A01.01 or A01.01.11  
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACErev2_code <- function(y) {
  # currentyl only until level 2
  is_NACE_level1(y) | is_NACE_level2(y) | is_combination(y)
}


#' 
#' #' Converts a data.frame with NACE rev2 classification to a hierarchical data.tree. 
#' #' Currently only supports until 2nd level (e.g. A03, C11, ...)
#' #'
#' #' @param x a data.frame
#' #' @param code character: column name with NACE rev2 codes (need to be in the form A, B, ..., A01, C13, C10-12, C10_C11, ...)
#' #'
#' #' @return a data.tree object with all columns of x as attributes
#' #' @export
#' #'
#' #' @examples
#' convert_NACErev2_to_tree <- function(x, code) {
#'   # currently only works until 2nd level (eg. A03, C11, ...)
#'   level1_vec <- x[get(code) %in% LETTERS][[code]]
#'   for (i in level1_vec) {
#'     x[grepl(i, x[[code]]), code_tree := i]
#'   }
#'   x[nchar(get(code)) > 1, code_tree := paste0(code_tree, '.', get(code))]
#'   x[, code_tree := paste0('TOTAL.', code_tree)]
#'   tree <- as.Node(x, pathName = 'code_tree', pathDelimiter = '.')
#'   return(tree)
#' }



#' Title
#'
#' @param x 
#' @param code_name 
#' @param attribute 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
is_coherent_NACErev2 <- function(x, 
                                 code_name = 'code', 
                                 attribute = 'value', 
                                 tol = 1E-3) {
  tree <- convert_NACErev2_to_tree(x, code = code_name)
  return(is_coherent_tree(tree, attribute = attribute, tol = tol))
}



#' Disaggregate Correspondence table with combined NACErev2 sectors 
#' 
#' 
#' 
#' Combinations of sectors can either have the form C10_C11 (type underscore)
#' or C10-C13 (type bar). 
#' 
#' _ implies 'and' (C10_C11 = C10 and C11)
#' - implies 'from to' (C10-C13 = C10, C11, C12, C13)
#'
#' @param x a correspondence table (data.table in long format, 2 columns only) where either the source or the target classification include NACErev2 codes that are a combination of several categories. 
#' @param col numeric: where to find the column with NACErev2 combinations
#'
#' @return
#' @import data.table
#' @export
#'
#' @examples
#' 

# x <- data.table(source = c('C11_13'), 
#            target = c('a'))
# col = 1
# rm(x)
# rm(col)


split_NACE_combinations <- function(x, col) {
  colnames_original <- colnames(x)
  x <- copy(x)
  if (col == 1) setcolorder(x, c(2,1))
  setnames(x, c('V1', 'V2'))
  
  #if (is.numeric(col)) col <- colnames(x)[col]
  
  x_new <- x[!is_combination(V2)]
  x_under <- x[is_combination_underscore(V2)]
  x_bar <- x[is_combination_bar(V2)]
  
  # Type underscore (C10_C11) =========
  if (nrow(x_under) > 0) {
    for (i in seq_along(nrow(x_under))) {
      col_splitted <- strsplit(x_under[i,][['V2']], '_')
      col_splitted <- unlist(col_splitted)
      
      if (!is.na(as.numeric(col_splitted[2]))) {
        # second part of combination is only number without leading letter (e.g. C10_11)
        col_splitted[2] <- paste0(substr(col_splitted[1], 1, 1), col_splitted[2]) 
      }
      
      
      x_new <- rbindlist(
        list(
          x_new, 
          data.table(
            V1 = rep(x_under[i,]$V1, 2),
            V2 = col_splitted
          )
        ), 
        use.names = FALSE
      )
    }  
  }
  
  # Type bar (C10-C13) ==============
  if (nrow(x_bar) > 0) {
    # extract digits from .... to ....
    from_to <- stringr::str_extract_all(x_bar$V2, '(\\d+)') %>% 
      lapply(as.numeric)
    
    # extract Letters
    let <-  stringr::str_extract_all(x_bar$V2, '([A-Z]+)')
    
    # are all letters the same?
    problem <- sapply(let, function(x) length(unique(x)) != 1)
    if (sum(problem) > 0) {
      stop(x_bar[problem], ' is invalid NACE code combination')
    }
    
    
    # paste letter and number together
    for (i in 1:length(from_to)) {
      col_splitted <- paste0(let[[i]], from_to[[i]][1]:from_to[[i]][2])
      
      x_new <- rbindlist(
        list(
          x_new, 
          data.table(
            V1 = rep(x_bar[i,]$V1, length(col_splitted)),
            V2 = col_splitted
          )
        )
      ) 
    }
    
  }
  
  if (col == 1) setcolorder(x_new, c(2,1))
  #setroworder(x_new, neworder = x[[col]])
  setnames(x_new, colnames_original)
  
  return(x_new)
}






