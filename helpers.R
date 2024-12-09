library(data.table)
library(kit)
library(collapse)
library(openxlsx2)
library(magrittr)

`%not in%` <- Negate(`%in%`)

`%without%` <- setdiff

`%++%` <- paste0

`%is%` <- inherits

`%equals%` <- identical

# To avoid typing single or double quotes:
# symbols -> strings
v <- function(...) 
  substitute(list(...)) %>% 
  as.list %>% 
  tail(-1) %>% 
  as.character

# Example:
#  renameColumns("Sickness/Health care" -> SICK,
#                Disability -> DISA,
#                "Old age" -> OLD,
#                Survivors -> SURVIV,
#                "Family/Children" -> FAM,
#                Unemployment -> UNEMPLOY,
#                Housing -> HOUSE,
#                "Social exclusion n.e.c." -> EXCLU)
renameColumns <- function(dt, ...) {
  pairs <-  
    substitute(list(...)) %>% 
    as.list %>% 
    tail(-1) %>% 
    lapply(. %>% as.list %>% tail(-1) %>% rev) 
  from <-
    pairs %>% 
    sapply(. %>% .[[1]] %>% as.character)
  to <-
    pairs %>% 
    sapply(. %>% .[[2]] %>% as.character)
  setnames(dt, from, to)
}

# Import all sheets in the Excel file as a list of data.frames
# like in the previous package version (openxlsx)
readAllSheets <- function(xlsx_file_name, drop_empty_sheets=TRUE,
                          suppress_warnings=TRUE, ...)
  xlsx_file_name %>% 
  read_sheet_names() %>% 
  sapply(function(x)
    try(`if`(suppress_warnings,suppressWarnings,identity)
        (read_xlsx(xlsx_file_name,x,...)), silent=TRUE) %>%  
      `if`(inherits(.,'try-error') &&
             attr(.,'condition')$message=='dims are inf:-inf', # empty worksheet
           `if`(!drop_empty_sheets, data.frame())
           ,.),
    simplify=FALSE) %>% 
  .[!sapply(.,is.null)]

# Filtering R data.table rows by condition within groups
filterDTrowsWithinGroups <-
  function(DT, row_filtering_expression, by, dot_is_list=TRUE)
    eval(bquote({
      if (dot_is_list)
        . <- list # data.table's alias
      DT[DT[, .(substitute(row_filtering_expression))
            , by = .(substitute(by))]
         [[if (is.list(.(substitute(by))))
             length(.(substitute(by))) + 1
           else
             2]]]
    }))

# R function for dealing with (ignoring) missing/empty arguments inside ellipsis (...)
# > list(a=1,,b=2:10)
# Error in list(a = 1, , b = 2:10) : argument 2 is empty
# > listWithoutEmptyArgs(a=1,,b=2:10)
# list(a = 1, b = 2:10)    
listWithoutEmptyArgs <- function(...)
  eval(Filter(\(x) !identical(as.character(x), "") || identical(x,""),
              bquote(.(substitute(list(...))))))

# R function to avoid the repetitions in situations like `list(first = first, second = second, third = third)`
namedList <- function(...) {
    # Capture the variable names as symbols
    # and convert symbols to character names
    var_names <- as.character(as.list(substitute(list(...)))[-1])
    # Create a named list
    stats::setNames(mget(var_names, envir = parent.frame()), var_names)
}

