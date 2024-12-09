library(data.table)
library(kit)
library(collapse)
library(openxlsx2)
library(magrittr)

EUMS <-
  c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", 
    "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", 
    "PT", "RO", "SI", "SK", "FI", "SE")
EUMSnames <-
    c("BE"='Belgium', "BG"='Bulgaria', "CZ"='Czechia', "DK"='Denmark', "DE"='Germany',
      "EE"='Estonia', "IE"='Ireland', "EL"='Greece', "ES"='Spain', "FR"='France', 
      "HR"='Croatia', "IT"='Italy', "CY"='Cyprus', "LV"='Latvia', "LT"='Lithuania',
      "LU"='Luxembourg', "HU"='Hungary', "MT"='Malta', "NL"='the Netherlands',
      "AT"='Austria', "PL"='Poland', "PT"='Portugal', "RO"='Romania', "SI"='Slovenia',
      "SK"='Slovakia', "FI"='Finland', "SE"='Sweden')

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

# R data.table: Summarize each column with each function and name the new columns automatically
#    ##    Usage example:
#    summaries(dt=my_dt,
#              col_names=c('x','y'),
#              fun_names=c('Sum','Mean','Median'),
#              additional_code='Count=length(x)',
#              by=c('A','B'))
#    ##    Generated code:
#    #     dt[,.(`Sum__x`=`Sum`(x),
#    #           `Sum__y`=`Sum`(y),
#    #           `Mean__x`=`Mean`(x),
#    #           `Mean__y`=`Mean`(y),
#    #           `Median__x`=`Median`(x),
#    #           `Median__y`=`Median`(y),
#    #           Count=length(x))
#    #       ,by=c("A","B")]
summaries <- function(dt, col_names, fun_names, additional_code=NULL, by=NULL, sep='__')
  eval(parse(
    text=
      expand.grid(var=col_names, fun=fun_names) %>% 
      {paste0('`',.$fun,sep,.$var,'`=`',.$fun,'`(',.$var,')')} %>%
      c(additional_code) %>% 
      paste(collapse=',') %>% 
      paste0('dt[,.(',.,'),by=c(',
             by %>% 
               gsub('"','\\"',.,fixed=TRUE) %>%
               paste0('"',.,'"') %>% 
               paste(collapse=','),
             ')]')
  ))

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

# mapply for all combinations of arguments or lapply for multiple vectors/lists of arguments
# A more efficient implementation avoiding
# expand.grid which copies the parameters/arguments multiple times,
# which is inefficient for large parameters (e.g. data.frames).
mlapply <- function(.Fun, ..., .Cluster=NULL, .parFun=parallel::parLapply) {
    `--List--` <-
        list(...)
    names(`--List--`) <-
        names(`--List--`) %>% 
        `if`(is.null(.),
             rep.int("", length(`--List--`)),
             .) %>% 
        ifelse(.=="", # for unnamed args in ...
               seq_along(.) %>% 
                   paste0(ifelse(.==1 | .>20 & .%%10==1, 'st', ""),
                          ifelse(.==2 | .>20 & .%%10==2, 'nd', ""),
                          ifelse(.==3 | .>20 & .%%10==3, 'rd', ""),
                          ifelse(.>3 & .<=20 | !(.%%10 %in% 1:3), 'th', "")) %>% 
                   paste("argument in mlapply's ..."),
               .)
    `--metadata--` <-
        data.frame(Name = paste0("`",names(`--List--`),"`"),
                   Len = lengths(`--List--`),
                   OriginalOrder = seq_len(length(`--List--`)),
                   stringsAsFactors=FALSE)
    eval(Reduce(function(previous,x)
        paste0('unlist(lapply(`--List--`$',x,',',
               'function(',x,')', previous,'),recursive=FALSE)'),
        x =
            `--metadata--` %>% 
            `[`(order(.$Len),) %>% 
            `$`(Name),
        init =
            `--metadata--` %>% 
            `[`(order(.$OriginalOrder),) %>% 
            `$`(Name) %>% 
            ifelse(grepl("argument in mlapply's ...",.,fixed=TRUE),
                   ., paste0(.,'=',.)) %>% 
            paste(collapse=',') %>%
            paste0('list(.Fun(',.,'))')) %>% 
            ifelse(.Cluster %>% is.null,
                   .,
                   sub('lapply(',
                       '.parFun(.Cluster,',
                       ., fixed=TRUE)) %>% 
            parse(text=.))
}

# A wrapper for base::split taking as an argument the expected number of sub-elements
split_into <- function(x, n, sorted=TRUE)
  ## x -- a vector (atomic or list)
  ## n -- the number of elements (groups)
  ## returns a list with n elements
  ## each containing some of the elements of x
  n %>%
  seq_len %>%
  rep.int(x %>%
            length %>%
            divide_by(n) %>%
            ceiling) %>%
  `if`(sorted, sort(.), .) %>%
  extract(seq_along(x)) %>%
  split(x,.)
