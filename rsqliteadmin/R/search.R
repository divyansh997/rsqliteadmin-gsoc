#' Search Function
#'
#' The function takes multiple search parameters, creates a query and filters out results on the basis of that query.
#' @param conn connection to SQLite database
#' @param distinct adds "DISTINCT" in the sql query if true
#' @param display_columns columns to be selected
#' @param table_names tables from which the columns should be selected
#' @param search_parameters column names on the basis of which to filter out results
#' @param search_parameters_values values for the respective search parameters
#' @param search_parameters_operators supports >, <, ==, >=, <=
#' @param ordering adds ORDER BY statement to query to sort the results
#' @param ordering_parameters columns by which to sort
#' @param check_null=NULL adds a NOT NULL or NULL statement to the query
#' @param check_null_parameters columns which should be tested for IS NULL or IS NOT NULL
#' @keywords search
#' @export
#' @examples
#' search()

search <- function(conn = NULL,
                   distinct = FALSE,
                   display_columns = NULL,
                   table_names = NULL,
                   search_parameters = NULL,
                   search_parameters_values = NULL,
                   search_parameters_operators = NULL,
                   ordering = NULL, #asc, dec
                   ordering_parameters = NULL,
                   check_null=NULL, #not null
                   check_null_parameters = NULL
){

  select_query<-""
  if(isTRUE(distinct))
    select_query<-"SELECT DISTINCT "
  else
    select_query<-"SELECT "

  from_query<-"FROM "
  where_query<-"WHERE "
  ordering_query<-"ORDER BY "
  result_query<-""

  for(i in 1:length(display_columns)){
    if(i!=length(display_columns))
      select_query<- paste(select_query, display_columns[i], ", ", sep = "")
    else
      select_query<- paste(select_query, display_columns[i], " ", sep = "")
  }

  for(i in 1:length(table_names)){
    if(i!=length(table_names))
      from_query<- paste(from_query, table_names[i], ", ", sep = "")
    else
      from_query<- paste(from_query, table_names[i], " ", sep = "")
  }


  for(i in 1:length(search_parameters)){
    if(i!=length(search_parameters))
      where_query<- paste(where_query, search_parameters[i], " ",
                          search_parameters_operators[i], " ",
                          search_parameters_values[i], " AND ", sep = "")
    else
      where_query<- paste(where_query, search_parameters[i], " ",
                          search_parameters_operators[i], " ",
                          search_parameters_values[i], sep = "")
  }

  if(length(search_parameters!=0)&&length(check_null)!=0)
    where_query<- paste(where_query, "AND ")

  for(i in 1:length(check_null)){
    if(i!=length(check_null)){
      if(check_null[i] == TRUE)
        where_query<- paste(where_query, check_null_parameters[i], " IS NULL AND ", sep = "")
      else
        where_query<- paste(where_query, check_null_parameters[i], " IS NOT NULL AND ", sep = "")
    }
    else{
      if(check_null[i] == TRUE)
        where_query<- paste(where_query, check_null_parameters[i], " IS NULL ", sep = "")
      else
        where_query<- paste(where_query, check_null_parameters[i], " IS NOT NULL ", sep = "")
    }
  }

  for(i in 1:length(ordering)){
    if(i!=length(ordering))
      ordering_query<- paste(ordering_query, ordering_parameters[i],
                             " ", ordering[i], ", ", sep = "")
    else
      ordering_query<- paste(ordering_query, ordering_parameters[i],
                             " ", ordering[i], sep = "")
  }

  result_query<- paste(select_query, from_query, where_query, ordering_query, sep = "")
  print(result_query)
  result <- dbGetQuery(conn, result_query)
  return(result)
}
