#' Insert Function
#'
#' The function inserts values into a table.
#' @param conn connection to SQLite database
#' @param insert_table table name into which values are to be inserted
#' @param insert_vector column names(vector) into which values should be inserted
#' @param insert_values values(vector) which are to be inserted
#' @keywords insert
#' @export
#' @examples
#' insert()

insert <- function(conn = NULL,
                   insert_table = NULL,
                   insert_vector = NULL,
                   insert_values = NULL
){
  insert_query<- "INSERT INTO "
  value_query<- "VALUES "
  result_query<- ""
  insert_query<- paste(insert_query, insert_table[1], " ", sep = "")
  if(!is.null(insert_vector)){
    insert_query<- paste(insert_query, "(", sep = "")
    for(i in 1:length(insert_vector)){
      if(i!=length(insert_vector))
        insert_query<- paste(insert_query, insert_vector[i], ", ", sep = "")
      else
        insert_query<- paste(insert_query, insert_vector[i], ") ", sep = "")
    }
  }

  value_query<- paste(value_query, "(", sep = "")
  for(i in 1:length(insert_values)){
    if(i!=length(insert_values))
      value_query<- paste(value_query, "'", insert_values[i], "'", ", ", sep = "")
    else
      value_query<- paste(value_query, "'", insert_values[i], "'", ");", sep = "")
  }
  result_query<- paste(insert_query, value_query, sep = "")
  print(result_query)
  dbExecute(conn, result_query)
}
