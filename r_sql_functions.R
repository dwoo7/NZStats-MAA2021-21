# R wrapper to execute the queries on SQL server.

# Check if required packages exist
packages = data.frame(package=c('DBI')
                      ,pkgversion=c('1.1.0')
)
packages.exist <- apply(packages, 1
                        ,FUN = function(x){
                          if(!(x[1] %in% installed.packages()[,"Package"]))
                            warning(paste0("Package ", x[1], " not installed. A minimum version of ", x[2], " must be installed."))
                          else {
                            if(packageVersion(x[1]) < x[2])
                              warning(paste0("Package ", x[1], " is outdated. A minimum version of ", x[2], " must be installed."))
                          }
                        })

require(DBI)


SQL_DRIVER <- "ODBC Driver 18 for SQL Server"
SQL_SERVER <- "PRTPRDSQL36.stats.govt.nz,1433"

#' Creates a connection to the database using DBI library.
#'
#' @description 
#' This function is used to connect to the specified database using the DBI library.
#' 
#' @param database The database to which a connection should be created.
#' 
#' @return Returns a DBI connection object
create_db_conn <- function(database){
  conn <- DBI::dbConnect(odbc::odbc(),
                         Driver = SQL_DRIVER,
                         Server = SQL_SERVER,
                         Database = database,
                         TrustServerCertificate="Yes",
                         Trusted_Connection = "Yes",
                         ConnectionPooling=FALSE)
  return(conn)
}

#' Closes a connection to the database.
#'
#' @description 
#' This function is used to close a specified connection objectcreated using DBI library.
#' 
#' @param database The database to which a connection should be created.
#' 
close_db_conn <- function(conn){
  dbDisconnect(conn)
}

#' Execute a list of queries
#'
#' @description 
#' This function is used to run SQL queries in one transation batch - and the database connection & 
#' list of these queries are passed on as a vector of strings as input.
#' 
#' @param conn The database to which a connection should be created.
#' @param querylist List of strings that contain the queries to be executed.
#' 
execute_queries <- function(conn, querylist){
  dbWithTransaction(conn, 
                    for (query in querylist){
                      tryCatch(dbExecute(conn, query, immediate=TRUE)
                               , error = function(cond) {
                                 warning(paste0("Error: ", cond))
                                 return(NA)
                               }
                      )
                    }
  )
}


#' Read in text from a file
#'
#' @description 
#' This function is used to read in text from a file specified by a filepath
#' and return the content as a long, continuous string.
#' 
#' @param full_filepath The database to which a connection should be created.
#' 
read_content_from_file <- function(full_filepath){
  if (!is.na(full_filepath) && file.exists(full_filepath) ){
    # Read in the file.
    content <- paste(readLines(full_filepath), collapse = "\n")
    
  }
  else stop(paste0("File ", full_filepath
                   ," not found. Check if the file exists at the specified location."))
  return(content)
}

#' Replaces a substring with another substring in a supplied string
#'
#' @description 
#' This function is used to replace the supplied parameter with its value in the
#' content of an SQL file.
#' 
#' @param content The content of the SQL file in hwich the parameter is to be replaced with a value.
#' @param param_name The placeholder string of the parameter to be replaced
#' @param value The value that the placeholder should be replaced with.
#' 
#' @return content with the replaced paraemter value.
#' 
replace_parameter_with_value <- function(content, param_name, value){
  content <- gsub(param_name, replacement=value, x=content, perl=TRUE, ignore.case = TRUE)
  return(content)
}

#' Use the parameter-value list to create executable SQL content
#'
#' @description 
#' This function is used to replace the supplied parameters with the values in the supplied
#' content of an SQL file.
#' 
#' @param content The content of the SQL file in hwich the parameter is to be replaced with a value.
#' @param parameters A named list- the names should be the parameter names, and values should be
#' parameter values
#' 
#' @return content of the SQL file where the parameters have been replaced with actual values
#' 
apply_parameters <- function(content, parameters){
  for (param in names(parameters)){
    content <- replace_parameter_with_value(content, paste0("{", param, "}"), parameters[[param]])
  }
  return(content)
}

#' Execute a "template" SQL file on the database after applying the supplied parameter values.
#'
#' @description 
#' This function is used to execute a "template" SQL file on the database. The parameter list is used
#' to replace the placeholders in the SQL file with actual values.
#' 
#' @param sql_library The path to the Modular Spells library
#' @param sql_file A name of the SQL file in the library
#' @param parameters A named list- the names should be the parameter names, and values should be
#' parameter values
#' @param database The database to which a connection needs to be establised to execute the SQL.
#' 
#' @return content of the SQL file where the parameters have been replaced with actual values
#' 
execute_queryfile_on_db <- function(sql_library, sql_file, parameters, database){
  parameters <- validate_config_file(as.data.frame(parameters), expected_names = c("targetschema", "projprefix", "indicatorsql",
                                                                    "populationdata", "idcolumn", "startdatecolumn", 
                                                                    "enddatecolumn") 
                                     )
  content <- read_content_from_file(file.path(sql_library, sql_file))
  content <- apply_parameters(content, parameters)
  queries <- parse_sqlqueries_from_string(content)
  conn <- create_db_conn(database)
  execute_queries(conn, queries)
  close_db_conn(conn)
}

#' Execute a "template" SQL file on the database after applying the supplied parameter values.
#'
#' @description 
#' This function is used to execute a "template" SQL file on the database. The parameter list is used
#' to replace the placeholders in the SQL file with actual values.
#' 
#' @param sql_library The path to the Modular Spells library
#' @param sql_file A name of the SQL file in the library
#' @param parameters A named list- the names should be the parameter names, and values should be
#' parameter values
#' @param database The database to which a connection needs to be establised to execute the SQL.
#' 
#' @return content of the SQL file where the parameters have been replaced with actual values
#' 
run_sqlfile_on_db <- function(sql_file, sql_library, database){
  print(paste0("Executing ", file.path(sql_library, sql_file), " on the database..."))
  content <- read_content_from_file(file.path(sql_library, sql_file))
  queries <- parse_sqlqueries_from_string(content)
  conn <- create_db_conn(database)
  execute_queries(conn, queries)
  close_db_conn(conn)
}

#' Create spell datasets
#'
#' @description 
#' This function is used to create customised copies of the indicator SQL files for a specific
#' project or population. It can also be used to create IDI_Clean specific versions of the indicator.
#' 
#' The indicator templates have placeholders in SQL script for IDI refresh version, project schema,
#' population tables, etc. This script uses the supplied configuration file to look for and replace
#' those placeholders with the actual values that the user wants to use for their project.
#' 
#' @param proj_config_file The configuration file for the project.
#' @param src_templates_folder
#' @param tgt_folder
#' 
create_spelltables_for_project <- function(proj_config_file 
                                           ,src_templates_folder
                                           ,tgt_folder
                                           ,create_in_database=FALSE
){
  # Read in the config file
  if (file.exists(proj_config_file))
    config_params <- readxl::read_excel(path=proj_config_file)
  else
    stop(paste0("File ", proj_config_file, " not found. Check if the file exists at the specified location."))
  
  # Verify all expected columns exist & other validation rules.
  config_params <- validate_config_file(config_params, expected_names = c("targetschema", "projprefix", "indicatorsql",
                                                                            "populationdata", "idcolumn", "startdatecolumn", 
                                                                            "enddatecolumn") )
  
  # For each row in the config file, create the project-specific version of the indicator file
  apply(config_params, 1, customise_indicator, src_templates_folder=src_templates_folder, tgt_folder=tgt_folder
        ,maintenance_flag=FALSE)
  
  #' @TODO: Execute these indicators in the DB if user requests it.
  if (create_in_database)
    apply(config_params["indicatorsql"], 1, run_sqlfile_on_db, sql_library=tgt_folder, database="IDI_UserCode" )
  
}

customise_indicator <- function(config, src_templates_folder, tgt_folder, maintenance_flag){
  targetschema <- config["targetschema"]
  projprefix <- config["projprefix"]
  indicatorsql <- config["indicatorsql"]
  populationdata <- config["populationdata"]
  idcolumn <- config["idcolumn"]
  startdatecolumn	<- config["startdatecolumn"]
  enddatecolumn <- config["enddatecolumn"]
  
  # Check if the indicator file exists
  if (!is.na(indicatorsql) && file.exists(file.path(src_templates_folder, indicatorsql)) ){
    #read in the file.
    sql <- paste(readLines(file.path(src_templates_folder, indicatorsql)), collapse = "\n")
    
  }
  else stop(paste0("File ", file.path(src_templates_folder, indicatorsql)
                   ," not found. Check if the file exists at the specified location."))
  
  # Check if target folder exists - else create it.
  if (!dir.exists(tgt_folder)){
    sprintf(paste0("Creating target directory ", tgt_folder))
    dir.create(tgt_folder, recursive=TRUE)
  }
  
  # Check if population file is supplied - if not, remove the join from the SQLs using regex. This does not happen in "maintenance" mode. 
  if (is.na(populationdata)){
    sql <- gsub(pattern="(?s)(?:inner join {populationdata})(.+?)(?=left|right|inner|join|where|group by|\\)select|\\)[\\n]*select|;|\\/\\*|--)"
                ,replacement= ""
                ,sql
                ,perl=TRUE
                ,ignore.case = TRUE)
  }
  else
    sql <- gsub("{populationdata}", replacement=populationdata, x=sql, perl=TRUE, ignore.case = TRUE)
  
  # Now, check if date-based join columns are supplied. If not, remove these joins from the SQLs using regex. This does not
  # happen in "maintenance mode".
  if ((is.na(startdatecolumn) && is.na(enddatecolumn) )){
    # remove date joins
    sql <- gsub(pattern="(?s)(?<=[\\=\\s*]pop.{idcolumn})(.+?)((?:[^\\}]*\\}){2})"
                ,replacement= ""
                ,sql
                ,perl=TRUE
                ,ignore.case = TRUE)
  }
  
  if (!is.na(idcolumn))
    sql <- gsub("{idcolumn}", replacement=idcolumn, x=sql, perl=TRUE, ignore.case = TRUE)
  if (!is.na(startdatecolumn))
    sql <- gsub("{startdatecolumn}", replacement=startdatecolumn, x=sql, perl=TRUE, ignore.case = TRUE)
  if (!is.na(enddatecolumn))
    sql <- gsub("{enddatecolumn}", replacement=enddatecolumn, x=sql, perl=TRUE, ignore.case = TRUE)
  if (!is.na(targetschema))
    sql <- gsub("{targetschema}", replacement=targetschema, x=sql, perl=TRUE, ignore.case = TRUE)
  if (!is.na(projprefix))
    sql <- gsub("{projprefix}", replacement=projprefix, x=sql, perl=TRUE, ignore.case = TRUE)
  
  
  #' @TODO: Check if IDI clean version & target DB exists
  
  # Write the new SQL file to target folder
  fileconn <- file(file.path(tgt_folder, indicatorsql), encoding = "UTF-8")
  writeLines(sql, fileconn)
  close(fileconn)
}

#' Clean up database objects
#'
#' @description 
#' This function is used to delete database objects that have been created on the database as part of a
#' project, by using the configuration file (user_config.xslx) and the outputs file for a particular
#' run of the IDI-specific modular codebase.
#' 
#' @param proj_config_file The configuration file for the project.
#' @param outputs_file The file that stores the list of output tables created from the modular codebase.
#' 
cleanup_objects <- function(proj_config_file, outputs_file){
  # Read in the config file
  config_params <- read_config(proj_config_file)
  
  # Verify all expected columns exist & other validation rules.
  config_params <- validate_config_file(config_params, expected_names = c("targetschema", "projprefix", "indicatorsql",
                                                                          "populationdata", "idcolumn", "startdatecolumn", 
                                                                          "enddatecolumn") )
  # Read in the outputs file if it exists
  if (file.exists(outputs_file))
    outputs <- read.csv(outputs_file)
  else
    stop(paste0("File ", outputs_file, " not found. Check if the file exists at the specified location."))
  
  outputs$file <- paste0(outputs$file, ".sql")
  
  tables <- inner_join(config_params, outputs, by=c("indicatorsql"="file")) %>%
    mutate(database = str_extract(output, "[^.]+")
           ,targetschema = gsub("\\[|\\]", "", targetschema)
           ,table = sub(".*\\.", "", str_replace(output, "\\{projprefix\\}", projprefix))
    ) %>% 
    select(indicatorsql, database, targetschema, table)
  
  apply(tables, 1, drop_table_if_exists)
  
}

#' Drop database objects
#'
#' @description 
#' This function is used to delete database objects if these exist.
#' 
#' @param config_row The connection to the database where the object exists
#' 
drop_table_if_exists <- function(config_row){
  database <- config_row["database"]
  schema <- config_row["targetschema"]
  table <- config_row["table"]
  tempcon <- create_db_conn(database)
  query <- paste0("drop ", check_dbobject_type(tempcon, schema, table), " if exists [", schema, "].", table)
  print(query)
  execute_queries(tempcon, c(query))
  close_db_conn(tempcon)
}

#' Find the database object type
#'
#' @description 
#' This function is used to find the database object type of a database, given its database name, schema and object name
#' 
#' @param conn The connection to the database where the object exists
#' @param schema The schema where the object exists
#' @param table The name of the database object
#' 
check_dbobject_type <- function(conn,  schema, table){
  query <- paste0("select type_desc from sys.all_objects o inner join sys.schemas s on (o.schema_id = s.schema_id) where s.name = '"
                  ,gsub("\\[|\\]", "", schema)
                  ,"' and o.name = '"
                  ,table
                  ,"'")
  
  res <- dbSendQuery(conn, query)
  objtype <- dbFetch(res)
  dbClearResult(res)
  if (grepl("TABLE", objtype$type_desc[1])) 
    { return("TABLE")}
  return(objtype$type_desc[1])
}

#' @TODO: Calculate dependent SQL files automatically and execute all dependencies on the server.

parse_sqlqueries_from_string <- function(content){
  strbuffer <- ""
  cmntblock <- 0
  charposition <- 0
  singlequote_ct <- 0
  valid_querystop <- c()
  recursive_comment <- c()
  
  for (char in strsplit(content, "")[[1]]){
    charposition <- charposition + 1
    strbuffer <- paste0(stringr::str_sub(strbuffer, -1, -1), char)
    
    if (char == "'" && cmntblock == 0)
      singlequote_ct <- singlequote_ct + 1
    
    if (strbuffer == "/*" && singlequote_ct%%2 == 0){
      cmntblock <- cmntblock + 1
      if (cmntblock > 1)
        recursive_comment <- c(recursive_comment, charposition-1 )
    }
    
    
    if (strbuffer == "*/" && singlequote_ct%%2 == 0){
      if (cmntblock > 1)
        recursive_comment <- c(recursive_comment, charposition )
      cmntblock <- cmntblock - 1
      
    }
    
    
    if (cmntblock == 0 && singlequote_ct%%2 == 0 && char == ";")
      valid_querystop <- c(valid_querystop, charposition-1)
  }
  
  
  #'TODO: A temporary section to address recursive comments - need a better solution
  for (pos in recursive_comment){
    substr(content, pos, pos) <- "*"
  }
  
  prevpos <- 0
  querylist <- c()
  for (pos in valid_querystop){
    querylist <- c(querylist, stringr::str_sub(content, prevpos, pos))
    prevpos <- pos+2
  }
  return (querylist)
}
# Example Code
# parameters <- list("targetdb" = "IDI_UserCode"
#                    ,"targetschema"= "DL-MAA2020-47"
#                    ,"projprefix" = "tst"
#                    ,"populationdata" = "[IDI_Sandpit].[DL-MAA2020-47].tst_population_xlarge"
#                    ,"idcolumn" = "snz_uid"
#                    ,"startdatecolumn" = "from_date"
#                    ,"enddatecolumn" = "to_date"
#                    ,"idicleanversion"="IDI_Clean_20201020"
#                    ,"indicatorsql"="tst"
#                    )
# sql_library <- "/nas/DataLab/MAA/MAA2020-47/4. Code development/idi_indicator_repo/idi_clean_20201020/sql"
# sql_file <- "census_quals.sql"
# database <- "IDI_userCode"
# 
# execute_queryfile_on_db(sql_library, sql_file, parameters, database)


 