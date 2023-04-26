#' @export
establishSession <- function(baseUrl = '', username='', password=''){
  if (baseUrl == ""){
    stop("Please provide the base url. E.g. http://localhost:9090/Monolith_Dev")
  } else{
    apiUrl <<- baseUrl
  }
  while(username ==''){
    # check if its in the sys env
    username <- Sys.getenv("SEMOSS_USERNAME")
    if (username == ""){
      username <- readline('Username:')
    }
  }
  
  while(password ==''){
    # check if its in the sys env
    password <- Sys.getenv("SEMOSS_PASSWORD")
    if (password ==''){
      password <- getPass::getPass("Enter the password: ")
    }
  }
  
  r <- httr::POST(paste0(apiUrl,"/api/auth/login",sep=''), 
                  query = list(username = "ttrankle", password = "911Rusty*")
  )
  
  if(r$status_code == 200){
    insightId <<- ''
    return(TRUE)
  }
  else {
    return(r$status_code)
  }
}

#' @export
importFrameFromSemossInsight <- function(project_id = '', insight_id = '', sql = ''){
  if (project_id == ''){
    project_id <- readline('Please enter the Project ID: ')
  }
  if (insight_id == ''){
    insight_id <- readline('Please enter the Insight ID: ')
  }
  
  base_url <- paste0(apiUrl,
                     '/api/project-',
                     project_id,
                     '/jdbc_json?insightId=',
                     insight_id,
                     '&open=true&sql=',
                     sep='')
  
  if (sql == ''){
    sql <- readline('Please enter the SQL: ')
  }
  
  fullUrl <- URLencode(paste0(base_url,sql,sep=''))
  response <- httr::GET(fullUrl)
  jsonResponse <- jsonlite::fromJSON(httr::content(response,as="text",encoding = "UTF-8"))
  
  if ('data' %in% names(jsonResponse)){
    responseDf <- data.frame(jsonResponse$data)
    colnames(responseDf) <- jsonResponse$columns
    return(responseDf)
  } else {
    responseDf <- data.frame(jsonResponse$dataArray)
    colnames(responseDf) <- jsonResponse$columns
    return(responseDf)
  }
}

#' @export
runPixel <- function(expression){
  if (insightId == ''){
    response <- httr::POST(url = paste0(apiUrl,'/api/engine/runPixel',sep=''), config = httr::content_type('application/x-www-form-urlencoded;charset=utf-8'), query = list(expression = "META|true", insightId = "new"))
    insightId <<- jsonlite::fromJSON(httr::content(response,as="text",encoding = "UTF-8"))$insightID
  }
  
  data <- list(expression = expression, insightId = URLencode(insightId))
  
  response <- httr::POST(url = paste0(apiUrl,'/api/engine/runPixel',sep=''), config = httr::content_type('application/x-www-form-urlencoded;charset=utf-8'), query = data)
  return(jsonlite::fromJSON(httr::content(response,as="text",encoding = "UTF-8")))
}

#' @export
getDataFrameFromTaskData <- function(expression){
  response <- runPixel(expression)
  if (response$pixelReturn$operationType[[1]] == 'TASK_DATA'){
    dfRetrun <- data.frame(response$pixelReturn$output$data$values[[1]])
    colnames(dfRetrun) <- response$pixelReturn$output$data$headers[[1]]
    return(dfRetrun)
  } else{
    return(NULL)
  }
}