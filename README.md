# **semoss**

*semoss* is a library for connecting to [SEMOSS](https://semoss.org/).

## Using this package you can:

 - Pull data from an existing insight using REST API.
 - Create Pandas DataFrame from responses generated by SEMOSS that have 'TASK_DATA' operation type.
 - Run pixel and get the json response.

## **Install**
```r
devtools::install_github("ttrankle/semoss")
```


## **Usage**

To interract with a semoss instance, laod in the library and establish a session:


### Setup
```r
>>> library("semoss")

# connect to an instance of SEMOSS by passing in the base url for api
# This will prompt a username & password login. Alternatively, you can set 'SEMOSS_USERNAME' and 'SEMOSS_PASSWORD' in environement variables or pass the argumenets into the function 
# establishSession('http://localhost:8080/Monolith', username = 'myUsername', password = 'password123')
>>> establishSession('http://localhost:8080/Monolith')
TRUE
```

### Using REST API to pull data from an Insight and create a data.frame in your R env
```r
# define the Project ID
>>> projectId = '30991037-1e73-49f5-99d3-f28210e6b95c'

# define the Insight ID
>>> inishgtId = '26b373b3-cd52-452c-a987-0adb8817bf73'

# define the SQL for the SEMOSS frame you want to query within the insight
>>> sql = 'select * FROM SEMOSS_FRAME123'

# if you dont provide one of the following, it will ask you to provide it via prompt
>>> diabetes_df = importFrameFromSemossInsight(project_id = projectId, insight_id = inishgtId, sql = sql)
>>> head(diabetes_df)
```
|    |   AGE |   PATIENT |   WEIGHT |
|---:|------:|----------:|---------:|
|  0 |    19 |      4823 |      119 |
|  1 |    19 |     17790 |      135 |
|  2 |    20 |      1041 |      159 |
|  3 |    20 |      2763 |      274 |
|  4 |    20 |      3750 |      161 |

### Create a data.frame from Task Data

```r
# define a pixel command
>>> pixel = 'Database ( database = [ "995cf169-6b44-4a42-b75c-af12f9f45c36" ] ) | Select ( DIABETES__AGE , DIABETES__PATIENT , DIABETES__WEIGHT ) .as ( [ AGE , PATIENT , WEIGHT ] ) | Distinct ( false ) | Collect(-1)'

# use getDataFrameFromTaskData method with the pixel command to create a Pandas DataFrame
>>> df = getDataFrameFromTaskData(pixel)
>>> head(df)
```
|    |   AGE |   PATIENT |   WEIGHT |
|---:|------:|----------:|---------:|
|  0 |    19 |      4823 |      119 |
|  1 |    19 |     17790 |      135 |
|  2 |    20 |      1041 |      159 |
|  3 |    20 |      2763 |      274 |
|  4 |    20 |      3750 |      161 |

### Get the JSON response of any pixel
```r
# run the pixel
>>> runPixel('1+1')
$insightID
[1] "e840440f-e4c3-4a90-a6bb-cf79109d1b41"

$sessionTimeRemaining
[1] "7200"

$pixelReturn
  pixelId pixelExpression isMeta output operationType
1       0         1 + 1 ;  FALSE      2     OPERATION
```

---
## Dev
Please email SEMOSS