library(aws.s3)


# assign AWS bucket, folder and region 
bucket <- "jolts-data-public" # name of bucket where jolts file is hosted
region <- "us-east-2" # bucket assigned to us-east-2 region


# send new index.html file to AWS S3 bucket #### 
put_object(
  file = "docs/index.html",  # local file
  object = paste0(Sys.Date(), "_index.html"),  # name of file going to S3 bucket
  bucket = bucket,
  region = region
) 

