# creating the unique file name

fileName <- sprintf(
  "%s_%s.rds", 
  as.integer(Sys.time()), 
  digest::digest(data)
)