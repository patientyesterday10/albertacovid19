source("R/lib/packages.R")

load_ab_covid_data <- function(){
  dt <- data.table(read.csv("data/covid19dataexport.csv"))
  dt[, Date.reported := as.Date(Date.reported, format = "%Y-%m-%d")]

  manual_updates <- data.table(
    Date.reported=as.Date(c("2021-04-01","2021-04-02","2021-04-03","2021-04-20","2021-04-21")),
    N=c(1100,1100,950,1765,1857),
    Alberta.Health.Services.Zone=c("Unknown","Unknown","Unknown","Unknown","Unknown")
  )

  manual_adds <- rbindlist(
    apply(manual_updates,MARGIN = 1, FUN = function(x){
      if(nrow(dt[Date.reported==as.Date(x['Date.reported']),])==0){
        tmp <- data.table(
          Date.reported=as.Date(x['Date.reported']),
          Alberta.Health.Services.Zone=as.character(x["Alberta.Health.Services.Zone"]),
          Gender="Unknown",
          Age.group = "Unknown",
          Case.status = "Active",
          Case.type = "Comfirmed",
          I=seq(1,x['N'],by=1)
        )
        tmp$I<-NULL
        return(tmp)
      }
      return(NULL)
    }))

  dt <- rbindlist(list(dt,manual_adds))


  return(dt)
}
