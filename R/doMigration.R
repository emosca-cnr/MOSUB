doMigration <- function (population=NULL, INDEX=-1, curr.gen=NULL, migrants.fraction=0.05, mysql.user=NULL, mysql.pass=NULL, mysql.db=NULL, mysql.host=NULL, mysql.table=NULL ){
    
    pop <- length(population$individuals)
    mysql.con <- RMySQL::dbConnect( RMySQL::MySQL(), user=mysql.user, password=mysql.pass, dbname=mysql.db, host=mysql.host)
   
    qry <- paste('delete from population where id = ', INDEX, sep='')
    rs <- RMySQL::dbSendQuery(mysql.con, qry)
   
    for(i in 1:pop)
        DBSaveIndividual(population$individuals[[i]], mysql.con, INDEX, curr.gen, population$rank[i], population$cd[i])
   
    N <- round(pop * migrants.fraction)

    qry <- paste("select distinct individual from ", mysql.table, " where id != ", INDEX," order by gen DESC, rank ASC, dist DESC limit ", N, sep="")
    rs <- RMySQL::dbSendQuery(mysql.con, qry)
    d <- RMySQL::fetch(rs, n = -1)
    size <- dim(d)
   
    RMySQL::dbDisconnect(mysql.con)

    if(size[1] > 0){
   
        new.individuals <- list()
        ## d: 1:limit x population,rank,distance,gen,index
        for(i in 1:size[1])
            new.individuals <- c(new.individuals, strsplit(d[i, 1], split=','))

        population$individuals[(pop-N+1) : (pop)] <- NULL
        population$individuals <- c(population$individuals, new.individuals)

    }
      
   return(population)
}

   
