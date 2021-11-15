DBSaveIndividual <- function (individual=NULL, mysql.con=NULL, INDEX=NULL, curr.gen=NULL, rank=NULL, distance=NULL){

   temp <- paste(individual, collapse=",")
   qry <- paste("insert into population (individual, rank, dist, id, gen) values ('", temp, "', ", rank, ", ", "'", distance,"'",", ",INDEX,", ", curr.gen,")",sep='')
   RMySQL::dbSendQuery(mysql.con, qry)

}
