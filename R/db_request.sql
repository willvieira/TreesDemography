/* install PostgreSQL (terminal) */
/* brew install postgresqlcd */
/* brew install  postgis */



/* Start server (terminal) */
brew services start postgresql
brew services stop postgresql



/* Create new user */
psql postgres
CREATE ROLE wvieira WITH LOGIN PASSWORD '';
ALTER ROLE wvieira CREATEDB;
\q



/* create database */
createdb -U wvieira forest

/* important db file into database forest */
psql forest < quicc_wo_clim.sql

/* list databases */
psql -U wvieira -l

/* Load database */
psql forest -U wvieira

/* check tables (La base de donnee principale c'est rdb_quicc */
\dt rdb_quicc.*

/* describe table */
\d+ rdb_quicc.plot



/* Export filter (relative path not allowed here) */

/* Export origin of each plot_id */
COPY (SELECT
   DISTINCT rdb_quicc.plot.plot_id,
   rdb_quicc.plot_info.org_db_loc
FROM
   rdb_quicc.plot
INNER JOIN rdb_quicc.plot_info ON rdb_quicc.plot .plot_id = rdb_quicc.plot_info .plot_id)
TO '/Users/wvieira/GitHub/TreesDemography/plot_info.csv' DELIMITER ',' CSV HEADER;

/* Export year_measured, dbh, age, sun_access, position_canopy of each tree_id */
COPY (SELECT
   rdb_quicc.tree.plot_id,
   rdb_quicc.tree.tree_id,
   rdb_quicc.tree.year_measured,
   rdb_quicc.tree.dbh,
   rdb_quicc.tree.age,
   rdb_quicc.tree.sun_access,
   rdb_quicc.tree.position_canopy
FROM
   rdb_quicc.tree)
TO '/Users/wvieira/GitHub/TreesDemography/tree_info.csv' DELIMITER ',' CSV HEADER;


/* drop (delete) database */
dropdb forest
