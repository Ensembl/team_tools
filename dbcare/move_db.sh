#!/bin/bash

if [[ ! -n $2 &&  ! -n $3 && -n $1 ]]
then
    echo "Usage: `basename $0` old_host new_host db_name "
    echo "move databases between otterpipe 1 & 2"
    exit 0
fi

old_host=$1
new_host=$2

# connexion variables
user='ottadmin'
pass='wibble'
db_name=$3

if [[ ${old_host} = 'otterpipe2' ]]; then
    db_old=" -uottroot -plutrasuper -h${old_host} -P3303 "
    db_new=" -uottroot -plutrasuper -h${new_host} -P3302 "
elif [[ ${old_host} = 'otterpipe1' ]]; then
    db_old=" -uottroot -plutrasuper -h${old_host} -P3302 "
    db_new=" -uottroot -plutrasuper -h${new_host} -P3303 "
else
    echo "Unknow hosts: $old_host and $new_host"
    exit
fi

# dump the database in a temporary file
/usr/local/ensembl/mysql/bin/mysqldump $db_old $db_name > $db_name.sql

# create the database on the new host
/usr/local/ensembl/mysql/bin/mysql $db_new -e "create database $db_name;"

# rename all tables from old database to new one
/usr/local/ensembl/mysql/bin/mysql $db_new $db_name < $db_name.sql;

# deletion of the old database
/usr/local/ensembl/mysql/bin/mysql $db_old -e "DROP DATABASE IF EXISTS $db_name;"

# delete the backup file
rm $db_name.sql;
