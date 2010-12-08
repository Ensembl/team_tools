#!/bin/bash
# This is an utility script to rename a database hosted on one of the otter servers.

user=$1
pass=$2
host=$3
old_db=$4
new_db=$5

if [ -z $user -o -z $pass -o -z $host ] ; then
    echo "Missing user or password or host: rename_db.sh <user> <pass> <host> <old_name> <new_name>"
    exit 1
fi

if [ -z $old_name -o -z $new_name ] ; then
    echo "Missing new or old database names: rename_db.sh <user> <pass> <host> <old_name> <new_name>"
    exit 1
fi

# connexion variables
user='ottadmin'
pass='lutralutra'
host=$1

if [ $host = 'otterpipe2' ]; then
    db_connection="mysql -f -u$user -p$pass -h${host} -P3303 "
elif [ $host = 'otterpipe1' ]; then
    db_connection="mysql -f -u$user -p$pass -h${host} -P3302 "
elif [ $host = 'otterlive' ]; then
    db_connection="mysql -f -u$user -p$pass -h${host} -P3301 "
else
    echo "Unknow host: $host"
    exit 1
fi

# creation of the new database
$db_connection -e "CREATE DATABASE IF NOT EXISTS $new_db;"

# rename all tables from old database to new one
for table in `$db_connection -e "SHOW TABLES FROM $old_db;"`
do
    if [ $table !=  "Tables_in_${old_db}" ]; then
        echo "$new_db.$table"
        $db_connection -e "RENAME TABLE ${old_db}.$table TO ${new_db}.$table ;"
    fi
done

# deletion of the old database
$db_connection -e "DROP DATABASE IF EXISTS $old_db;"
