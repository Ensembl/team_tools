#!/bin/bash

echo "this script is obsolete, use db_schema_upgrade instead" >&2
exit 1

# path and connexion variables
script_path='/nfs/anacode/scripts/'
ensembl_path='/software/anacode/pipeline/ensembl58' # this should be pointing to the new Ensembl version directory
patch_path="${ensembl_path}/sql"

user=$1    # ottroot
pass=$2
host=$3    # e.g. otterpipe1
db_name=$4 # e.g. pipe, loutre or chromoDB 
version=$5 # e.g. 57_58


# set the attribute_type upload command
command="perl $ensembl_path/misc-scripts/attribute_types/upload_attributes.pl -nobackup -force_table_write \
-file $ensembl_path/misc-scripts/attribute_types/attrib_type.txt ";

log_file="/nfs/anacode/logs/db_upgrade/db_upgrade_${host}_${version}.log"

if [ -z $user -o -z $pass ]
then
    echo "Missing user or password: upgrade_dbs.sh <user> <pass> <host> <dbname> <version> (version like 51_52)"
    exit
fi

if [ -z $host -o -z $version -o -z $db_name ]
then
    echo "Missing host or version or database name: upgrade_dbs.sh  <user> <pass> <host> <dbname> <version> (version like 51_52)"
    exit
fi

if [ $host == 'otterpipe1' ]
then
    param="-host otterpipe1 -port 3302 -user $user -pass $pass -nobackup -dbnames";
    pipe="mysql -u$user -p$pass -hotterpipe1 -P3302";
elif [ $host == 'otterpipe2' ]
then
    param="-host otterpipe2 -port 3303 -user $user -pass $pass -nobackup -dbnames";
    pipe="mysql -u$user -p$pass -hotterpipe2 -P3303";
elif [ $host == 'otterlive' ]
then
    param="-host otterlive -port 3301 -user $user -pass $pass -nobackup -dbnames";
    pipe="mysql -u$user -p$pass -hotterlive -P3301";
else
    echo "Unknown host $host";
    exit
fi


for db in `echo "SHOW DATABASES LIKE '${db_name}%'" | $pipe -N`
do
    echo "database is $db"  >> $log_file
    for patch in ${patch_path}/patch_${version}*
    do
	basename $patch  >> $log_file
	cat $script_path/sql/engine.sql $patch | sed "s/MyISAM/InnoDB/g" | /usr/bin/time -a -f "%E real,%U user,%S sys" -o $log_file $pipe $db
    done;
    echo yes | ${command} ${param} $db;
done
