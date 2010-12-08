#!/usr/local/bin/bash

if [ -z "$1" ]
then
    echo "Need host"
    echo "Usage: `basename $0` host organisme loutre_name"
    exit 0
fi
if [ -z "$2" ]
then
    echo "Need organisme"
    echo "Usage: `basename $0` host organisme loutre_name"
    exit 0
fi

if [ -z "$3" ]
then
    echo "Need loutre name"
    echo "Usage: `basename $0` host organisme loutre_name"
    exit 0
fi

# path variables
home='/nfs/team71/analysis/ml6/work/'
new_loutre_sql=$home'CVS/ensembl-otter_HEAD/sql/otter_new_schema.sql'
production='/software/anacode/pipeline/'
#production='/nfs/team71/analysis/ml6/work/pipe_dev/'
script=$home'script/'
ensembl=$production'ensembl/'
pipe=$production'ensembl-pipeline/'
ana=$production'ensembl-analysis/'

# connexion variables
user='ottroot'
pass='xxx'
host=$1
org=$2
database_name=$3

echo "---> Creation of loutre database $database_name for $org"

if [ $host == 'otterpipe2' ]; then
    port=3303
elif [ $host == 'otterpipe1' ]; then
    port=3302
elif [ $host == 'otterlive' ]; then
    port=3301
else
    echo "Unknow host: $host"
    exit
fi

options=" -f -u${user} -p${pass} -h${host} -P${port} "
db_connection="mysql ${options}"


# I. Database initialisation
#1) database creation or deletion
$db_connection -e "DROP DATABASE IF EXISTS $database_name;"
$db_connection -e "CREATE DATABASE $database_name;"
echo "---> Database $database_name created."

#2) Tables initialisation

cat $script'sql/engine.sql' $ensembl'sql/table.sql' | sed "s/MyISAM/InnoDB/g" | $db_connection $database_name
cat $script'sql/engine.sql' $new_loutre_sql | sed "s/MyISAM/InnoDB/g" | $db_connection $database_name

cat $script'sql/engine.sql' $script'sql/loutre/analysis.sql' | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/loutre/coord_system.sql' | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/loutre/meta.sql' | $db_connection $database_name
mysqlimport -L -c meta_key,meta_value $options $database_name ${script}taxonomy/meta.${org}
perl $ensembl'misc-scripts/attribute_types/upload_attributes.pl' \
    -host $host -port $port -user $user -pass $pass -dbnames $database_name \
    -file $ensembl'misc-scripts/attribute_types/attrib_type.txt'
cat $script'sql/engine.sql' $script'sql/loutre/author.sql' | $db_connection $database_name

echo '---> loutre Schema version is '
$db_connection $database_name -e "SELECT meta_value FROM meta WHERE meta_key = 'schema_version';"

echo "---> Tables for $database_name loaded."
