#!/usr/local/bin/bash

if [ -z "$1" ]
then
  echo "Need host"
  echo "Usage: `basename $0` host organisme pipeline_name"
  exit 0
fi
if [ -z "$2" ]
then
  echo "Need organism"
  echo "Usage: `basename $0` host organisme pipeline_name"
  exit 0
fi

if [ -z "$3" ]
then
  echo "Need pipeline name"
  echo "Usage: `basename $0` host organisme pipeline_name"
  exit 0
fi

# path variables
home='/nfs/team71/analysis/ml6/work'
#production='/software/anacode/pipeline/'
production='/nfs/team71/analysis/ml6/work/pipe_dev/'
script=$home'/script/'
ensembl=$production'ensembl/'
pipe=$production'ensembl-pipeline/'
ana=$production'ensembl-analysis/'
conf=$home'/CVS/personal/ml6/analysis_rules/'
# connexion variables
user='ottroot'
pass='xxx'
host=$1
org=$2
database_name=$3


echo "---> Creation of the pipeline database $database_name for $org"

if [ $host == 'otterpipe2' ]; then
	port=3303
elif [ $host == 'otterpipe1' ]; then
	port=3302
else
	echo "Unknow host: $host"
	exit
fi


options="-u${user} -p${pass} -h${host} -P${port} "
db_connection="mysql ${options}"


# I. Database initialisation
#1) database creation or deletion
$db_connection -e "DROP DATABASE IF EXISTS $database_name;"
$db_connection -e "CREATE DATABASE $database_name;"
echo "---> Database $database_name created."

#2) Tables initialisation

cat $script'sql/engine.sql' $ensembl'sql/table.sql' | sed "s/MyISAM/InnoDB/g" | $db_connection $database_name
cat $script'sql/engine.sql' $pipe'sql/table.sql' | sed "s/MyISAM/InnoDB/g" | $db_connection $database_name
cat $script'sql/engine.sql' $pipe'sql/hit_description.sql' | sed "s/MyISAM/InnoDB/g" | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/input_id_seq_region.sql' | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/create_history_tables.sql' | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/coord_system.sql' | $db_connection $database_name
cat $script'sql/engine.sql' $script'sql/meta.sql' | $db_connection $database_name

mysqlimport -L -c meta_key,meta_value $options $database_name ${script}taxonomy/meta.${org}
perl $ensembl'misc-scripts/attribute_types/upload_attributes.pl' \
-host $host -port $port -user $user -pass $pass -dbnames $database_name -nobackup \
-file $ensembl'misc-scripts/attribute_types/attrib_type.txt'

echo '---> Pipeline Schema version is '
$db_connection $database_name -e "SELECT meta_value FROM meta WHERE meta_key = 'schema_version';"


#Update the input_id_analysis table to store in the result column the analysis runtime determined in seconds
$db_connection $database_name -e "ALTER TABLE input_id_analysis CHANGE COLUMN result result INT NOT NULL DEFAULT 0;"

echo "---> Tables for $database_name loaded."


#3) Filling in the analysis and rules tables
perl ${pipe}scripts/analysis_setup.pl -dbhost $host -dbport $port -dbuser $user -dbpass $pass -dbname $database_name -read -file ${conf}${org}_exonerate.analysis
perl ${pipe}scripts/rule_setup.pl -dbhost $host -dbport $port -dbuser $user -dbpass $pass -dbname $database_name -read -file ${conf}${org}_exonerate.rules

echo "---> Tables Analysis and Rules initialised."


