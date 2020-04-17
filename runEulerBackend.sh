#!/bin/bash

# defaults
MODE=NORMAL # could be REC, PLAYER, BULK

for i in "$@"
do
case $i in
    -m=REC|-m=PLAYER|-m=BULK|--mode=REC|--mode=PLAYER|--mode=BULK)
    MODE="${i#*=}"
    shift # past argument=value
    ;;
#    --default)
#    DEFAULT=YES
#    shift # past argument with no value
#    ;;
    *)
          # unknown option
    ;;
esac
done
echo "MODE            = ${MODE}"



#echo "SEARCH PATH     = ${SEARCHPATH}"
#echo "LIBRARY PATH    = ${LIBPATH}"
#echo "DEFAULT         = ${DEFAULT}"
#echo "Number files in SEARCH PATH with EXTENSION:" $(ls -1 "${SEARCHPATH}"/*."${EXTENSION}" | wc -l)
#if [[ -n $1 ]]; then
#    echo "Last line of file specified as non-opt/last argument:"
#    tail -1 $1
#fi

# RTS options
REAL_CORES=$(lscpu --all --parse=CORE,SOCKET | grep -Ev "^#" | sort -u | wc -l)
echo "Real cores      = $REAL_CORES"

RTS_OPTIONS="-A64M -AL256M -I0 -qb0 -qn$REAL_CORES -N$REAL_CORES"
echo "RTS options = $RTS_OPTIONS"


# Environment

export NODE_ENV=development

# ART mode
if [[ "$MODE" = "REC" ]]; then
  export RECORDER_ENABLED=TRUE
  export RECORDER_RECORDINGS_DIR=/tmp/recsdir
fi

# Logger parameters
export LOGGER_IS_ASYNC=False
export LOGGER_LEVEL=Debug
export LOGGER_FILE_PATH=/tmp/euler-backend.log
export LOGGER_TO_CONSOLE=True
export LOGGER_TO_FILE=True


# ecRedis parameters
export DEV_REDIS_CONNECT_HOST=localhost
export DEV_REDIS_CONNECT_PORT=6379
export DEV_REDIS_CONNECT_DATABASE=0
export DEV_REDIS_CONNECT_MAX_CONNECTIONS=50
export DEV_REDIS_CONNECT_MAX_IDLE_TIME=30

# kvRedis parameters
export DEV_REDIS_CLUSTER_CONNECT_HOST=localhost
export DEV_REDIS_CLUSTER_CONNECT_PORT=30001
export DEV_REDIS_CLUSTER_CONNECT_DATABASE=0
export DEV_REDIS_CLUSTER_CONNECT_MAX_CONNECTIONS=50
export DEV_REDIS_CLUSTER_CONNECT_MAX_IDLE_TIME=30

#MySQL parameters
export DEV_MYSQL_CONNECTION_NAME=eulerMysqlDB
export DEV_MYSQL_CONNECT_HOST=localhost
export DEV_MYSQL_CONNECT_PORT=3306
export DEV_MYSQL_CONNECT_USER=cloud
export DEV_MYSQL_CONNECT_PASSWORD=scape
export DEV_MYSQL_CONNECT_DATABASE=jdb
export DEV_MYSQL_CONNECT_PATH=""
export DEV_MYSQL_POOL_STRIPES=1
export DEV_MYSQL_POOL_KEEP_ALIVE=10
export DEV_MYSQL_POOL_RESOURCES_PER_STRIPE=50

stack exec euler-backend --rts-options "$RTS_OPTIONS"

EOF