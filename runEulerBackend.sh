#!/bin/bash

#
showHelp() {
cat <<EOF

Usage: runEulerBackend.sh [OPTIONS]

Options:
  -l, --log-level=<LEVEL>          Logging level DEBUG | INFO | WARNING | ERROR, default is DEBUG
  -m, --mode=<MODE>                ART mode (NORMAL | REC | PLAYER | BULK), default is NORMAL
      --status-async=true|false    Execute order status queries concurrently

EOF
}

defaultParams() {
  # defaults
  LOGGER_LEVEL=DEBUG
  MODE=NORMAL
  ORDER_STATUS_ASYNC=FALSE
}

showParams() {
  echo
  echo "-----------------------------------------------------"
  echo "LOGGER_LEVEL       = ${LOGGER_LEVEL}"
  echo "MODE               = ${MODE}"
  echo "ORDER_STATUS_ASYNC = ${ORDER_STATUS_ASYNC}"
  echo "-----------------------------------------------------"
  echo
}

# -----------------------------------------------------------------------------
defaultParams

# parse parameters
for i in "$@"
do
case $i in
    -m=REC|-m=PLAYER|-m=BULK|--mode=REC|--mode=PLAYER|--mode=BULK)
    MODE="${i#*=}"
    shift # past argument=value
    ;;
    -l=Debug|-l=Info|-l=Warning|-l=Error|--log-level=Debug|--log-level=Info|--log-level=Warning|--log-level=Error)
    LOGGER_LEVEL="${i#*=}"
    shift # past argument=value
    ;;
    --status-async=true|--status-async=false)
    ORDER_STATUS_ASYNC="${i#*=}"
    shift # past argument=value
    ;;
    -h|--help)
    showHelp
    exit 0
    ;;
    *)
          # unknown option
    ;;
esac
done

showParams




export ORDER_STATUS_ASYNC=$ORDER_STATUS_ASYNC

# RTS options
REAL_CORES=$(lscpu --all --parse=CORE,SOCKET | grep -Ev "^#" | sort -u | wc -l)
echo "Real cores      = $REAL_CORES"

RTS_OPTIONS="-A64M -AL256M -I0 -qb0 -qn$REAL_CORES -N$REAL_CORES"
echo "RTS options = $RTS_OPTIONS"




# Default environment
export NODE_ENV=development
export RECORDER_ENABLED=FALSE


# ART mode
if [[ "$MODE" = "REC" ]]; then
  export RECORDER_ENABLED=TRUE
  export RECORDER_RECORDINGS_DIR=/tmp/recsdir
fi






# Logger parameters
export LOGGER_IS_ASYNC=False
export LOGGER_LEVEL=$LOGGER_LEVEL
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


#env

stack exec euler-backend --rts-options "$RTS_OPTIONS"



EOF