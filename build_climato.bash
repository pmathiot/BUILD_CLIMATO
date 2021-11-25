#!/bin/bash
#===========================================
if [[ $# -ne 4 ]]; then echo "build_climato.bash [CONFIG] [CASE] [YEARB] [YEARE]"; exit; fi
ulimit -s unlimited
CONFIG=$1
CASE=$2
YEARB=$3
YEARE=$4
RUNID=${CONFIG}-${CASE}
BUILD_DIR=`pwd`
#===========================================
cd $BUILD_DIR

. param.bash

for GRID in $GRID_LST; do
    TRIGGER=`get_triggername`
    if [ -f ${TRIGGER} ]; then rm ${TRIGGER}; fi

    echo ''
    echo ''
    echo " $RUNID $GRID is in progress ... "
    echo ''

    . $BUILD_DIR/src/bash/get_mean_data.bash

    nit=0
    while [[ ! -f $TRIGGER && $nit -le 180 ]]; do
      sleep 60
      nit=$((nit+1))
    done

    if [[ $nit -gt 180 ]]; then 
       echo 'too much iteration, exit'
       if [ -f ${TRIGGER} ]; then rm ${TRIGGER} ; fi
       exit 42
    fi
done

for GRID in $GRID_LST; do
    TRIGGER=`get_triggername`
    while [[ -f $TRIGGER && $nit -le 180 ]]; do
      sleep 60
      printf '='
      nit=$((nit+1))
    done
    $BUILD_DIR/src/bash/put_mean_data.bash $CONFIG $RUNID $GRID $YEARB $YEARE
done
#===========================================
