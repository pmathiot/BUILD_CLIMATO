#!/bin/bash

module load intel/17.0 intelmpi/2017.0.098 hdf5/1.8.17 netcdf/4.4.0_fortran-4.4.2

# where original data are store
STOPATH=${SCRATCHDIR}/DRAKKAR/${CONFIG}/${RUNID}-S/

# where the processing is done
WRKPATH=${SCRATCHDIR}/BUILD_CLIMATO/${RUNID}/

# grid (update it for your need)
GRID_LST='gridT gridU gridV gridW flxT icemod'

# log directory
LOGDIR=$BUILD_DIR/LOG/LOG_$RUNID/

# trigger file
get_triggername() {
  echo ${BUILD_DIR}/run_${RUNID}_${YEARB}-${YEARE}_${GRID}
}

# get NEMO FILE
get_nemofilename() {
  echo ${RUNID}_${TAG}.${FREQ}_${GRID}.nc
}

# get TAG
get_tag() {
  FREQ=$1 ; YYYY=$2 ; MM=$3 ; DD=$4
  if [ $FREQ == '1y' ]; then
     echo y${YYYY}
  elif [ $FREQ == '1m' ]; then
     echo y${YYYY}m${MM}
  else
     echo y${YYYY}m${MM}d${DD}
  fi
}

# get output file name
get_mymfilename() {
  FREQ=$1 ; YEAR1=$2 ; YEAR2=$3; MM=$4
  FREQOUT=$((YEAR2-YEAR1+1))y
  if [ $FREQ == '1y' ]; then
     echo ${RUNID}_y${YEAR1}.${FREQOUT}_${GRID}.nc
  elif [ $FREQ == '1m' ]; then
     echo ${RUNID}_y${YEAR1}m${MM}.${FREQOUT}_${GRID}.nc
  else
     echo "FREQ_not_supported"
  fi
}
