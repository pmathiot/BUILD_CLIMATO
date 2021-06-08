#!/bin/bash
##SBATCH --mem=8G
#SBATCH --nodes=1
##SBATCH --ntasks=13
##SBATCH --ntasks-per-node=24
#SBATCH --constraint BDW28
#SBATCH --threads-per-core=1
#SBATCH --time=01:00:00
#SBATCH --exclusive


trap "exit 1" TERM
export TOP_PID=$$

function build_mean {

      FREQ=$1
      MM=$2
      YEAR1=$3
      YEAR2=$4

      FILEOUT=`get_mymfilename $FREQ $YEAR1 $YEAR2 $MM`
      if [[ $? -ne 0 ]]; then exit 1; fi

# if mean still in the meaning directory => cleanning
      if [ -f $FILEOUT ]; then
         echo "mean ${FILEOUT} corrupted, auto clean"
         echo ''
         rm -f ${FILEOUT}; 
         if [ -f $CLIMPATH/${FILEOUT} ]; then rm -f $CLIMPATH/${FILEOUT}; fi
      fi

# if file already at the right place, nothing to do.
      if [ ! -f $CLIMPATH/${FILEOUT} ]; then 

         TAG=`get_tag ${FREQ} ???? $MM 01`
         GLOBFILE=`get_nemofilename`
         LIST_FILE=`ls $GLOBFILE`

# sanity check
         NFILE=`echo $LIST_FILE | wc -w`
         echo "$NFILE files to average:"
         echo $LIST_FILE
         echo ''
         NFILETOHAVE=$((YEAR2-YEAR1+1))

         if [[ $NFILE -ne $NFILETOHAVE ]] ; then 
             echo "  $NFILE files is wrong for $SEASO, should be $NFILETOHAVE, exit "
             kill -s TERM $TOP_PID
             exit 1
         fi

# mk mean
         echo " start averaging: $BIN_NEMO $LIST_FILE $FILEOUT "
         echo ''
         $BIN_NEMO $LIST_FILE $FILEOUT
         if [[ $? -ne 0 ]]; then
             echo " error in built file $FILEOUT; exit"
             kill -s TERM $TOP_PID
             exit 1
         fi

         mv ${FILEOUT} $CLIMPATH/.

# cleaning
         if [[ $? -eq 0 ]]; then
            echo ''
            echo "cleaning file: "
            ls $GLOBFILE
            echo ''
            rm -f $GLOBFILE
         else
            echo "error in mv output file; exit"
            exit 1
         fi

      else

         echo " mym file ${FILEOUT} already there, nothing to do."
         echo ''

      fi
}

RUNID=$1
GRID=$2
YEARB=$3
YEARE=$4

BUILD_DIR=`pwd`
echo $BUILD_DIR

. param.bash

CLIMPATH=$WRKPATH/${RUNID}_${YEARB}-${YEARE}
LOGPATH=$LOGDIR/${RUNID}_${YEARB}-${YEARE}
BIN_NEMO=$BUILD_DIR/src/f90/mean_nemo.exe
TRIGGER=`get_triggername`
#===========================================

cd $CLIMPATH
ulimit -s unlimited
if [ ! -d $LOG_DIR ]; then mkdir -p $LOG_DIR ; fi
for MM in 01 02 03 04 05 06 07 08 09 10 11 12; do
   echo "built monthly mean ${YEARB}-${YEARE} m$MM ${GRID} ..."
   cd $CLIMPATH/${GRID}/m$MM/.
   FREQ=1m 
   build_mean $FREQ $MM $YEARB $YEARE > $LOGPATH/log_mkmean_${GRID}_m$MM 2>&1 &
   if [[ $MM == 03 || $MM == 06 || $MM == 09 || $MM == 12 ]] ; then wait ; fi
done

for SEASO in ANN; do
   echo "build yearly mean ${YEARB}-${YEARE} ${SEASO} ${GRID} ..."
   cd $CLIMPATH/${GRID}/$SEASO/.
   FREQ=1y
   build_mean $FREQ $MM $YEARB $YEARE > $LOGPATH/log_mkmean_${GRID}_ANN 2>&1 &
done

wait

echo ''
echo "build multi year mean ${YEARB} ${YEARE} ${GRID} done"

rm ${TRIGGER}
