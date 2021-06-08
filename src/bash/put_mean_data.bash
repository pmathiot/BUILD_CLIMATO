#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --constraint BDW28
#SBATCH --threads-per-core=1
#SBATCH --time=01:00:00

ulimit -s unlimited

move_files() 
{
   DIROUT=$1
   FILESIN=${@:2}
   echo " saving file : "
   for FILE in `ls $FILESIN`; do
       echo "-- $FILE"
       mv $FILE $DIROUT/.
   done
   echo ''
}

CONFIG=$1
CASE=$2
YEARB=$3
YEARE=$4
RUNID=${CONFIG}-${CASE}
BUILD_DIR=`pwd`

. param.bash

cd $WRKPATH/${RUNID}_${YEARB}-${YEARE}

FREQOUT=$(($YEARE-$YEARB+1))
if [ ! -d $MEANPATH/${FREQOUT}y/$YEARB ]; then 
   echo " create $MEANPATH/${FREQOUT}y/$YEARB"
   mkdir -p $MEANPATH/${FREQOUT}y/$YEARB
fi

for GRID in $GRID_LST ; do

   FILEOUTM=`get_mymfilename 1m $YEARB $YEARE \?\?`
   NFILE=`ls $FILEOUTM | wc -l `
   if [[ $NFILE -ne 12 ]]; then echo "error in number of monthly files in $WRKPATH/${RUNID}_${YEARB}-${YEARE} for grid $GRID : $NFILE"; exit 42; fi

   FILEOUTY=`get_mymfilename 1y $YEARB $YEARE \?\?`
   NFILE=`ls $FILEOUTY | wc -l `
   if [[ $NFILE -ne 1 ]]; then echo "error in number of annual files in $MEANPATH for grid $GRID : $NFILE"; exit 42; fi

   move_files $MEANPATH/${FREQOUT}y/$YEARB $FILEOUTM

   move_files $MEANPATH/${FREQOUT}y/$YEARB $FILEOUTY
   
   echo ''
done
