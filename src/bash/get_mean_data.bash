#!/bin/bash
# DIRECTIVES:
#PBS -N get_data
#PBS -q shared
#PBS -V
#PBS -l walltime=03:00:00
#PBS -l mem=4gb
#PBS -l ncpus=2
#===========================================

function get_mean_data {
      #set -x
# build monthly mean
# get the file list for monthly mean
      FREQ=$1
      MM=$2
      YY1=$3
      YY2=$4
      FILE_LST=''
      YEAR=$YY1
      while [[ $YEAR -le $YY2 ]]; do
          TAG=`get_tag ${FREQ} $YEAR $MM 01`
          FILE=`get_nemofilename`
          FILE_LST=`echo ${FILE_LST} ${STOPATH}/$FREQ/${YEAR}/${FILE}`
          YEAR=$((YEAR+1))
      done
      for MFILE in `echo ${FILE_LST}`; do
          FILE=`basename ${MFILE}`
          if [ -f ${FILE}_in_progress ]; then
             echo -e "\e[0;31;1m    file $FILE is corrupted ; auto cleaning \e[0m"
             rm $FILE
          fi
          if [ ! -f $FILE ]; then
             touch ${FILE}_in_progress
             echo "    file ${FILE} ..."
             cp $MFILE .
             if [[ $? -ne 0 ]]; then 
                echo -e "\e[0;31;1m    downloading file ${FILE} failed ; auto cleaning; exit \e[0m"
                rm ${FILE};
                exit 1
             fi
             rm ${FILE}_in_progress
          fi
      done
}
#===========================================

CLIMPATH=$WRKPATH/${RUNID}_${YEARB}-${YEARE}
LOGPATH=$LOGDIR/${RUNID}_${YEARB}-${YEARE}

if [ ! -d $LOGPATH  ]; then mkdir -p $LOGPATH  ; fi
if [ ! -d $CLIMPATH ]; then mkdir -p $CLIMPATH ; fi

for MM in 01 02 03 04 05 06 07 08 09 10 11 12; do
   if [ ! -d $CLIMPATH/${GRID}/m$MM ]; then mkdir -p $CLIMPATH/${GRID}/m$MM ; fi
   cd $CLIMPATH/${GRID}/m$MM/.
   FREQ=1m
   FILEOUT=`get_mymfilename $FREQ $YEARB $YEARE $MM`
   if [ ! -f $CLIMPATH/$FILEOUT ]; then
      echo " downloading month $MM in progress ..."
      get_mean_data $FREQ $MM ${YEARB} ${YEARE} # > $LOGPATH/log_getdata_${YEARB}-${YEARE}_${GRID}_m$MM 2>&1
   else
      echo " mym file $FILEOUT already built, nothing to do "
   fi
done

for SEASO in ANN; do
   if [ ! -d $CLIMPATH/${GRID}/$SEASO ]; then mkdir -p $CLIMPATH/${GRID}/$SEASO ; fi
   cd $CLIMPATH/${GRID}/$SEASO/.
   FREQ=1y
   FILEOUT=`get_mymfilename $FREQ $YEARB $YEARE $SEASO`
   if [ ! -f $CLIMPATH/$FILEOUT ]; then
      echo ' downloading annual file in progress ...'
      get_mean_data $FREQ 01 $YEARB $YEARE  #> $LOGPATH/log_getdata_${YEARB}-${YEARE}_${GRID}_$SEASO 2>&1
   else
      echo " mym file ${FILEOUT} already built, nothing to do "
   fi
done

touch ${TRIGGER}

cd $BUILD_DIR/

echo ''
echo 'start averaging ...'
echo ''

sbatch -J mym_$GRID -o ${LOGPATH}/mym_${GRID}.out -e ${LOGPATH}/mym_${GRID}.err $BUILD_DIR/src/bash/schedul_monthly_mean.bash $RUNID $GRID $YEARB $YEARE 

