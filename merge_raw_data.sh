#!/bin/bash

# ALL THIS ASSUMES 1-LINE HEADERS!!

rawdatadir='/Users/rpr061/Documents/DATAMANAGEMENT/Projects/ICOS/test/Nuka_raw_data/data'
workspacedir='/Users/rpr061/Documents/DATAMANAGEMENT/Projects/ICOS/test/Nuka_raw_data/workspace'
outfilename='AllNuka2017.txt'

echo $outfilename

if [ ! -d ${workspacedir} ]; then
      mkdir ${workspacedir}
fi

files=( $(find $rawdatadir -name "Nuka*dat.txt") ) # use the () to store into an ARRAY
echo ${files[0]}
# Create "structure" file with the header. 
cd ${workspacedir}
for f in $(seq 0 $((${#files[@]} - 1))) 
do head -n 1 ${files[$f]} >> structure.tmp
done


# Sort structure file and identify how many uniques headers (excluding 1st column, i.e. file name)
sort structure.tmp -k 1 > sortedstructure.tmp
uniq sortedstructure.tmp > uniqueheaders.tmp

# More than one type of header (different columns, in number and/or order)
if [ $(wc -l < uniqueheaders.tmp) -gt 1 ]; then
      for d in $(seq 1 $(($(wc -l < uniqueheaders.tmp) - 0)))
      do mkdir -p S$d

            # Pick one header
            uniheader=$(sed -n $d'p' uniqueheaders.tmp)
            # Find lines (and therefore filenames) with same header in structure file
            filesuniheader=( $(grep -r "$uniheader" ${rawdatadir} | cut -d: -f 1) )
            echo "First file with header #${d}: ${filesuniheader[0]}"
            # copy the files into the folder
            for fuh in $(seq 0 $((${#filesuniheader[@]} - 1)))
            do cp ${filesuniheader[$fuh]} ./S$d/
            done
            echo "All ${#filesuniheader[@]} files have been copied to folder S${d}"

            cd ./S$d
            # Join into one large file allS.txt, with only one the first line of headers.(and do calculations in Excel)
            head -n 1 ${filesuniheader[0]} > "${workspacedir}/S${d}_${outfilename}"
            for fuh in $(seq 0 $((${#filesuniheader[@]} - 1)))
            do tail -n +2 ${filesuniheader[$fuh]} >> "${workspacedir}/S${d}_${outfilename}"
                  #echo "${filesuniheader[$fuh]} added to the main file ${workspacedir}/S${d}_${outfilename}"
            done

            cd ..
      done


      # Find all variables measured
      count=0
      while read line
      do
            echo $count
            vars=( $(echo  ${line// /_}) )
            echo ${#vars[@]}
            echo $vars
            #echo "${vars[@]}" > "${workspacedir}/L${count}"
            echo "${line}" > "${workspacedir}/LL${count}.tmp"
            tr '\t' '\n' < "${workspacedir}/LL${count}.tmp" > "${workspacedir}/LLL${count}.tmp"

            if [ "$(uname)" == "Darwin" ]; then
                  dos2unix  "${workspacedir}/LLL${count}.tmp"
            fi

            (( count++ ))
      done < "${workspacedir}/uniqueheaders.tmp"

      numberfiles=$(( $count - 1 ))
      for n in $(seq 1 $numberfiles); do
            echo file compared "${workspacedir}/LLL${n}.tmp"
            grep -Fxvf "${workspacedir}/LLL0.tmp" "${workspacedir}/LLL${n}.tmp"  >> "${workspacedir}/allextravars.tmp"
            head "${workspacedir}/allextravars.tmp"
      done

      sort -u "${workspacedir}/allextravars.tmp" > "${workspacedir}/extravars.tmp"
      cat "${workspacedir}/LLL0.tmp" "${workspacedir}/extravars.tmp" >  "${workspacedir}/allvars.tmp"


      # Merge dataset in one single file. If variable doesn't exist, create NaN column. If exist, fill empty fields with NaN
      nvars=$(wc -l < "${workspacedir}/allvars.tmp")
      echo $nvars


      for nv in $(seq 1 $(( ${nvars} ))); do
            var=$(sed "${nv}q;d" "${workspacedir}/allvars.tmp")
            echo variable name: $var
            echo $var > "${workspacedir}/tempvarvalues.tmp" 

            for Sfile in ${workspacedir}/S*.txt; do
                  ivar=$(sed -n $'1s/\t/\\\n/gp' ${Sfile} | grep -n "${var}" | cut -d: -f1)

                  echo $Sfile
                  echo variable column: $ivar

                  if [ -z "${ivar}" ]; then
                        lengthvar_1=$(wc -l < $Sfile)
                        lengthvar=$(( ${lengthvar_1} - 1 ))
                        echo file length $lengthvar
                        for lv in $(seq 0 $(( ${lengthvar} - 1 )) ); do
                              vararray[${lv}]=NaN
                        done
                        echo length nan array: ${#vararray[@]}

                  else
                        oldIFS="$IFS"
                        IFS=$'\n'
                        vararray_h=( $(awk -F'\t' -v col=$ivar '{ if ($col==""){print "NaN"} else {print $col} }' ${Sfile}) )
                        IFS="$oldIFS"
                        vararray=("${vararray_h[@]:1}")

                  fi

                  echo variable length: ${#vararray[@]}

                  printf "%s\n" "${vararray[@]}" >> "${workspacedir}/tempvarvalues.tmp"
                  dos2unix "${workspacedir}/tempvarvalues.tmp"
                  vararray=()
            done

            if ((${nv} == 1)); then
                  cp "${workspacedir}/tempvarvalues.tmp" "${workspacedir}/allvalues.tmp"
            else
                  paste "${workspacedir}/tempallvalues.tmp" "${workspacedir}/tempvarvalues.tmp" > "${workspacedir}/allvalues.tmp"
            fi

            cp "${workspacedir}/allvalues.tmp" "${workspacedir}/tempallvalues.tmp"
            rm  "${workspacedir}/tempvarvalues.tmp"

      done

      cp "${workspacedir}/allvalues.tmp" "${workspacedir}/${outfilename}" 
      ### If only 1 type of header, merge into one file
else
      for f in $(seq 0 $((${#files[@]} - 1)))
      do tail -n +2 ${files[$f]} >> "${workspacedir}/${outfilename}"
      done

fi

pwd

# Cleanup
rm -i -f -- ${workspacedir}/*.tmp
