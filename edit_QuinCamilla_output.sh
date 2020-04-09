#!/bin/bash

# User input
workdir='/Users/rpr061/Dropbox/BCDC_projects/ICOS/ICOS_Labelling_work_folder/Reports/09_Oestgarnsholm'
QCoutfile='QuinCeReady_SE_Oes_Water_OTC_data_2016_8-11_sal4dec_NODEPTH_txt-ICOS OTC Labelling Step 2.csv'
typeofstation='FOS'

# -----
# VOS: Salinity usually; FOS: Equilibrator and Atmospheric variables
if [ ${typeofstation} = 'VOS' ]; then
      removevariables[0]=Salinity
elif [ ${typeofstation} = 'FOS' ]; then
      removevariables[0]=Atmospheric
      removevariables[1]='Equilibrator'
else
      echo 'Provide a valid type of station (VOS/FOS)'
fi

tempoutfile=$(echo "${QCoutfile// /_}.tmp")
cp "${workdir}/${QCoutfile}" "${workdir}/${tempoutfile}" 
vi  "${workdir}/${tempoutfile}" -c ":1" -c ":s/ /\./g" -c ":%s/₂/2/g" -c ":wq"
awk -F',' '{print $13}' "${workdir}/${tempoutfile}" >  "${workdir}/originalmessages.tmp"
cp  "${workdir}/originalmessages.tmp"  "${workdir}/messages.tmp"

for count in $(seq 0 $(( ${#removevariables[@]} - 1 ))); do 
      removevariable=${removevariables[${count}]}
      echo $removevariable
      awk 'BEGIN{FS=OFS=";"} { for(i=1;i<=NF;i++) {if ($i ~/'$removevariable'/) {$i=""}} ; print }' "${workdir}/messages.tmp" > "${workdir}/cleanedmessages.tmp"
      cp "${workdir}/cleanedmessages.tmp" "${workdir}/messages.tmp"  

done

vi  "${workdir}/cleanedmessages.tmp"  -c ":%s/;;/;/g" -c ":%s/;$//g" -c ":%s/^;//g" -c ":wq"
awk 'BEGIN{FS=OFS=","} FNR==NR{a[NR]=$1;next}{$13=a[FNR]}1' "${workdir}/cleanedmessages.tmp" "${workdir}/${tempoutfile}" >   "${workdir}/test.tmp"
vi  "${workdir}/test.tmp"  -c ":%s/,4,,\([0-9.].*$\)/,2,,\1/g" -c ":wq"
mv "${workdir}/test.tmp" "${workdir}/${QCoutfile}"

# Cleanup
rm -i -f -- "${workdir}/*.tmp"
