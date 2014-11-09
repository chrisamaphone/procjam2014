# /bin/bash

celf=/home/rjsimmon/celf/celf
seed=`date +%Y%j%H`

echo "=== Procjam production manager ==="
echo "=== $(date), producing seed ${seed} ==="

for dir in /home/www/play/u/*/
do
    dir=${dir%*/}
    name=${dir##*/}
    echo "Producing ${name} from ${dir##/home/www/}"
    ${celf} -s ${seed} ${dir}/${name}.clf > ${dir}/${name}.out
    wc=`wc -l ${dir}/${name}.out | awk {'print $1'}`
    echo "   Produced ${name} script (${name}.clf), ${wc} lines"
done

echo "=== Procjam production manager finished ==="
echo ""