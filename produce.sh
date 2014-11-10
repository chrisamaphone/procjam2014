# /bin/bash

seed=`date +%Y%j%H`

echo "=== $(date), using seed ${seed} ==="

for dir in world/*/
do
    dir=${dir%*/}
    name=${dir##*/}
    echo "Producing ${name} from ${dir##/home/www/}"
    celf -s ${seed} ${dir}/${name}.clf > ${dir}/${name}.out
    wc=`wc -l ${dir}/${name}.out | awk {'print $1'}`
    echo "   Produced ${name} script (${name}.clf), ${wc} lines"
    ./tamaraify ${dir}/${name}.out ${dir}/${name}.tw 50
    twee ${dir}/${name}.tw > ${dir}/index.html
done
