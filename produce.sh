# /bin/bash

seed=`date +%Y%j%H`

echo "=== $(date), using seed ${seed} ==="

for dir in world/*/
do
    dir=${dir%*/}
    name=${dir##*/}
    rm -f ${dir}/${name}.out ${dir}/${name}.tw ${dir}/index.html
    echo "Producing ${name} from ${dir##/home/www/}"
    celf -s ${seed} ${dir}/${name}.clf > ${dir}/${name}.out
    wc=`wc -l ${dir}/${name}.out | awk {'print $1'}`
    echo "   Produced ${name} script (${name}.out), ${wc} lines."
    ./tamaraify ${dir}/${name}.out ${dir}/${name}.scenes ${dir}/${name}.tw ${seed} 50
    echo "   Produced ${name} twee (${name}.twee)."
    twee ${dir}/${name}.tw > ${dir}/index.html
    echo "   Produced html from twee."
done
