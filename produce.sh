# /bin/bash

seed=`date +%Y%j%H`

echo "=== $(date), using seed ${seed} ==="

if test "$#" -ne 0
then
    for i
    do
        args="${args} world/$i/"
    done
else
    args=world/*/
fi

echo ${args}

for dir in ${args}
do
    dir=${dir%*/}
    name=${dir##*/}
    rm -f ${dir}/${name}.out ${dir}/${name}.tw ${dir}/index.html
    echo "Producing ${name} from ${dir##/home/www/}"
    celf -s ${seed} ${dir}/${name}.clf > ${dir}/${name}.out
    wc=`wc -l ${dir}/${name}.out | awk {'print $1'}`
    echo "   Produced ${name} script (${name}.out), ${wc} lines."
    ./tamaraify ${dir}/${name}.out ${dir}/${name}.scenes ${dir}/${name}.tw ${seed} 50000
    wc=`wc -w ${dir}/${name}.tw | awk {'print $1'}`
    echo "   Produced ${name} twee (${name}.tw, ${wc} words)"
    twee ${dir}/${name}.tw > ${dir}/index.html
    wc=`wc -c ${dir}/index.html | awk {'print $1'}`
    echo "   Produced html from twee (index.html, ${wc} bytes)"
done
