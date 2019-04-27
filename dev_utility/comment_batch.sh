#!/bin/bash
grep_results="$(grep -rwn debug_timer | grep -v ! | grep -v tags | grep -w call)"
files_list="$(echo "${grep_results}" | awk -F ':' '{print $1}')"
iline_list="$(echo "${grep_results}" | awk -F ':' '{print $2}')"
nop=$(echo "${files_list}" | wc -l)

for i in $(seq 1 ${nop})
do
  f=$(echo ${files_list} | cut -d ' ' -f $i)
  l=$(echo ${iline_list} | cut -d ' ' -f $i)
  echo "$i,$f,$l"
  sed -n "$l"'p' $f
  line_old="$(sed -n "$l"'p' $f)"
  line_new="!${line_old}"
  sed -i "$l"'a'"${line_new}" $f
  sed -i "$l"'d' $f
done

