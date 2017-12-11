#! /bin/bash
nohup redis-server &
nohup dummy &
for ((i = 1; i <= $1; i++)); do
    nohup worker &
done
assessment

