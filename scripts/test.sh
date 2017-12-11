#!/bin/sh

docker run -p 3000:3000 -d --name assessment-test safnuk/assessment \
    || (docker build -t safnuk/assessment . \
    && docker run -p 3000:3000 -d --name assessment-test safnuk/assessment)
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${RED}======== Test with working code sample =========${NC}"
set -x
http :3000/submissions username=bob exercise_id:=0 code="`cat tests/good.txt`"
sleep 1
http :3000/submissions/status?id=1
set +x

echo -e "${RED}======== Test with partially working code sample =========${NC}"
set -x
http :3000/submissions username=bob exercise_id:=0 code="`cat tests/hello.txt`"
sleep 1
http :3000/submissions/status?id=2
set +x

echo -e "${RED}======== Test too many requests from same user =========${NC}"
set -x
http :3000/submissions username=bob exercise_id:=0 code="`cat tests/hello.txt`"
set +x

echo -e "${RED}======== Test code sample with error =========${NC}"
set -x
http :3000/submissions username=frank exercise_id:=0 code="`cat tests/bad.txt`"
sleep 1
http :3000/submissions/status?id=3
set +x

docker stop assessment-test
docker rm assessment-test
