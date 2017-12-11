from openjdk:7-jdk-slim

RUN apt-get -y update && apt-get -y upgrade
RUN apt-get -y install wget
RUN apt-get -y install redis-server
RUN wget -qO- https://get.haskellstack.org/ | sh

WORKDIR /app
ADD . /app

EXPOSE 6379
EXPOSE 3000
EXPOSE 3001
CMD ["/bin/bash", "start.sh"]
