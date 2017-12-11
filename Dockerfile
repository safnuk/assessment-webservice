from openjdk:7-jdk-slim

RUN apt-get -y update && apt-get -y upgrade
RUN apt-get -y install wget
RUN apt-get -y install redis-server
RUN wget -qO- https://get.haskellstack.org/ | sh

WORKDIR /app
ADD . /app

ENV PATH=/root/.local/bin:${PATH}
RUN cp Config.hs src/Assessment/Config.hs
RUN stack setup && stack install

# port the assessment server runs on in Docker container
EXPOSE 3000

ENTRYPOINT ["/bin/bash", "scripts/dockerstart.sh"]

# by default, start one worker to process the work queue
CMD ["1"]
