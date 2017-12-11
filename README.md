# Code Assessment Webservice
A robust, scalable microservice for compiling submitted samples of java code and running the resulting binary against provided test cases.

## Requirements

The webservice is deployed through a docker container. The only requirement is having docker installed on the host computer. In other words, the command
```bash
docker run hello-world
```
should run successfully. If you wish to test the service, you will also need to install [httpie](https://httpie.org).

## Overview

The service is designed around the principles of fault tolerance and allowing independent scaling of the various subcomponents. The three main components of the service are
    * A simple webserver implementing the REST API for interacting with the service
    * A reliable work queue, using [Redis](redis.io) as the in-memory data store
    * A worker program for processing the submitted jobs


## Installation

To install the service, first clone the repository
```bash
git clone https://github.com/safnuk/assessment-webservice.git assessment && cd assessment
```
Then build the docker image
```bash
docker build -t safnuk/assessment .
```
Note that trailing dot at the end of the command - it is required.

## Running the service

After following the above installation steps, to deploy you run the command
```bash
export NUM_WORKERS=4 && export SERVICE_PORT=3000 && docker run -p $SERVICE_PORT:3000 -d --name assessment safnuk/assessment $NUM_WORKERS
```
Here `NUM_WORKERS` specified the number of worker threads to spawn, while `SERVICE_PORT` is the port used to access the service.

You can confirm that the service is running through
```bash
http :$SERVICE_PORT/health
```
If you see a response of `UP`, then the service is operational.

## Testing the service
