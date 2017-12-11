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

* A simple webserver implementing a REST API for interacting with the service
  
  The two main ways to interact with the service are by posting code samples to `/submissions` and then get their status from `/submissions/status`. Data is passed and returned in JSON format. Refer to the file `scripts/test.sh` for examples of properly formatted submissions to the service.

* A reliable work queue, using [Redis](redis.io) as the in-memory data store

  See the [tutorial](https://github.com/safnuk/reliable-worker-queue) for an explanation of the reliable work queue pattern which is implemented for the project.
  
* A worker program for processing the submitted jobs

  The program pops submitted jobs from the work queue, saves the code snippet to a temporary file, compiles with `javac`, then runs the compiled binary with inputs provided from the assignment service. The output is recorded and compared to the expected output provided by the assignment service. The workers are highly scalable, in that one can spawn as many as desired, on any number of servers. They do not have to be on the same server as either the web service or the Redis backend.

## Implementation details

The service is written in Haskell, using the [Scotty](https://hackage.haskell.org/package/scotty) framework for the web server, the [Hworker](https://hackage.haskell.org/package/hworker) library to implement the work queue, and the [Turtle](https://hackage.haskell.org/package/turtle) library for interfacing with java for compiling and running the test cases. It also makes use of the [Req](https://hackage.haskell.org/package/req) library for sending http requests to the external logging and assignment services.

## Installation

To install the service, first clone the repository
```bash
git clone https://github.com/safnuk/assessment-webservice.git assessment && cd assessment
```
Modify the file `Config.hs` to the desired specifications. Most of the defaults are sensible and should not need to be changed, except for the fields `logPort` and `testsPort`, which specify the access ports for the logging and assignment services, respectively. The defaults refer to a dummy test server running within the container, used for testing purposes but not for production deployment.

Next build the docker image
```bash
docker build -t safnuk/assessment .
```
Note the trailing dot at the end of the command - it is required.

## Running the service

After following the above installation steps, to deploy you run the command
```bash
export NUM_WORKERS=4 && export SERVICE_PORT=3000 \
    && docker run -p $SERVICE_PORT:3000 -d --name assessment safnuk/assessment $NUM_WORKERS
```
Here `NUM_WORKERS` specifies the number of worker threads to spawn, while `SERVICE_PORT` is the port used to access the service.

You can confirm that the service is running through
```bash
http :$SERVICE_PORT/health
```
If you see a response of `UP`, then the service is operational.

Although the default deployment has the entire service running through a single container (mostly to make it easier to test and play with), in an actual production setting one would want to have each component (web server, Redis server, worker(s)) running in its own container. The design of the service makes it trivially easy to do so, as each component is an independent executable.

## Testing the service

A script to test the basic functionality of the service is run through
```bash
sh scripts/test.sh
```
It demonstrates submissions of code snippets under a variety of conditions, including passing all tests, partially passing the tests, syntax errors, and submitting too many requests.
