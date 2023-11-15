#!/bin/sh

while true; do
  ab -n 100 -c 5 http://localhost:8080/todos > /dev/null 2>&1

  sleep 2
done
