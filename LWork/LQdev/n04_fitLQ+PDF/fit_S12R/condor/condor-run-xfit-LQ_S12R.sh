#!/bin/bash

if [ ! -z $LD_LIBRARY_PATH_STORED ]; then
  echo "Using LD_LIBRARY_PATH_STORED"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH_STORED:$LD_LIBRARY_PATH
fi

echo "PATH=$PATH"
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
echo "command to run:  ../xfitter"

../xfitter
