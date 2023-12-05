#!/bin/bash

echo "Hello world from postCreateCommand.sh"

dotnet tool list

dotnet tool install -g firely.terminal

# sudo apt-get install libfreetype6
# sudo ln -s /usr/lib/x86_64-linux-gnu/libfreetype.so.6.10.1 /usr/lib/libfreetype.so.6
# sudo apt-get install libfreetype6
# sudo apt-get -y install fontconfig

sudo ./_updatePublisher.sh -y

mkdir bin

curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_protos_utils.sh -o ./bin/generate_protos_utils.sh     && \
  curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_protos.sh -o ./bin/generate_protos.sh && \
  curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_definitions_and_protos.sh -o ./bin/generate_definitions_and_protos.sh && \
  sudo chmod +x ./bin/generate_protos.sh && sudo chmod +x ./bin/generate_definitions_and_protos.sh