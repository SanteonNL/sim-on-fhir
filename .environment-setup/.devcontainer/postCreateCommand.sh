#!/bin/bash

echo "Hello from postCreateCommand.sh"

echo "Installing Firely Terminal"
dotnet tool install -g firely.terminal

echo "Installing fonts for Java FHIR-publisher"
sudo apt-get update
sudo apt-get -y install libfreetype6 fontconfig

echo "Installing/downloading Java FHIR-publisher"
sudo ./_updatePublisher.sh -y

echo "Installing Go-protoc-module"
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

echo "Downloading protobuf-scripts to generate proto-files from FHIR-profiles"
mkdir -p bin
curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_protos_utils.sh -o ~/bin/generate_protos_utils.sh     && \
  curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_protos.sh -o ~/bin/generate_protos.sh && \
  curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_definitions_and_protos.sh -o ~/bin/generate_definitions_and_protos.sh && \
  sudo chmod +x ~/bin/generate_protos.sh && sudo chmod +x ~/bin/generate_definitions_and_protos.sh