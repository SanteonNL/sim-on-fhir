# Getting started with protobufs for Golang 
For more info, check https://protobuf.dev/getting-started/gotutorial/ 
1. If you haven’t installed the compiler, [download the package](https://protobuf.dev/downloads) and **follow the instructions in the README**.
2. Run the following command to install the Go protocol buffers plugin:
```
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
```
3. run this command to generate go structs: 
```
protoc --go_out=../golang addressbook.proto
```


FHIR:
install Bazel https://bazel.build/install

Generating custom profiles and protos makes use of a couple of scripts defined by the FhirProto library. To add these to your (protoc) bin, run
```
curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_protos_utils.sh > ~/protoc-25.1-win64/bin/generate_protos_utils.sh     && \
  curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_protos.sh > ~/protoc-25.1-win64/bin/generate_protos.sh && \
  curl https://raw.githubusercontent.com/google/fhir/v0.5.0/bazel/generate_definitions_and_protos.sh > ~/protoc-25.1-win64/bin/generate_definitions_and_protos.sh && \
  chmod +x ~/protoc-25.1-win64/bin/generate_protos.sh && chmod +x ~/protoc-25.1-win64/bin/generate_definitions_and_protos.sh

```