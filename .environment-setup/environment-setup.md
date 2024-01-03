# Preparing Windows 
Ubuntu will be used to run a number of scripts for this repository. Another Linux-distro could probably also be used. When your default environment is Windows, please check if WSL version 2 in installed. You can check this by opening a Windows terminal and try to set the default version to '2':
```
wsl --set-default-version 2
```

If that fails, you probably have to upgrade WSL. For more information, check: https://learn.microsoft.com/en-us/windows/wsl/install-manual 
### TL;DR version of WSL2 upgrade: 
Download WSL2 upgrade:
```
curl.exe --output %USERPROFILE%/downloads/wsl_update_x64.msi --url https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi
```
Install WSL2 upgrade:
```
%USERPROFILE%/downloads/wsl_update_x64.msi
```
Set WSL version 2 as your default version
```
wsl --set-default-version 2
```

Install Ubuntu
```
wsl --install -d Ubuntu
```
Just to be sure; set Git in Windows to use the windows credential manager (that will also be use by Ubuntu later)
```
git config --global credential.helper wincred
```
You should now be able to run Ubuntu from the Windows start menu

# Preparing Ubuntu

open a new Ubuntu terminal and create/open the file sim-on-fhir-setup.sh in text editor 'nano':
```
nano sim-on-fhir-setup.sh
```


Copy this to the file (right mouse click to paste in nano):
```
#!/bin/bash
echo "-----making a bin-directory in the user-home"
mkdir -p bin

echo "-----updating package tool"
sudo apt update -y 

echo "-----Installing midnight-commander (file-explorer), java, protobuf, dotnet, the prerequisites for bazel (g++ unzip zip) and the prerequisites for jekyll (ruby-full build-essential zlib1g-dev)"
sudo apt install -y mc default-jdk protobuf-compiler dotnet-sdk-6.0 g++ unzip zip ruby-full build-essential zlib1g-dev

if ! bazel version > /dev/null ; then
   wget https://github.com/bazelbuild/bazel/releases/download/7.0.0/bazel-7.0.0-installer-linux-x86_64.sh
   chmod +x bazel-7.0.0-installer-linux-x86_64.sh
   ./bazel-7.0.0-installer-linux-x86_64.sh --user
fi

echo "-----Setting environment variables to bash configuration file"
export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
echo 'export JAVA_HOME="'$JAVA_HOME'"' >> ~/.bashrc
echo 'export PATH=$PATH:~/.dotnet/tools' >> ~/.bashrc

echo '# Install Ruby Gems to ~/gems' >> ~/.bashrc
echo 'export GEM_HOME="$HOME/gems"' >> ~/.bashrc
echo 'export PATH="$HOME/gems/bin:$PATH"' >> ~/.bashrc

echo "-----reload bash configuration file"
source ~/.bashrc

#niet nodig?
# echo "-----Installing fonts for Java FHIR-publisher"
# sudo apt-get -y install libfreetype6 fontconfig



echo "-----Installing Go-protoc-module"
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

echo "-----Installing npm & sushi"
npm install -g npm@latest 
npm install -g fsh-sushi

echo "-----Installing go"
if ! go version > /dev/null ; then
   wget https://go.dev/dl/go1.21.5.linux-amd64.tar.gz 
   #extract the archive you just downloaded into /usr/local, creating a fresh Go tree in /usr/local/go
   sudo tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz 

   #To set environment variables we will open the .profile file in our home directory. Then we append the PATH
   echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile
   #force the Ubuntu terminal to reload the user-profile configuration file:
   source .profile
fi

echo "-----Installing Jekyll"
gem install jekyll bundler

echo "-----Installing Firely Terminal"
if ! fhir --version > /dev/null ; then
   dotnet tool install -g firely.terminal
fi


#Configure WSL to use the Windows credential helper
git config --global credential.helper "/mnt/c/Program\ Files/Git/mingw64/bin/git-credential-manager-core.exe"

echo "-----check if go is correctly installed"
go version 
echo "-----check if java is correctly installed"
java -version
echo "-----check if protobuf-compiler is correctly installed"
protoc --version
echo "-----check if bazel is correctly installed"
bazel version
echo "-----check if jekyll is correctly installed"
jekyll --version

echo "-----Downloading protobuf-scripts to generate proto-files from FHIR-profiles"

curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_protos_utils.sh -o ~/bin/generate_protos_utils.sh
curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_protos.sh -o ~/bin/generate_protos.sh
curl https://raw.githubusercontent.com/google/fhir/master/bazel/generate_definitions_and_protos.sh -o ~/bin/generate_definitions_and_protos.sh
sudo chmod +x ~/bin/generate_protos.sh
sudo chmod +x ~/bin/generate_definitions_and_protos.sh


echo "-----Please set git credentials"

echo -n "Your name: "
read -r name

echo -n "Your email adress: "
read -r email

git config --global user.name $name
git config --global user.email $email

echo "-----Cloning git repo"
git clone https://github.com/SanteonNL/sim-on-fhir
cd sim-on-fhir
sudo chmod +x _updatePublisher.sh

echo "-----Installing/downloading Java FHIR-publisher"
sudo _updatePublisher.sh -y
```

Save & exit sim-on-fhir-setup.sh (Ctrl-X)

Configure permission to execute sim-on-fhir-setup.sh
```
chmod +x sim-on-fhir-setup.sh
```
Execute sim-on-fhir-setup.sh
```
./sim-on-fhir-setup.sh
```

Now, you can start vscode
```
code .
```