echo "Installing Apt Toolings"
apt-get update
apt-get install -y apt-utils software-properties-common git make curl wget gpg

echo "Installing Emacs 27.1"
add-apt-repository ppa:kelleyk/emacs
apt-get update
apt-get install -y emacs27-nox

echo "Installing Docker"
apt-get install -y docker.io

echo "Installing Nodejs"
apt-get install -y nodejs

echo "Installing Python3"
apt-get install -y python3

echo "Installing Lua"
apt-get install -y luarocks

echo "Installing Go"
apt-get install -y golang

echo "Installing Openjdk"
apt-get install -y openjdk-11-jdk

echo "Installing Browsh"
wget https://github.com/browsh-org/browsh/releases/download/v1.6.4/browsh_1.6.4_linux_amd64.deb
apt-get install ./browsh_1.6.4_linux_amd64.deb
rm ./browsh_1.6.4_linux_amd64.deb

echo "Installing fzf"
apt-get install -y fzf

echo "Installing Ripgrep"
apt-get install ripgrep

echo "Installing Mosh"
git clone https://github.com/mobile-shell/mosh
cd mosh
apt-get -y install automake libtool g++ protobuf-compiler \
  libprotobuf-dev libboost-dev libutempter-dev libncurses5-dev \
  zlib1g-dev libio-pty-perl libssl-dev pkg-config
./autogen.sh
./configure
make
make install
cd ..

echo "Installing Oh my Bash"
bash -c "$(curl -fsSL https://raw.githubusercontent.com/ohmybash/oh-my-bash/master/tools/install.sh)"

echo "Installing Lazydocker"
go get github.com/jesseduffield/lazydocker
