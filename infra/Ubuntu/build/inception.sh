cd /root/.emacs.d/infra/Ubuntu
rm -R build
mkdir build
tangle Main.org
chmod +x build/build.sh
cd build && ./build.sh
