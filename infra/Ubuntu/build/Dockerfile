FROM ubuntu:20.04
ARG DEBIAN_FRONTEND=noninteractive
WORKDIR /tmp
ADD tangle.sh /root/bin/tangle
RUN chmod +x  /root/bin/tangle
ADD setup.sh  /tmp/setup.sh
RUN cd /tmp && chmod +x ./setup.sh && ./setup.sh
RUN rm /tmp/setup.sh
ADD inception.sh /tmp/inception.sh
RUN chmod +x /tmp/inception.sh
RUN echo 'export PATH=$HOME/bin:$PATH' >> /root/.bashrc
RUN echo "-"
RUN git clone https://github.com/zcaudate/etude.git /root/.emacs.d
