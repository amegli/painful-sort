FROM debian:latest
RUN apt-get update
RUN apt-get install -y emacs-nox
WORKDIR /root
COPY ./emacs/.emacs /root/.emacs
