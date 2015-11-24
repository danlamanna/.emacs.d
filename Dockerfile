FROM fedora:23
RUN dnf install -y emacs
ADD . /emacs

ENTRYPOINT ["emacs"]
