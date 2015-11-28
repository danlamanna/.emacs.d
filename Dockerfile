FROM fedora:23

RUN dnf install -y npm
RUN npm install -g jscs

RUN dnf install -y emacs

ADD . /emacs

ENTRYPOINT ["emacs"]
