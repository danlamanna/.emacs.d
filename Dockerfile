FROM fedora:23

RUN dnf install -y npm
RUN npm install -g jscs

RUN dnf install -y python-virtualenvwrapper python-pip
RUN pip install -U jedi epc flake8

RUN dnf install -y emacs

ADD . /emacs

# Link /emacs to ~/.emacs.d for instances in my config where I refer to ~/.emacs.d
RUN ln -s /emacs ~/.emacs.d

ENTRYPOINT ["emacs"]
