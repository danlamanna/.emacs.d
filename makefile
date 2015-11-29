HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs@git-$(HEAD) .

test: build
	docker run --rm --volume /root -it emacs@git-$(HEAD) --batch --load /emacs/init.el

clean:
	docker rmi --force emacs@git-$(HEAD)
