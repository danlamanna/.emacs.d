HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs@git-$(HEAD) .

test-build: build
	docker run --rm --volume /root -it emacs@git-$(HEAD) --batch --load /emacs/init.el

test: build
	docker run --rm --volume /root -it emacs@git-$(HEAD) --batch \
		--load ert \
		--load /emacs/init.el \
		--load /emacs/tests.el \
		--funcall ert-run-tests-batch-and-exit

test-env: build
	docker run --rm --volume /root -it emacs@git-$(HEAD) --load /emacs/init.el

clean:
	docker rmi --force emacs@git-$(HEAD)
