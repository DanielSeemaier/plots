init: docker-build install
docker-build:
	docker compose build
install:
	docker compose run --rm r ./install.R

example-pdf:
	docker compose run --rm r ./example_pdf.R

example-tex:
	docker compose run --rm r ./example_tex.R
