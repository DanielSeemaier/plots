init: docker-build install
docker-build:
	docker compose build
install:
	docker compose run --rm r ./install.R

pdf:
	docker compose run --rm r ./example_pdf.R

tex:
	docker compose run --rm r ./example_tex.R
