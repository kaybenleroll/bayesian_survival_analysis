# Project configuration
project_user    := "kaybenleroll"
project_name    := "bayesian_survival"
project_tag     := "latest"
image_tag       := project_user + "/" + project_name + ":" + project_tag
docker_user     := "rstudio"
docker_pass     := "CHANGEME"
docker_uid      := `id -u`
docker_gid      := `id -g`
rstudio_port    := "8787"
project_folder  := "bayessurv"
container_name  := "bayes-surv"
github_user     := `gh config get gh_user`

docker_build_args := ""

# List available recipes
default:
	@just --list

# Generate all HTML files from QMD files
all-html: 
	#!/usr/bin/env bash
	for qmd in *.qmd; do
		echo "TIMESTAMP: $(date) - Rendering script $qmd" >> output.log 2>&1
		quarto render "$qmd" --to html >> output.log 2>&1
		echo "TIMESTAMP: $(date) - Finished ${qmd%.qmd}.html" >> output.log 2>&1
	done


# Clean model files
clean-models:
	rm -fv stan_models/*

# Docker commands
docker-build:
	docker build -t {{image_tag}} \
		--build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
		{{docker_build_args}} \
		-f Dockerfile . 2>&1 | tee -a docker_build.log

# Show Docker build context
docker-show-context:
	docker build -f build/context.dockerfile -t context-image .
	docker run --rm -it context-image find /tmp/build
	docker rmi test:latest

# Run Docker container
docker-run:
	docker run --rm -d \
		-p {{rstudio_port}}:8787 \
		-e USER={{docker_user}} \
		-e PASSWORD={{docker_pass}} \
		-e USERID={{docker_uid}} \
		-e GROUPID={{docker_gid}} \
		-v "${PWD}:/home/{{docker_user}}/{{project_folder}}:rw" \
		--name {{container_name}} \
		{{image_tag}}

# Fix Docker container permissions
docker-fix-permissions:
	docker exec {{container_name}} bash -c "chown -R {{docker_user}}:{{docker_user}} /home/{{docker_user}}"

# Enter Docker container bash
docker-bash:
	docker exec -it -u {{docker_user}} {{container_name}} bash
