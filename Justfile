# Project configuration
PROJECT_USER    := "kaybenleroll"
PROJECT_NAME    := "bayesian_survival"
PROJECT_TAG     := "latest"
IMAGE_TAG       := PROJECT_USER + "/" + PROJECT_NAME + ":" + PROJECT_TAG
DOCKER_USER     := "rstudio"
DOCKER_PASS     := "CHANGEME"
DOCKER_UID      := `id -u`
DOCKER_GID      := `id -g`
PWD             := `pwd`
RSTUDIO_PORT    := "8787"
PROJECT_FOLDER  := "bayes_surv"
CONTAINER_NAME  := "bayes-surv"
GITHUB_USER     := `gh config get gh_user`

DOCKER_BUILD_ARGS := ""

# List available recipes
default:
	@just --list

# Generate all HTML files from QMD files
all-html: \
	exploration_lifebook_data \
	classic_survival_models \
	initial_bayesian_survival \
	conditional_survival_prediction

# Recipe template for rendering QMD files
_render-qmd qmd_file *deps:
	#!/usr/bin/env bash
	echo "TIMESTAMP: $(date) - Rendering script {{qmd_file}}" >> output.log 2>&1
	quarto render "{{qmd_file}}" --to html >> output.log 2>&1
	echo "TIMESTAMP: $(date) - Finished {{qmd_file}}" >> output.log 2>&1


# Individual QMD file rules with proper dependencies

# Render exploratory data analysis notebook
exploration_lifebook_data:
	just _render-qmd exploration_lifebook_data.qmd

# Render classical survival models notebook (depends on exploration)
classic_survival_models: exploration_lifebook_data
	just _render-qmd classic_survival_models.qmd

# Render initial Bayesian survival analysis notebook
initial_bayesian_survival:
	just _render-qmd initial_bayesian_survival.qmd

# Render conditional survival prediction notebook (depends on classic models and exploration)
conditional_survival_prediction: classic_survival_models exploration_lifebook_data
	just _render-qmd conditional_survival_prediction.qmd



# Clean model files
clean-models:
	rm -fv stan_models/*

# Docker commands
docker-build-image:
	docker build -t {{IMAGE_TAG}} \
		--build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
		{{DOCKER_BUILD_ARGS}} \
		-f Dockerfile . 2>&1 | tee -a docker_build.log

# Show Docker build context
docker-show-context:
	docker build -f build/context.dockerfile -t context-image .
	docker run --rm -it context-image find /tmp/build
	docker rmi test:latest

# Run Docker container
docker-run:
	docker run --rm -d \
	  --userns=keep-id \
	  -e RUNROOTLESS=false \
	  -p "127.0.0.1:{{RSTUDIO_PORT}}:8787" \
	  -e USER={{DOCKER_USER}} \
	  -e PASSWORD={{DOCKER_PASS}} \
	  -e USERID={{DOCKER_UID}} \
	  -e GROUPID={{DOCKER_GID}} \
	  -v "{{PWD}}:/home/rstudio/{{PROJECT_FOLDER}}:z" \
	  -v "{{PWD}}/.rstudio_copilot:/home/rstudio/.config/github-copilot:rw" \
	  --name {{CONTAINER_NAME}} \
	  {{IMAGE_TAG}}

# Fix Docker container permissions
docker-fix-permissions:
	docker exec {{CONTAINER_NAME}} bash -c "chown -R {{DOCKER_USER}}:{{DOCKER_USER}} /home/{{DOCKER_USER}}"

# Enter Docker container bash
docker-bash:
	docker exec -it {{CONTAINER_NAME}} bash
