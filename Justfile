# =============================================================================
# PROJECT CONFIGURATION
# =============================================================================

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

DOCKER_BUILD_ARGS := ""

# =============================================================================
# DEFAULT TARGET
# =============================================================================

# List all available recipes
default:
	@just --list

# =============================================================================
# RENDERING TARGETS - Quarto Notebooks
# =============================================================================

# Render all notebooks
all: all-html

# Generate all HTML files from QMD files
all-html: \
	exploration_lifebook_data \
	classic_survival_models \
	initial_bayesian_survival \
	bayesian_survival_filtered_data \
	conditional_survival_prediction

# Internal helper: Check if rebuild needed based on content hash
_needs_rebuild output *sources:
	#!/usr/bin/env bash
	mkdir -p .just-cache
	# Combine all source files and compute hash
	combined_hash=$(cat {{sources}} 2>/dev/null | md5sum | cut -d' ' -f1)
	cache_file=".just-cache/$(basename {{output}}).md5"
	
	# Rebuild if: output missing, cache missing, or hash changed
	if [ ! -f {{output}} ] || \
	   [ ! -f "$cache_file" ] || \
	   [ "$combined_hash" != "$(cat $cache_file 2>/dev/null)" ]; then
		echo "$combined_hash" > "$cache_file"
		exit 0  # needs rebuild
	else
		echo "$(basename {{output}}) is up to date (no content changes)"
		exit 1  # skip rebuild
	fi

# Internal helper: Render a single QMD file with logging and hash-based caching
_render-qmd qmd_file *deps:
	#!/usr/bin/env bash
	html_file="${1%.qmd}.html"
	
	# Build list of all dependencies (qmd + library files)
	all_deps="{{qmd_file}} lib_utils.R lib_survival_modelling.R lib_stan_diagnostics.R lib_brms_hazard.R"
	
	# Check if rebuild needed
	if just _needs_rebuild "$html_file" $all_deps; then
		echo "TIMESTAMP: $(date) - Rendering script {{qmd_file}}" >> output.log 2>&1
		quarto render "{{qmd_file}}" --to html >> output.log 2>&1
		echo "TIMESTAMP: $(date) - Finished {{qmd_file}}" >> output.log 2>&1
	fi

# --- Data Exploration ---

# Render exploratory data analysis notebook (creates datasets)
exploration_lifebook_data:
	just _render-qmd exploration_lifebook_data.qmd

# Render exploratory data analysis notebook (alias)
explore: exploration_lifebook_data

# --- Classical Survival Models ---

# Render classical survival models notebook (Kaplan-Meier, Cox-PH)
classic_survival_models: exploration_lifebook_data
	just _render-qmd classic_survival_models.qmd

# Render classical survival models (alias)
classic: classic_survival_models

# --- Bayesian Survival Models ---

# Render initial Bayesian survival analysis notebook (small dataset)
initial_bayesian_survival: exploration_lifebook_data
	just _render-qmd initial_bayesian_survival.qmd

# Render initial Bayesian survival analysis (alias)
initial: initial_bayesian_survival

# Render filtered Bayesian survival analysis notebook (filtered small dataset)
bayesian_survival_filtered_data: exploration_lifebook_data
	just _render-qmd bayesian_survival_filtered_data.qmd

# Render filtered Bayesian survival analysis (alias)
filtered: bayesian_survival_filtered_data

# Render all Bayesian models
all-bayesian: initial_bayesian_survival bayesian_survival_filtered_data

# --- Advanced Topics ---

# Render conditional survival prediction notebook
conditional_survival_prediction: classic_survival_models exploration_lifebook_data
	just _render-qmd conditional_survival_prediction.qmd

# Render conditional survival prediction (alias)
conditional: conditional_survival_prediction

# =============================================================================
# CLEANING TARGETS
# =============================================================================

# Clean all generated files
clean-all: clean-html clean-models clean-cache clean-outputs

# Remove all HTML output files
clean-html:
	rm -fv *.html

# Remove Stan model files
clean-models:
	rm -fv stan_model/*

# Remove Quarto cache
clean-cache:
	rm -rf *_cache/
	rm -rf *_files/
	rm -rf .just-cache/

# Remove output logs
clean-outputs:
	rm -fv output.log

# Remove chunk timing files
clean-timing:
	rm -fv chunk_timings/*.parquet

# Clean Stan output CSVs
clean-stan-output:
	rm -fv stan_output/*.csv

# Nuclear option: remove all generated and cached files
nuke: clean-all clean-timing clean-stan-output
	@echo "All generated files removed"

# =============================================================================
# DOCKER TARGETS - Container Management
# =============================================================================

# Build Docker image
docker-build:
	docker build -t {{IMAGE_TAG}} \
		--build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
		{{DOCKER_BUILD_ARGS}} \
		-f Dockerfile . 2>&1 | tee -a docker_build.log

# Rebuild Docker image (no cache)
docker-rebuild:
	docker build --no-cache -t {{IMAGE_TAG}} \
		--build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
		{{DOCKER_BUILD_ARGS}} \
		-f Dockerfile . 2>&1 | tee -a docker_build.log

# Show Docker build context
docker-show-context:
	docker build -f build/context.dockerfile -t context-image .
	docker run --rm -it context-image find /tmp/build
	docker rmi context-image

# Run Docker container with RStudio Server
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

# Stop Docker container
docker-stop:
	docker stop {{CONTAINER_NAME}} || true

# Remove Docker container
docker-rm: docker-stop
	docker rm {{CONTAINER_NAME}} || true

# Restart Docker container
docker-restart: docker-stop docker-run

# Fix Docker container permissions
docker-fix-permissions:
	docker exec {{CONTAINER_NAME}} bash -c "chown -R {{DOCKER_USER}}:{{DOCKER_USER}} /home/{{DOCKER_USER}}"

# Enter Docker container bash shell
docker-bash:
	docker exec -u {{DOCKER_USER}} -it {{CONTAINER_NAME}} bash

# Enter Docker container as root
docker-bash-root:
	docker exec -u root -it {{CONTAINER_NAME}} bash

# Show Docker container logs
docker-logs:
	docker logs {{CONTAINER_NAME}}

# Show Docker container status
docker-status:
	docker ps -a --filter name={{CONTAINER_NAME}}

# Remove Docker image
docker-rmi:
	docker rmi {{IMAGE_TAG}} || true

# Complete Docker cleanup
docker-clean: docker-rm docker-rmi
	@echo "Docker container and image removed"

# =============================================================================
# DEVELOPMENT TARGETS
# =============================================================================

# Watch for changes and auto-render (requires entr)
watch notebook:
	ls {{notebook}}.qmd | entr -c just {{notebook}}

# Check Quarto installation
check-quarto:
	@quarto --version
	@quarto check

# Check R installation
check-r:
	@R --version

# Validate all QMD files without rendering
validate:
	@for file in *.qmd; do \
		echo "Validating $$file..."; \
		quarto render "$$file" --to html --execute false || exit 1; \
	done

# Show project info
info:
	@echo "Project: {{PROJECT_NAME}}"
	@echo "Docker Image: {{IMAGE_TAG}}"
	@echo "Container: {{CONTAINER_NAME}}"
	@echo "RStudio Port: {{RSTUDIO_PORT}}"
	@echo "Working Directory: {{PWD}}"

# =============================================================================
# DATA MANAGEMENT TARGETS
# =============================================================================

# List all parquet data files
list-data:
	@ls -lh data/*.parquet 2>/dev/null || echo "No parquet files found"

# Show data directory size
data-size:
	@du -sh data/ 2>/dev/null || echo "No data directory found"

# Backup data directory
backup-data:
	tar -czf data_backup_$(date +%Y%m%d_%H%M%S).tar.gz data/

# =============================================================================
# MODEL MANAGEMENT TARGETS
# =============================================================================

# List Stan model files
list-models:
	@echo "Fitted models:"
	@ls -lh stan_model/*.qs 2>/dev/null || echo "No .qs model files found"
	@echo ""
	@echo "Stan model definitions:"
	@ls -lh stan_code/*.stan 2>/dev/null || echo "No .stan files found"

# Show model directory sizes
models-size:
	@echo "stan_model directory:"
	@du -sh stan_model/ 2>/dev/null || echo "No stan_model directory"
	@echo ""
	@echo "fitted_models directory:"
	@du -sh fitted_models/ 2>/dev/null || echo "No fitted_models directory"

# Backup all model files
backup-models:
	tar -czf models_backup_$(date +%Y%m%d_%H%M%S).tar.gz stan_model/ fitted_models/ stan_code/ 2>/dev/null || true

# =============================================================================
# UTILITY TARGETS
# =============================================================================

# Show disk usage for all project directories
disk-usage:
	@echo "Project disk usage:"
	@du -sh data/ stan_model/ fitted_models/ chunk_timings/ 2>/dev/null | sort -h || echo "Some directories not found"

# Show all HTML output files
list-html:
	@ls -lh *.html 2>/dev/null || echo "No HTML files found"

# Show recent log entries
show-logs:
	@tail -n 50 output.log 2>/dev/null || echo "No output.log found"

# Show chunk timing files
list-timing:
	@ls -lh chunk_timings/*.parquet 2>/dev/null || echo "No timing files found"
