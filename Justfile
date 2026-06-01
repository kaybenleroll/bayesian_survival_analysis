# =============================================================================
# BAYESIAN SURVIVAL ANALYSIS - BUILD SYSTEM
# =============================================================================
#
# OVERVIEW:
# This Justfile manages the rendering pipeline for a series of interconnected
# Quarto notebooks. It supports both local execution and containerized execution
# via Podman to ensure absolute reproducibility.
#
# DEPENDENCY MANAGEMENT & "MAKE-LIKE" BEHAVIOR:
# This project uses a directed acyclic graph (DAG) to manage dependencies between
# notebooks (e.g., Model B requires Data from Notebook A).
# 
# We implement this using Content Hashing (MD5) rather than traditional file
# timestamps (mtime). 
# 
# Why Content Hashing over Timestamps?
# 1. Git-Safety: Switching branches or pulling code updates file timestamps to 
#    "now". Traditional `make` would trigger a massive, unnecessary rebuild. 
#    Content hashing only rebuilds if the actual text/data changed.
# 2. Precision: Saving a file without modifying it (or touching it) won't 
#    trigger false-positive rebuilds.
#
# How it works:
# The `_needs_rebuild` helper computes a combined MD5 hash of:
#   1. The target `.qmd` file.
#   2. Core shared R libraries (`lib_*.R`).
#   3. ANY upstream artifacts specified in `*extra_deps` (like the output .html
#      or .parquet files of a preceding notebook).
# If this combined hash differs from the one stored in `.just-cache/`, a rebuild
# is triggered. Downstream notebooks automatically rebuild when upstream outputs change.
#
# =============================================================================
# PROJECT CONFIGURATION
# =============================================================================

PROJECT_USER    := "kaybenleroll"
PROJECT_NAME    := "bayesian_survival"
PROJECT_TAG     := "latest"
IMAGE_TAG       := PROJECT_USER + "/" + PROJECT_NAME + ":" + PROJECT_TAG
CONTAINER_USER  := "rstudio"
CONTAINER_PASS  := "bayes"
CONTAINER_UID   := `id -u`
CONTAINER_GID   := `id -g`
PWD             := `pwd`
RSTUDIO_PORT    := "8787"
PROJECT_FOLDER  := "bayes_surv"
CONTAINER_NAME  := "bayes-surv"

PODMAN_BUILD_ARGS := ""

# =============================================================================
# DEFAULT TARGET
# =============================================================================

# List all available recipes
default:
  @just --list

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

# Internal helper: Check if rebuild needed based on content hash
# This provides Make-like dependency management but uses MD5 checksums instead of 
# timestamps. This makes the pipeline "Git-safe" (switching branches won't 
# trigger a rebuild unless the content actually changed).
_needs_rebuild output *sources:
  #!/usr/bin/env bash
  mkdir -p .just-cache
  # Combine all source files and compute a single hash. If an upstream 
  # dependency is an .html file, its content changes will trigger a rebuild here.
  combined_hash=$(cat {{sources}} 2>/dev/null | md5sum | cut -d' ' -f1)
  cache_file=".just-cache/$(basename {{output}}).md5"
  
  if [ ! -f {{output}} ] || \
     [ ! -f "$cache_file" ] || \
     [ "$combined_hash" != "$(cat $cache_file 2>/dev/null)" ]; then
    echo "$combined_hash" > "$cache_file"
    exit 0  # Hash changed or output missing: needs rebuild
  else
    echo "$(basename {{output}}) is up to date (no content changes)."
    exit 1  # Hash matches: skip rebuild
  fi

# Internal helper: Render a single QMD file
# This is the core rendering engine. It accepts a list of *extra_deps which allows
# us to build a directed acyclic graph (DAG) of notebooks. If notebook B 
# depends on the HTML output of notebook A, we list B's extra_deps as A.html.
_render-qmd qmd_file mode="local" *extra_deps="":
  #!/usr/bin/env bash
  qmd="{{qmd_file}}"
  html_file="${qmd%.qmd}.html"
  # Dependencies include the script itself, core R libraries, and any specified
  # upstream artifacts (like .html or .parquet files).
  all_deps="{{qmd_file}} lib_utils.R lib_survival_modelling.R lib_stan_diagnostics.R lib_brms_hazard.R {{extra_deps}}"
  
  if just _needs_rebuild "$html_file" $all_deps; then
    echo "TIMESTAMP: $(date) - Rendering script {{qmd_file}} (Mode: {{mode}})" >> output.log 2>&1
    if [ "{{mode}}" = "podman" ]; then
      echo "Rendering {{qmd_file}} in podman..."
      podman exec -w /home/{{CONTAINER_USER}}/{{PROJECT_FOLDER}} {{CONTAINER_NAME}} quarto render "{{qmd_file}}" --to html >> output.log 2>&1
    else
      echo "Rendering {{qmd_file}} locally..."
      quarto render "{{qmd_file}}" --to html >> output.log 2>&1
    fi
    echo "TIMESTAMP: $(date) - Finished {{qmd_file}}" >> output.log 2>&1
  fi

# =============================================================================
# LOCAL RENDERING TARGETS (Host Machine)
# =============================================================================

# Render a specific QMD file locally without dependencies
render qmd_file:
  just _render-qmd {{qmd_file}} local

# Generate all HTML files from QMD files on host
all-html: \
  exploration_lifebook_data \
  classic_survival_models \
  initial_bayesian_survival \
  bayesian_survival_filtered_data \
  conditional_survival_prediction \
  bayesian_survival_talk

# Render all Bayesian models on host
all-bayesian: initial_bayesian_survival bayesian_survival_filtered_data

# Render exploratory data analysis notebook locally
exploration_lifebook_data:
  just _render-qmd exploration_lifebook_data.qmd local

# Render presentation slides locally
bayesian_survival_talk:
  just _render-qmd bayesian_survival_talk.qmd local

# Render exploratory data analysis notebook locally (alias)
explore: exploration_lifebook_data

# Render classical survival models notebook locally
classic_survival_models: exploration_lifebook_data
  just _render-qmd classic_survival_models.qmd local "exploration_lifebook_data.html"

# Render classical survival models locally (alias)
classic: classic_survival_models

# Render initial Bayesian survival analysis notebook locally
initial_bayesian_survival: exploration_lifebook_data
  just _render-qmd initial_bayesian_survival.qmd local "exploration_lifebook_data.html"

# Render initial Bayesian survival analysis locally (alias)
initial: initial_bayesian_survival

# Render filtered Bayesian survival analysis notebook locally
bayesian_survival_filtered_data: exploration_lifebook_data
  just _render-qmd bayesian_survival_filtered_data.qmd local "exploration_lifebook_data.html"

# Render filtered Bayesian survival analysis locally (alias)
filtered: bayesian_survival_filtered_data

# Render conditional survival prediction notebook locally
conditional_survival_prediction: classic_survival_models exploration_lifebook_data
  just _render-qmd conditional_survival_prediction.qmd local "classic_survival_models.html exploration_lifebook_data.html"

# Render conditional survival prediction locally (alias)
conditional: conditional_survival_prediction

# =============================================================================
# PODMAN RENDERING TARGETS (Container)
# =============================================================================

# Render a specific QMD file in podman without dependencies
podman-render qmd_file:
  just _render-qmd {{qmd_file}} podman

# Generate all HTML files from QMD files in podman
podman-all-html: \
  podman-exploration_lifebook_data \
  podman-classic_survival_models \
  podman-initial_bayesian_survival \
  podman-bayesian_survival_filtered_data \
  podman-conditional_survival_prediction \
  podman-bayesian_survival_talk

# Render all Bayesian models in podman
podman-all-bayesian: podman-initial_bayesian_survival podman-bayesian_survival_filtered_data

# Render exploratory data analysis notebook in podman
podman-exploration_lifebook_data:
  just _render-qmd exploration_lifebook_data.qmd podman

# Render presentation slides in podman
podman-bayesian_survival_talk:
  just _render-qmd bayesian_survival_talk.qmd podman

# Render classical survival models notebook in podman
podman-classic_survival_models: podman-exploration_lifebook_data
  just _render-qmd classic_survival_models.qmd podman "exploration_lifebook_data.html"

# Render initial Bayesian survival analysis notebook in podman
podman-initial_bayesian_survival: podman-exploration_lifebook_data
  just _render-qmd initial_bayesian_survival.qmd podman "exploration_lifebook_data.html"

# Render filtered Bayesian survival analysis notebook in podman
podman-bayesian_survival_filtered_data: podman-exploration_lifebook_data
  just _render-qmd bayesian_survival_filtered_data.qmd podman "exploration_lifebook_data.html"

# Render conditional survival prediction notebook in podman
podman-conditional_survival_prediction: podman-classic_survival_models podman-exploration_lifebook_data
  just _render-qmd conditional_survival_prediction.qmd podman "classic_survival_models.html exploration_lifebook_data.html"

# =============================================================================
# CLEANING TARGETS
# =============================================================================

# Clean all generated files
clean-all: clean-html clean-models clean-predictions clean-cache clean-outputs

# Remove all production HTML output files (excludes temp_ and tmp_ files)
clean-html:
  @find . -maxdepth 1 -name "*.html" ! -name "temp_*" ! -name "tmp_*" -exec rm -fv {} +

# Remove production Stan model files (excludes temp_ and tmp_ files)
clean-models:
  @find stan_model/ -maxdepth 1 ! -name "temp_*" ! -name "tmp_*" ! -name ".gitignore" -exec rm -fv {} +

# Remove production prediction files (excludes temp_ and tmp_ files)
clean-predictions:
  @find predictions/ -maxdepth 1 ! -name "temp_*" ! -name "tmp_*" ! -name ".gitignore" -exec rm -fv {} +

# Remove production Quarto cache (excludes temp_ and tmp_ files)
clean-cache:
  @find . -maxdepth 1 \( -name "*_cache" -o -name "*_files" \) ! -name "temp_*" ! -name "tmp_*" -exec rm -rfv {} +
  rm -rf .just-cache/

# Remove output logs
clean-outputs:
  rm -fv output.log

# Remove production chunk timing files (excludes temp_ and tmp_ files)
clean-timing:
  @find chunk_timings/ -maxdepth 1 -name "*.parquet" ! -name "temp_*" ! -name "tmp_*" -exec rm -fv {} +

# Clean production Stan output CSVs (excludes temp_ and tmp_ files)
clean-stan-output:
  @find stan_output/ -maxdepth 1 -name "*.csv" ! -name "temp_*" ! -name "tmp_*" -exec rm -fv {} +

# Nuclear option: remove all generated and cached files
nuke: clean-all clean-timing clean-stan-output
  @echo "All generated files removed"

# =============================================================================
# PODMAN MANAGEMENT TARGETS
# =============================================================================

# Build Podman image
podman-build-image:
  podman build -t {{IMAGE_TAG}} \
    --build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
    {{PODMAN_BUILD_ARGS}} \
    -f Dockerfile . 2>&1 | tee -a podman_build.log

# Rebuild Podman image (no cache)
podman-rebuild-image:
  podman build --no-cache -t {{IMAGE_TAG}} \
    --build-arg BUILD_DATE=`date -u +'%Y-%m-%dT%H:%M:%SZ'` \
    {{PODMAN_BUILD_ARGS}} \
    -f Dockerfile . 2>&1 | tee -a podman_build.log

# Show Podman build context
podman-show-context:
  podman build -f build/context.dockerfile -t context-image .
  podman run --rm -it context-image find /tmp/build
  podman rmi context-image

# Run Podman container with RStudio Server
podman-run-image:
  podman run --rm -d \
    --userns=keep-id \
    -e RUNROOTLESS=false \
    -p "127.0.0.1:{{RSTUDIO_PORT}}:8787" \
    -e USER={{CONTAINER_USER}} \
    -e PASSWORD={{CONTAINER_PASS}} \
    -e USERID={{CONTAINER_UID}} \
    -e GROUPID={{CONTAINER_GID}} \
    -v "{{PWD}}:/home/rstudio/{{PROJECT_FOLDER}}:z" \
    -v "{{PWD}}/.rstudio_copilot:/home/rstudio/.config/github-copilot:rw" \
    --name {{CONTAINER_NAME}} \
    {{IMAGE_TAG}}

# Stop Podman container
podman-stop-image:
  podman stop {{CONTAINER_NAME}} || true

# Remove Podman container
podman-rm: podman-stop-image
  podman rm {{CONTAINER_NAME}} || true

# Restart Podman container
podman-restart: podman-stop-image podman-run-image

# Fix Podman container permissions
podman-fix-permissions:
  podman exec {{CONTAINER_NAME}} bash -c "chown -R {{CONTAINER_USER}}:{{CONTAINER_USER}} /home/{{CONTAINER_USER}}"

# Enter Podman container bash shell
podman-bash:
  podman exec -u {{CONTAINER_USER}} -it {{CONTAINER_NAME}} bash

# Enter Podman container as root
podman-bash-root:
  podman exec -u root -it {{CONTAINER_NAME}} bash

# Show Podman container logs
podman-logs:
  podman logs {{CONTAINER_NAME}}

# Show Podman container status
podman-status:
  podman ps -a --filter name={{CONTAINER_NAME}}

# Remove Podman image
podman-rmi:
  podman rmi {{IMAGE_TAG}} || true

# Complete Podman cleanup
podman-clean: podman-rm podman-rmi
  @echo "Podman container and image removed"

# =============================================================================
# DEVELOPMENT TARGETS
# =============================================================================

# Watch for changes and auto-render locally (requires entr)
watch notebook:
  ls {{notebook}}.qmd | entr -c just {{notebook}}

# Watch for changes and auto-render in podman (requires entr)
podman-watch notebook:
  ls {{notebook}}.qmd | entr -c just podman-{{notebook}}

# Check Quarto installation
check-quarto:
  @quarto --version
  @quarto check

# Check R installation
check-r:
  @R --version

# Validate all production QMD files without rendering (excludes temp_ and tmp_ files)
validate:
  @for file in *.qmd; do \
    case "$$file" in \
      temp_*|tmp_*) \
        continue ;; \
      *) \
        echo "Validating $$file..."; \
        quarto render "$$file" --to html --execute false || exit 1; \
        ;; \
    esac \
  done

# Show project info
info:
  @echo "Project: {{PROJECT_NAME}}"
  @echo "Podman Image: {{IMAGE_TAG}}"
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

# Show all HTML output files (excludes temp_ and tmp_ files)
list-html:
  @ls -lh *.html 2>/dev/null | grep -vE 'temp_|tmp_' || echo "No production HTML files found"

# Show recent log entries
show-logs:
  @tail -n 50 output.log 2>/dev/null || echo "No output.log found"

# Show chunk timing files
list-timing:
  @ls -lh chunk_timings/*.parquet 2>/dev/null || echo "No timing files found"