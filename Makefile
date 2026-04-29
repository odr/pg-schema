# Makefile for pg-schema (run from repo root: directory with cabal.project).
#
# Build directories: most targets use their own --builddir so dist-newstyle
# stays stable for HLS. Run `make local-docs` when you want Haddock there.
#
# Hackage (cabal.project.hackage — no +arbitrary/+flat/+hashable; matches .cabal defaults):
#   hackage-docs  — only pg-schema; not pg-schema-tutorial.
#   hackage-sdist — source tarball of pg-schema only.
#   hackage-upload-* — upload tarballs from hackage-sdist / hackage-docs only (no rebuild).
#     *-candidate = release candidate; *-publish = final publish (--publish).
#     Auth: ~/.config/cabal/config (token or user/pass).
#
# test-run needs PostgreSQL for generation (override PG_CONN in test-gen).
# Tutorial generator uses a fixed DSN in generator/Main.hs unless you change it.

CABAL          ?= cabal
PKG            := pg-schema
VERSION        := $(shell sed -n 's/^version:[[:space:]]*//p' pg-schema/pg-schema.cabal | head -n1)

# same effect as enabling the `debug` flag on pg-schema (trace queries via -DDEBUG)
PG_SCHEMA_DEBUG = -c 'pg-schema +debug'

# isolate from dist-newstyle (IDE / HLS), except local-docs
BUILDDIR_IDE     := dist-newstyle
BUILDDIR_RELEASE := dist-make
BUILDDIR_DEBUG   := dist-make-debug
BUILDDIR_HADDOCK := dist-haddock-hackage
BUILDDIR_SDIST   := dist-make-sdist
OUT_SDIST        := dist-sdist

HACKAGE_SDIST_TAR := $(OUT_SDIST)/$(PKG)-$(VERSION).tar.gz
HACKAGE_DOCS_TAR  := $(BUILDDIR_HADDOCK)/$(PKG)-$(VERSION)-docs.tar.gz

PROJECT_HACKAGE  := --project-file=cabal.project.hackage

CABAL_REL := $(CABAL) --builddir=$(BUILDDIR_RELEASE)
CABAL_DBG := $(CABAL) --builddir=$(BUILDDIR_DEBUG)
CABAL_HDK := $(CABAL) --builddir=$(BUILDDIR_HADDOCK) $(PROJECT_HACKAGE)
CABAL_SDT := $(CABAL) --builddir=$(BUILDDIR_SDIST) $(PROJECT_HACKAGE)

# --- new tutorial package (tutorial/app) ---
TUTORIAL_PKG          := tutorial
TUTORIAL_GEN_EXE      := $(TUTORIAL_PKG):generator
TUTORIAL_APP_EXE      := $(TUTORIAL_PKG):tutorial
TUTORIAL_DB           ?= tutorial
TUTORIAL_SQL_SCHEMA   := tutorial/sql/00-create-schema.sql
TUTORIAL_GEN_MAIN     := tutorial/app/generator/Main.hs
TUTORIAL_GEN_SCH      := tutorial/app/app/Sch.hs
DOCS_ROOT             := tutorial/docs
DOCS_SOURCE_ROOT      := $(DOCS_ROOT)/source
DOCS_SOURCE_SQL       := $(DOCS_SOURCE_ROOT)/sql
DOCS_SOURCE_APP       := $(DOCS_SOURCE_ROOT)/app
DOCS_SQL_MD           := $(DOCS_SOURCE_SQL)/00-create-schema.md
DOCS_SCH_MD           := $(DOCS_SOURCE_APP)/Sch.md
DOCS_GEN_MAIN_MD      := $(DOCS_SOURCE_APP)/generator-Main.md
DOCS_VENV_DIR         := tutorial/.venv
MKDOCS_BIN            := $(DOCS_VENV_DIR)/bin/mkdocs

.PHONY: all
all: build

.PHONY: help
help:
	@echo "Targets (builddirs: $(BUILDDIR_RELEASE), $(BUILDDIR_DEBUG), … — not dist-newstyle):"
	@echo "  build / build-debug"
	@echo "  test-run        (generate Sch.hs, build tests, run tests)"
	@echo "  test-run-debug  (same with pg-schema +debug)"
	@echo "  old-tutorial-run    (generate app/Sch.hs, build, run pgs-tutorial)"
	@echo "  old-tutorial-run-debug"
	@echo "  tut-from-scratch   (db + schema + generator + app + docs sync + docs build)"
	@echo "  tut-db-init        (create DB if needed, apply tutorial schema)"
	@echo "  tut-run            (run tutorial generator and app executables)"
	@echo "  tut-sync-docs-source (generate docs/source/*.md wrappers from source files)"
	@echo "  tut-docs-venv      (create tutorial/.venv and install mkdocs deps)"
	@echo "  tut-docs-build     (mkdocs build via tutorial/.venv)"
	@echo "  tut-docs-serve     (mkdocs serve via tutorial/.venv)"
	@echo "  local-docs      Haddock $(PKG) -> $(BUILDDIR_IDE) (cabal.project; for HLS hover)"
	@echo "  hackage-docs  -> $(HACKAGE_DOCS_TAR)"
	@echo "                  (pg-schema only; cabal.project.hackage = default package flags)"
	@echo "  hackage-sdist -> $(HACKAGE_SDIST_TAR)  (same project file)"
	@echo "  hackage-upload-candidate       (upload hackage-sdist tarball → candidate)"
	@echo "  hackage-upload-docs-candidate  (upload hackage-docs tarball → candidate)"
	@echo "  hackage-upload-publish         (same sdist tarball → *published*)"
	@echo "  hackage-upload-docs-publish    (same docs tarball → published package)"

# --- library / project build ---

.PHONY: build
build:
	$(CABAL_REL) build $(PKG)

.PHONY: build-debug
build-debug:
	$(CABAL_DBG) build $(PKG) $(PG_SCHEMA_DEBUG)

# --- tests: generate test-pgs/Sch.hs, build suites, run ---

.PHONY: test-run
test-run:
	$(CABAL_REL) test $(PKG):test-gen
	$(CABAL_REL) build $(PKG):json-spec $(PKG):test-pgs $(PKG):test-gen
	$(CABAL_REL) test $(PKG)

.PHONY: test-run-debug
test-run-debug:
	$(CABAL_DBG) test $(PKG):test-gen $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) build $(PKG):json-spec $(PKG):test-pgs $(PKG):test-gen $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) test $(PKG) $(PG_SCHEMA_DEBUG)

# --- tutorial: generate app/Sch.hs, build, run ---

.PHONY: old-tutorial-run
old-tutorial-run:
	$(CABAL_REL) run pg-schema-tutorial:pgs-tutorial-generator
	$(CABAL_REL) build pg-schema-tutorial:pgs-tutorial
	$(CABAL_REL) run pg-schema-tutorial:pgs-tutorial

.PHONY: old-tutorial-run-debug
old-tutorial-run-debug:
	$(CABAL_DBG) run pg-schema-tutorial:pgs-tutorial-generator $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) build pg-schema-tutorial:pgs-tutorial $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) run pg-schema-tutorial:pgs-tutorial $(PG_SCHEMA_DEBUG)

# --- new tutorial flow ---

.PHONY: tut-db-init
tut-db-init:
	@psql -Atqc "SELECT 1 FROM pg_database WHERE datname='$(TUTORIAL_DB)'" postgres | grep -q 1 \
		|| createdb "$(TUTORIAL_DB)"
	psql "dbname=$(TUTORIAL_DB)" -f "$(TUTORIAL_SQL_SCHEMA)"

.PHONY: tut-run
tut-run:
	$(CABAL_REL) run $(TUTORIAL_GEN_EXE)
	$(CABAL_REL) run $(TUTORIAL_APP_EXE)

.PHONY: tut-sync-docs-source
tut-sync-docs-source:
	mkdir -p "$(DOCS_SOURCE_SQL)" "$(DOCS_SOURCE_APP)"
	rm -f "$(DOCS_SOURCE_SQL)"/*.sql "$(DOCS_SOURCE_APP)"/*.hs
	@{ \
		printf '%s\n\n' '# SQL schema: `00-create-schema.sql`'; \
		printf '%s\n\n' 'Source path: `$(TUTORIAL_SQL_SCHEMA)`'; \
		printf '%s\n' '```sql'; \
		cat "$(TUTORIAL_SQL_SCHEMA)"; \
		printf '\n%s\n' '```'; \
	} > "$(DOCS_SQL_MD)"
	@test -f "$(TUTORIAL_GEN_SCH)" || { printf '%s\n' "Missing $(TUTORIAL_GEN_SCH) — run: $(MAKE) tut-run"; exit 1; }
	@{ \
		printf '%s\n\n' '# Haskell schema module: `Sch.hs`'; \
		printf '%s\n\n' 'Source path: `$(TUTORIAL_GEN_SCH)`'; \
		printf '%s\n' '```haskell'; \
		cat "$(TUTORIAL_GEN_SCH)"; \
		printf '\n%s\n' '```'; \
	} > "$(DOCS_SCH_MD)"
	@{ \
		printf '%s\n\n' '# Tutorial generator: `Main.hs`'; \
		printf '%s\n\n' 'Source path: `$(TUTORIAL_GEN_MAIN)`'; \
		printf '%s\n' '```haskell'; \
		cat "$(TUTORIAL_GEN_MAIN)"; \
		printf '\n%s\n' '```'; \
	} > "$(DOCS_GEN_MAIN_MD)"

.PHONY: tut-docs-build
tut-docs-build: tut-docs-venv
	cd tutorial && ../$(MKDOCS_BIN) build

.PHONY: tut-docs-serve
tut-docs-serve: tut-docs-venv
	cd tutorial && ../$(MKDOCS_BIN) serve

.PHONY: tut-docs-venv
tut-docs-venv:
	@test -x "$(MKDOCS_BIN)" || { \
		python3 -m venv "$(DOCS_VENV_DIR)"; \
		"$(DOCS_VENV_DIR)/bin/pip" install mkdocs mkdocs-material pymdown-extensions; \
	}

.PHONY: tut-from-scratch
tut-from-scratch: tut-db-init tut-run tut-sync-docs-source tut-docs-build

# --- Haddock for HLS (default project + dist-newstyle; on demand) ---

.PHONY: local-docs
local-docs:
	$(CABAL) --builddir=$(BUILDDIR_IDE) haddock $(PKG)

# --- Hackage ---

.PHONY: hackage-docs
hackage-docs:
	$(CABAL_HDK) haddock --haddock-for-hackage $(PKG)
	@echo "Documentation tarball: $(HACKAGE_DOCS_TAR)"

.PHONY: hackage-sdist
hackage-sdist:
	mkdir -p $(OUT_SDIST)
	$(CABAL_SDT) sdist $(PKG) --output-directory=$(OUT_SDIST)
	@echo "Source tarball: $(HACKAGE_SDIST_TAR)"

# Upload only existing files. Candidate targets omit --publish; publish targets use --publish.

.PHONY: hackage-upload-candidate
hackage-upload-candidate:
	@test -f "$(HACKAGE_SDIST_TAR)" || { printf '%s\n' "Missing $(HACKAGE_SDIST_TAR) — run: $(MAKE) hackage-sdist"; exit 1; }
	@printf 'Upload Hackage source *candidate* (not published):\n  %s\nOK? [y/N] ' "$(HACKAGE_SDIST_TAR)" && read ans && case "$$ans" in y|Y|yes|YES) ;; *) echo 'Aborted.'; exit 1;; esac
	$(CABAL) upload "$(HACKAGE_SDIST_TAR)"

.PHONY: hackage-upload-docs-candidate
hackage-upload-docs-candidate:
	@test -f "$(HACKAGE_DOCS_TAR)" || { printf '%s\n' "Missing $(HACKAGE_DOCS_TAR) — run: $(MAKE) hackage-docs"; exit 1; }
	@printf 'Upload Hackage documentation for *candidate* (not published):\n  %s\nOK? [y/N] ' "$(HACKAGE_DOCS_TAR)" && read ans && case "$$ans" in y|Y|yes|YES) ;; *) echo 'Aborted.'; exit 1;; esac
	$(CABAL) upload --documentation "$(HACKAGE_DOCS_TAR)"

.PHONY: hackage-upload-publish
hackage-upload-publish:
	@test -f "$(HACKAGE_SDIST_TAR)" || { printf '%s\n' "Missing $(HACKAGE_SDIST_TAR) — run: $(MAKE) hackage-sdist"; exit 1; }
	@printf '*** PUBLISH *** Hackage source (final, not candidate):\n  %s\nOK? [y/N] ' "$(HACKAGE_SDIST_TAR)" && read ans && case "$$ans" in y|Y|yes|YES) ;; *) echo 'Aborted.'; exit 1;; esac
	$(CABAL) upload --publish "$(HACKAGE_SDIST_TAR)"

.PHONY: hackage-upload-docs-publish
hackage-upload-docs-publish:
	@test -f "$(HACKAGE_DOCS_TAR)" || { printf '%s\n' "Missing $(HACKAGE_DOCS_TAR) — run: $(MAKE) hackage-docs"; exit 1; }
	@printf '*** PUBLISH *** Hackage documentation for published package:\n  %s\nOK? [y/N] ' "$(HACKAGE_DOCS_TAR)" && read ans && case "$$ans" in y|Y|yes|YES) ;; *) echo 'Aborted.'; exit 1;; esac
	$(CABAL) upload --documentation --publish "$(HACKAGE_DOCS_TAR)"
