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

.PHONY: all
all: build

.PHONY: help
help:
	@echo "Targets (builddirs: $(BUILDDIR_RELEASE), $(BUILDDIR_DEBUG), … — not dist-newstyle):"
	@echo "  build / build-debug"
	@echo "  test-run        (generate Sch.hs, build tests, run tests)"
	@echo "  test-run-debug  (same with pg-schema +debug)"
	@echo "  tutorial-run    (generate app/Sch.hs, build, run pgs-tutorial)"
	@echo "  tutorial-run-debug"
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

.PHONY: tutorial-run
tutorial-run:
	$(CABAL_REL) run pg-schema-tutorial:pgs-tutorial-generator
	$(CABAL_REL) build pg-schema-tutorial:pgs-tutorial
	$(CABAL_REL) run pg-schema-tutorial:pgs-tutorial

.PHONY: tutorial-run-debug
tutorial-run-debug:
	$(CABAL_DBG) run pg-schema-tutorial:pgs-tutorial-generator $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) build pg-schema-tutorial:pgs-tutorial $(PG_SCHEMA_DEBUG)
	$(CABAL_DBG) run pg-schema-tutorial:pgs-tutorial $(PG_SCHEMA_DEBUG)

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
