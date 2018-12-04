CWD     := $(shell pwd)
RANDOMPATH := $(CWD)
STACK   := stack
BUILD   := $(STACK) build
COMMIT  := $(RANDOMPATH)/commit.hs
RANDOM  := $(STACK) exec Main

GIT       := git
GITDIFF   := $(GIT) diff
GITLOG    := $(GIT) log
GITSTATUS := $(GIT) status
GITPULL   := $(GIT) pull

build:
	$(BUILD)
	$(RANDOM)

commit:
	$(COMMIT)

random:
	$(BUILD)
	$(RANDOM)

diff:
	$(GITDIFF)

log:
	$(GITLOG)

pull:
	$(GITPULL)

run:
	@echo "stack and haskell is used with random"

status:
	$(GITSTATUS)

upgrade:
	$(STACK) upgrade

version:
	$(STACK) --version
