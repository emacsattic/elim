############################################################################
# make macro functions:
INSTALLED   = $(if $(shell pkg-config $(1) --exists || echo t),\
                   $(error $(1) package not installed))
############################################################################
# package dependencies:
PACKAGES    := glib-2.0 libxml-2.0 purple libxml-2.0
$(foreach P, $(PACKAGES), $(call INSTALLED, $P))
############################################################################
DEFINES     := -D_GNU_SOURCE \
               $(shell                                                      \
                 if pkg-config glib-2.0 --atleast-version 2.14 2>/dev/null; \
                  then                                                      \
                      echo '-D';                                            \
                  else                                                      \
                      echo '-U';                                            \
                  fi;)GLIB_HAS_ADD_SECONDS

CFLAGS      += -Wall -std=c99 $(DEFINES)
CFLAGS      += $(foreach P, $(PACKAGES), $(shell pkg-config --cflags $P)) 
LDFLAGS     += $(foreach P, $(PACKAGES), $(shell pkg-config --libs   $P)) 
TVER        := $(shell etags --version | head -n 1 | grep -i exuberant)
BINARIES    := elim-client test/sexp-test
CH_FILES    := $(wildcard *.c         ) \
               $(wildcard prpl/*.c    ) \
               $(wildcard xnode/*.c   ) \
               $(wildcard sexp/*.c    ) \
               $(wildcard ui_ops/*.c  ) \
               $(wildcard handlers/*.c)
HANDLER_SRC := $(wildcard handlers/*.c)
HANDLER_OBJ := $(patsubst %.c, %.o, $(HANDLER_SRC) )
OBJ_FILES   := $(patsubst %.c, %.o, $(CH_FILES) )
UTIL_OBJ    := sexp/sexp-xml.o xnode/xnode.o
CLIENT_OBJ  := $(patsubst %.c, %.o, $(wildcard handlers/*.c))

############################################################################
.PHONY: clean diag distclean

all: $(BINARIES)

############################################################################
# test scripts/utils etc, such as there are:
test/sexp-test: test/sexp-test.o test/sexp-example.h $(UTIL_OBJ)

test/sexp-test.o: test/sexp-test.c test/sexp-example.h

############################################################################
# object files and dependencies thereof:
elim-client: $(OBJ_FILES) elim-func-handlers.o

elim-client.o: handler-list.h elim-client-queue.h elim-func-handlers.h

$(OBJ_FILES): %.o: %.c %.h

$(HANDLER_OBJ): ui_ops/ops.h prpl/util.h elim-rpc.h

############################################################################
# generated source files:
elim-func-handlers.c: make/elim-func-handlers-c.sh $(HANDLER_SRC) 
	$< $(filter-out %.sh, $^) > $@;

ui_ops/ops.h: make/elim-ops-h.sh $(patsubst %.c, %.h, $(wildcard ui_ops/*.c))
	$< $(filter-out %.sh, $^) > $@;

handler-list.h: make/handler-list-h.sh $(wildcard handlers/*.h)
	$< $(filter-out %.sh, $^) > $@;

############################################################################
diag:
	@echo "packages : "$(PACKAGES)
	@echo "CFLAGS   : "$(CFLAGS)
	@echo "LDFLAGS  : "$(LDFLAGS)
	@echo "CH_FILES : "$(CH_FILES)
	@echo "OBJ_FILES: "$(OBJ_FILES)

clean:
	@( rm -fv $(BINARIES) $(OBJ_FILES) \
	          handler-list.h           \
	          test/sexp-test.o         \
	          ui_ops/ops.h             \
	          elim-func-handlers.c    );

TAGS: $(CH_FILES)
	@if [ x"$(TVER)" != x ]; then etags --recurse; fi

distclean: clean
	@find . -type f -name *~ -exec rm {} \;
