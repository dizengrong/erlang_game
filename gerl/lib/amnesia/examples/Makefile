#
# Makefile
#
# $Id$
#

include ../Makefile.conf

EBIN=../ebin
INCLUDE=../include
BEAM=erlc -b beam -pa $(EBIN) \
	-I $(INCLUDE)
ERL=erl -pa $(EBIN)
YRL=erlc -W
FIND=find
DOCDIR=../doc

.SUFFIXES: .erl .beam

OBJS=\
test_db \
sales_test \
bank_test \
test



ERL_OBJS=$(foreach file, $(OBJS), $(file).beam)
DOC_OBJS=$(foreach file, $(OBJS), $(DOCDIR)/$(file).html)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

%.beam: %.erl
	$(BEAM) $<

shell:
	$(ERL)

test_create:
	$(ERL) -s test create

test_populate:
	$(ERL) -s test populate

clean:
	$(RM) -f *.beam

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

