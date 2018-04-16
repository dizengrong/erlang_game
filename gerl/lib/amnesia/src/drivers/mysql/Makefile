#
# Makefile
#
# $Id$
#

EBIN=../../../ebin
BEAM=erlc -b beam -pa $(EBIN) -o $(EBIN)
ERL=erl -pa $(EBIN)
YRL=erlc -W
FIND=find
DOCDIR=../doc

.SUFFIXES: .erl .beam

OBJS=\
mysql_drv \
mysql_auth \
mysql_conn \
mysql \
mysql_recv



ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)
DOC_OBJS=$(foreach file, $(OBJS), $(DOCDIR)/$(file).html)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

shell:
	$(ERL)

test_create:
	$(ERL) -s test create

test_populate:
	$(ERL) -s test populate

clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

