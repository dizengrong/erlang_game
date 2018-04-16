#
#
#

include Makefile.conf
VSN=1.6.2
TARGET_DIR=amnesia-$(VSN)
TARGET_PATH=$(ERLLIB)/$(TARGET_DIR)
APP_NAME = amnesia

all:
	(cd src; make)

tests:
	(cd examples; make)

clean:
	(cd src; make clean) && \
	(cd examples; make clean)

shell:
	erl -pa ebin -pa examples

docs:
	@[ -d doc/reference_manual ] || mkdir doc/reference_manual
	@(cd src; \
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"../doc/reference_manual"' '[{def,{vsn,"$(VSN)"}}]'; \
	cd ..)

release:
	(make clean; \
	cd ..; \
	rm -rf amnesia-*; \
	mkdir amnesia-$(VSN); \
	cp -r trunk/* amnesia-$(VSN); \
	tar \
	  	--exclude=amnesia-$(VSN)/website* \
		--exclude=amnesia-$(VSN)/TODO \
		--exclude=amnesia-$(VSN)/doc/join_syntax* \
		--exclude=*/.svn* \
	-zcvf amnesia-$(VSN).tar.gz amnesia-$(VSN); \
	rm -rf amnesia-$(VSN); \
	)

install:
	@(echo "Checking for previous installation of Amnesia...";\
	cd $(ERLLIB);\
	for i in `ls -1`; do \
	LIBNAME=`echo $$i | tr -c -d [:alpha:]`;\
	if [ "$$LIBNAME" = "amnesia" ]; then \
		AMNESIADIR=$$i; \
	fi; \
	done; \
	if [ -n "$$AMNESIADIR" ]; then \
		echo "Removing previous installation of Amnesia: " $$AMNESIADIR;\
		rm -rf $$AMNESIADIR; \
	fi \
	)
	@echo "Installing " $(TARGET_DIR)
	@install -d --owner=root --group=root $(TARGET_PATH)
	@install -d --owner=root --group=root $(TARGET_PATH)/src/driver/mysql
	@install -d --owner=root --group=root $(TARGET_PATH)/ebin
	@install -d --owner=root --group=root $(TARGET_PATH)/include
	@install -d --owner=root --group=root $(TARGET_PATH)/examples
	@install --mode=644 --owner=root --group=root \
		src/*.erl $(TARGET_PATH)/src
	@install --mode=644 --owner=root --group=root \
		src/drivers/mysql/*.erl $(TARGET_PATH)/src
	@install --mode=644 --owner=root --group=root \
		include/*.hrl $(TARGET_PATH)/include
	@install --mode=644 --owner=root --group=root \
		ebin/*.beam $(TARGET_PATH)/ebin
	@install --mode=644 --owner=root --group=root \
		examples/*.erl $(TARGET_PATH)/examples
	@install --mode=644 --owner=root --group=root \
		examples/*.hrl $(TARGET_PATH)/examples
	@echo "Done"

