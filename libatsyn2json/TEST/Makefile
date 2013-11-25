#
# A simple Makefile
#
######

ATSHOMEQ="$(ATSHOME)"
PATSHOMEQ="$(PATSHOME)"

######

ATSCC=$(ATSHOMEQ)/bin/atscc
ATSCCFLAGS=
ATSRUNTIME=$(ATSHOMEQ)/ccomp/runtime

######
ATSCTRB=$(ATSHOMEQ)/contrib
######


CFLAGS_JANSSON=$(shell pkg-config jansson --cflags)
LDFLAGS_JANSSON=$(shell pkg-config jansson --libs)

ifeq ($(CFLAGS_JANSSON), )
$(error Cannot locate jansson. Please run e.g.  \
  export PKG_CONFIG_PATH=${HOME}/programs/install/lib/pkgconfig:${PKG_CONFIG_PATH})
endif

######

all:: mytest

######

mytest: mytest.dats ../libatsyn2json.a
	$(ATSCC) -I$(PATSHOME)/src -IATS $(PATSHOME) -IATS $(ATSHOME) \
  -IATS $(ATSCTRB) -I $(ATSCTRB) $(CFLAGS_JANSSON) -o $@ $< \
  -L $(PATSHOME)/libatsyn2json -latsyn2json -lgmp $(LDFLAGS_JANSSON)
cleanall:: ; $(RMF) mytest

######

RMF=rm -f

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_?ats.c *_?ats.o

cleanall:: clean

###### end of [Makefile] ######

