################################################################
# Makefile for Haskell related work
#
# Note: This Makefile is not linked into the main Makefile tree
#
# 
# This file has some hardwired paths in it - but it's easy to override them.
# For example, the Haggis uses this command:
#
#   make SD=24 GC_OPTS='--target hugs' XV_DIR=/home/reid/y/XVision2
#
# which uses a different screen depth, makes things work with greencard 2.0
# and points at a different XVision2 installation.
################################################################

# Currently Yale specific.  We need to figure out how to make this file 
# uniform at Yale and JHU.

default: FVisionStdDIS.so NXVision2.so

GC       = green-card
GC_OPTS  = --target=hugs
GC_REDIRECT = --output-c
XV_DIR	 = /home/jds58/pkgs/XVision2
LDFLAGS	+= -shared -nostdlib

XV_LIBS += $(wildcard $(XV_DIR)/lib/lib*.a)

# LIBS += -L/usr/X11R6/lib -lX11 -lraw1394 -lXext -lstdc++ -lgcc  
LIBS += -L/usr/X11R6/lib -lX11 -lXext -lstdc++ -lgcc   -lpthread 

CC = cc
CFLAGS += -g -O0 
CFLAGS += -w -Wall -Wno-unused

CXX = g++ 
CXXFLAGS += -g -O0 
CXXFLAGS += -w -Wall -Wno-unused
CXXFLAGS += -fguiding-decls
CXXFLAGS += -I$(XV_DIR)/include -DSCREEN_DEPTH=$(SD)

# if you want to set the screen depth explicitly,
# do it here.  Otherwise, the depth of the screen 
# used to compile XVision2 (the screen you type "make" in)
# will be determined and used.

SD = $(shell $(XV_DIR)/bin/screendepth)


################################################################
# Greencard
################################################################

GC_FILES	= $(wildcard *.gc)
GC_CC_FILES	= $(addsuffix .cc,$(basename $(GC_FILES)))
GC_C_FILES	= $(addsuffix .c, $(basename $(GC_FILES)))
GC_O_FILES	= $(addsuffix .o, $(basename $(GC_FILES)))
GC_SO_FILES	= $(addsuffix .so,$(basename $(GC_FILES)))
GC_HS_FILES	= $(addsuffix .hs,$(basename $(GC_FILES)))

# For understanding compiler warnings and debugging, they're invaluable
# And keeping the .o files reduces rebuild time
.PRECIOUS: $(GC_C_FILES) $(GC_O_FILES)

%.hs %.c: %.gc
	$(GC) $(GC_OPTS) $<

%.o: %.cc
	$(CXX) -c -o $@ $< $(CXXFLAGS) 

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS) 

FVisionStdDIS.so: FVisionStdDIS.o
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

NXVision2.so:	NXVision2.o aux.o $(XV_LIBS)
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS) $(XV_LIBS)

clean::
	rm -f $(GC_CC_FILES) $(GC_C_FILES) $(GC_O_FILES) $(GC_SO_FILES) $(GC_HS_FILES)
	rm aux.o

veryclean:: clean
	rm -f *~


################################################################
# End of Makefile
################################################################
