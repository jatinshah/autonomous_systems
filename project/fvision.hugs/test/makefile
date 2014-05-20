# Generated automatically from Makefile.in by configure.
PROG		= showgreg
XV = $(HOME)/local/XVision2
CPPFLAGS	=  \
                  -I$(XV)/include -g -DSCREEN_DEPTH=$(SD) 
OBJS		=  main.o
LIBS		= -L$(XV)/lib -lXVDevs -lXVCons -lXVImages -lXVTools -lmpeg\
		   -L/usr/X11R6/lib -lXxf86vm -lXxf86dga -lXext -lX11  -lpthread 

# if you want to set the screen depth explicitly,
# do it here.  Otherwise, the depth of the screen 
# used to compile XVision2 (the screen you type "make" in)
# will be determined and used.
SD = $(shell $(XV)/bin/screendepth)

all: .depend $(OBJS)
	$(CXX) -gp -o $(PROG) $(OBJS) $(LIBS) -lm

clean: ;	rm -f *.o
		rm -f $(PROG) .depend

.depend: $(OBJS:.o=.cc)
	$(CC) -M  $(CPPFLAGS) $(OBJS:.o=.cc) > .depend

install: all
	install -c $(PROG) $(CUR_DEPTH)/bin

include .depend
