#
#       Makefile for LDM4-GEMPAK 5.1 Bridge Software
#	P.Bruehl Unidata
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	ldmlog \
	ldmbridge \
	programs

all :
	@case $(NA_OS) in \
	ibm)  (for dir in $(SUBDIRS); do \
	           cd $$dir/ibm; echo -n "making in " ; \
	            pwd ; $(MAKE) $(MFLAGS) $@ ; cd ../.. ; \
	      done) ;; \
	 *)   (for dir in $(SUBDIRS); do \
	            cd $$dir; echo -n "making in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd .. ; \
	      done) ;; \
	 esac



install :
	@case $(NA_OS) in \
	 ibm)  (for dir in $(SUBDIRS); do \
	            cd $$dir/ibm; echo -n "installing in " ; \
	               pwd ; $(MAKE) $(MFLAGS) install ; cd ../.. ; \
	       done) ;; \
	 *)    (for dir in $(SUBDIRS); do \
	            cd $$dir; echo -n "installing in " ; \
	             pwd ; $(MAKE) $(MFLAGS) install ; cd ../ ; \
	       done) ;; \
	 esac

programs_gf:
	@cd programs;  echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

clean:
	 for dir in $(SUBDIRS) ; do \
	      	    cd $$dir; echo -n "making clean in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd ../ ; \
	 done

