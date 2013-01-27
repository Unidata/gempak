#
#       Makefile for LDM4-GEMPAK 5.1 Bridge Software
#	P.Bruehl Unidata
#
include $(CONFIGDIR)/$(MAKEINC)
DECODERS = dcgrib dcacars dcnldn dcncprof dcwatch dctrop dcstorm dcgrib2 \
	dcsuomi dcprof dcnexr2 dcuspln dccosmic
UTILITIES = dcreanal dcreanal2 dcredbook dcwmostrip

SUBDIRS = $(DECODERS)  $(UTILITIES)

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

clean:
	@case $(NA_OS) in \
	 ibm)  (for dir in $(SUBDIRS) $(LIBRARIES); do \
	      	    cd $$dir/ibm; echo -n "making clean in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd ../.. ; \
	       done) ;; \
	 *)    (for dir in $(SUBDIRS) $(LIBRARIES); do \
	      	    cd $$dir; echo -n "making clean in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd ../ ; \
	       done) ;; \
	esac

