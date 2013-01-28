#################################################################
###################################################################
#
#	Makefile for UPC GEMPAK 6.6.0
#		Top level
#
####################################################################
####################################################################
####################################################################
#
#   The following environment variables must be defined before 
#   invoking this Makefile:
#
#   $NAWIPS  - points to the toplevel of the NAWIPS directory tree.
#   $GEMPAK and $GEMPAKHOME  - points to the toplevel of the gempak part
#              of the NAWIPS tree.  Usually directly under $NAWIPS.
#
#   $NA_OS   - Used in $(NAWIPS)/config/$(MAKEINC) to decide which
#              OS specific Makeinc.* to include.  It also cooresponds
#              to the directories in the NAWIPS tree where the executables
#              and libraries are installed.
#
####################################################################
#
#
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	extlibs \
	gempak 

# .INIT: INSTALLDIRS

all : INSTALLDIRS
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ) ; \
	done

All : all

everything: distclean all install programs_nc programs_gf
Everything: everything

basic: INSTALLDIRS distclean all install

web: INSTALLDIRS distclean all install programs_gf

programs_nc : INSTALLDIRS
	@cd gempak;  echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@

programs_gf : INSTALLDIRS
	@cd gempak;  echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@

install : INSTALLDIRS
	@case $(HAVEMOTIF) in \
	NO) \
	for dir in NODIR $(SUBDIRS_NOMOTIF); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "Making $@ in `pwd`" ; \
				$(MAKE) $(MFLAGS) $@) ; \
			   else \
				echo "Skipping $$dir"; \
			   fi; \
			;; \
		esac ; \
	done \
	;; \
	*) \
	for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "Making $@ in `pwd`" ; \
				$(MAKE) $(MFLAGS) $@) ; \
			   else \
				echo "Skipping $$dir"; \
			   fi; \
			;; \
		esac ; \
	done \
	;; \
	esac

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "Making $@ in `pwd`" ; \
				$(MAKE) $(MFLAGS) $@) ; \
			   else \
				echo "Skipping $$dir"; \
			   fi; \
			;; \
		esac ; \
	done

testprog :
	@cd gempak;  echo "making $@ programs in `pwd`" ; \
	   $(MAKE) $(MFLAGS) $@

#
#  Create the Installation directories, if they don't exist.
#
INSTALLDIRS: $(OS_LIB) $(OS_BIN) $(OS_INC) $(OS_INC)/MCHPRM.PRM
	@echo "..."

$(OS_LIB): 
	@echo "	Creating $@"
	@mkdir -p $@
	@chmod g+s $@

$(OS_BIN):
	@echo "	Creating $@"
	@mkdir -p $@
	@chmod g+s $@

$(SCRIPTS_EXE):
	@echo "	Creating $@"
	@mkdir -p $@
	@chmod g+s $@

$(OS_INC):
	@echo "	Creating $@"
	@mkdir -p $@
	@chmod g+s $@

$(OS_INC)/MCHPRM.PRM: $(OS_INC)
	if [ ! -f $(OS_INC)/MCHPRM.PRM ] ; then \
	   ln -s $(INCDIR)/$(MCHSYS) $(OS_INC)/MCHPRM.PRM ; \
	fi

# ln -s $(INCDIR)/MCHPRM.$(OPSYS) $(OS_INC)/MCHPRM.PRM ; \

#
# Keep the bin directory, unless we want really clean!
distclean: clean
	@if [ -d os/$(NA_OS) ] ; then \
	    ( echo  "making distclean in os/$(NA_OS)" ; cd os/$(NA_OS) ; $(RM) -rf share man include lib ) ; \
	fi

reallyclean: distclean
	@if [ -d os/$(NA_OS) ] ; then \
	    ( echo  "removing os/$(NA_OS)" ; cd os ; $(RM) -rf $(NA_OS) ) ; \
	fi

gnu_fortran:
	@if \
	    which g77 ; \
	then \
	    echo 'Use g77' ; \
	else \
	    if \
	        which gfortran ; \
	    then \
	        echo 'Use GFORTRAN' ; \
	    else \
	        echo 'No fortran compiler' ; \
	    fi ; \
	fi
