#
#       Makefile for GEMPAK 5.2
#               Builds the gemlib.a archive
#               Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

SUBDIRS = \
	cmm \
	css \
	cst \
	cfl \
	ctb \
	cv \
	da \
	dm \
	dp \
	er \
	fl \
	fortran_wrappers \
	gg \
	gp \
	im \
	in \
	ip \
	lc \
	lv \
	lw \
	mr \
	mv \
	ss \
	st \
	tb \
	tg \
	ti \
	tm 

OSDIRS = \
	mv

ULTRIXDIRS = \
	mv/ULTRIX

LINUXDIRS = \
	mv/Linux

OSF1DIRS = \
	mv/OSF1



SunOSDIR = \
	css/sunos5

LINUXDIR = \
	css/linux2

AIXDIR = \
	css/aix4




all :
	@for dir in $(SUBDIRS); do \
	   ( cd $$dir; echo "Making $@ in `pwd`" ; \
		$(MAKE) $(MFLAGS) $@ ) ; $(ARSLEEP) \
	done

	@case $(OPSYS) in \
	   LINUX|FreeBSD  ) \
		(cd $(LINUXDIRS); echo "$(OPSYS) - making $@ in `pwd` " ; \
			$(MAKE) $(MFLAGS) $@) ; \
		(cd $(LINUXDIR); echo "$(OPSYS) - making in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ;; \
	   SunOS | X86 ) \
		(cd $(SunOSDIR); echo "$(OPSYS) - making in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ;; \
	   ULTRIX  ) \
		(cd $(ULTRIXDIRS); echo "$(OPSYS) - making $@ in `pwd` " ; \
			$(MAKE) $(MFLAGS) $@) ;; \
	   OSF1 ) \
		(cd $(OSF1DIRS); echo "$(OPSYS) - making $@ in `pwd` " ; \
			$(MAKE) $(MFLAGS) $@) ;; \
	   AIX ) \
		if  [ $(AIX_NO_GEMPAK_SCANDIR) ] ; then \
		   echo "Using system scandir" ; \
		else \
		   ( cd $(AIXDIR); echo "$(OPSYS) - making in `pwd`" ; \
			$(MAKE) $(MFLAGS) $@) ; \
		fi ; \
		;; \
	   *) \
		(cd $(OSDIRS); echo "$(OPSYS) - making $@ in `pwd` " ; \
			$(MAKE) $(MFLAGS) $@) ;; \
	esac

install :

makefiles:
	genmake.csh

clean:
	@case $(OPSYS) in \
        SunOS | X86 ) \
        for dir in NODIR $(SUBDIRS) $(SunOS); do \
                case $$dir in \
                        NODIR) ;; \
                        *) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
                                pwd ; $(MAKE) $(MFLAGS) $@) ;; \
                esac ; \
        done ;; \
        AIX ) \
        for dir in NODIR $(SUBDIRS) $(AIX); do \
                case $$dir in \
                        NODIR) ;; \
                        *) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
                                pwd ; $(MAKE) $(MFLAGS) $@) ;; \
                esac ; \
        done ;; \
	ULTRIX ) \
	for dir in NODIR $(SUBDIRS) $(ULTRIXDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done ;; \
	OSF1 ) \
	for dir in NODIR $(SUBDIRS) $(OSF1DIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done ;; \
	LINUX ) \
	for dir in NODIR $(SUBDIRS) $(OSF1DIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
		esac ; \
	done ; \
        for dir in NODIR $(SUBDIRS) $(LINUX); do \
                case $$dir in \
                        NODIR) ;; \
                        *) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
                                pwd ; $(MAKE) $(MFLAGS) $@) ;; \
                esac ; \
        done ;; \
	*) \
		for dir in NODIR $(SUBDIRS) $(OSDIRS); do \
			case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making clean in " ; \
				pwd ; $(MAKE) $(MFLAGS) $@) ;; \
			esac ;  \
		done  ;; \
	esac

testprog :
	@case $(OPSYS) in \
	ULTRIX  ) \
	for dir in NODIR $(SUBDIRS) $(ULTRIXDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making in " ; \
				pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
		esac ; \
	done ;; \
	OSF1 ) \
	for dir in NODIR $(SUBDIRS) $(OSF1DIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making in " ; \
				pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
		esac ; \
	done ;; \
	LINUX ) \
	for dir in NODIR $(SUBDIRS) $(OSF1DIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) (cd $$dir; echo -n "$(OPSYS) - making in " ; \
				pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
		esac ; \
        done ;; \
	*) \
		for dir in NODIR $(SUBDIRS) $(OSDIRS); do \
			case $$dir in \
				NODIR) ;; \
				*) (cd $$dir; echo -n "$(OPSYS) - making in " ; \
					pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
			esac ;  \
		done  ; \
		for dir in NODIR $(SUBDIRS); do \
                	case $$dir in \
                        	NODIR) ;; \
                        	*) (cd $$dir; echo -n "$(OPSYS) - making in " ; \
                                	pwd ; $(MAKE) $(MFLAGS) tlink) ;; \
        	        esac ;  \
	      	done  ;; \
	
	esac
