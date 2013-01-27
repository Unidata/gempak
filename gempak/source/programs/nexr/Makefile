#
#       Makefile for Unidata contrib Software
#	Chiz Unidata
#
include $(CONFIGDIR)/$(MAKEINC)
UTILITIES = \
	nex2gini \
	nex2img \
	nexr2rhi

SUBDIRS = $(UTILITIES) 

SUBDIRS_GF = nexr2rhi

all :
	@for dir in $(SUBDIRS); do \
		cd $$dir; echo -n "making in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd .. ; \
	done



install :
	 @for dir in $(SUBDIRS); do \
	 	cd $$dir; echo -n "installing in " ; \
	             pwd ; $(MAKE) $(MFLAGS) install ; cd ../ ; \
	 done

programs_gf:
	@for dir in $(SUBDIRS_GF); do \
		cd $$dir; echo -n "making programs_gf in " ; \
			pwd ; $(MAKE) $(MFLAGS) $@; cd ../ ; \
	done

clean:
	@for dir in $(SUBDIRS); do \
	       cd $$dir; echo -n "making clean in " ; \
	             pwd ; $(MAKE) $(MFLAGS) $@ ; cd ../ ; \
	done
