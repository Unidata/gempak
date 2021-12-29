	SUBROUTINE GINIT
C************************************************************************
C* GINIT								*
C*									*
C* This subroutine initializes variables for GPLT.			*
C*									*
C* GINIT								*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 5/86	Changed default hw/sw line width to hw	*
C* M. desJardins/GSFC	 4/89	/PLOTBF/ was left out by mistake	*
C* M. desJardins/GSFC	 6/89	Added tick type and size		*
C* S. Schotz/GSC	 1/90	Added barb/arrow width and type	and	*
C*				text and marker width			*
C* S. Schotz/GSC	 3/90	Added cloud/weather symbol parameters	*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* M. desJardins/NMC	11/91	Initialize fill buffers			*
C* M. desJardins/NMC	11/91	Initialize contour attributes		*
C* M. desJardins/NMC	 3/92	Change contour attribute initialization	*
C* S. Jacobs/NMC	 6/94	Change contour attribute initialization *
C* K. Brill/EMC		 3/96	Add W & Q in carray; set _tmtyp flag	*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* G. Krueger/EAI	 8/96	Changed default map file to HIPOWO.CIA	*
C* S. Jacobs/NCEP	 8/96	Changed default map file to lower case	*
C*				and removed $GEMMAPS			*
C* M. Linda/GSC		 9/96	Added init. for icng, spcl, and turb	*
C* E. Wehner/EAi	10/96	Added init for front information	*
C* S. Jacobs/NCEP	10/96	Initialize curdev			*
C* M. Linda/GSC		 2/97	Removed PLOTBF.CMN			*
C* D. Keiser/GSC	 3/97	Added init. for special lines		*
C* C. Lin/EAI	 	 6/97	Removed carray				*
C* E. Safford/GSC        6/97   Added init. for special text colors     *
C* S. Maxwell/GSC        6/97   Added KFLTYP and RFILSZ                 *
C* S. Jacobs/NCEP	 9/97	Removed kstlc, kstfc, kstxc		*
C* S. Jacobs/NCEP	 9/97	Added kbrdr, krrotn, kjust		*
C* S. Jacobs/NCEP	 2/98	Added ksmtyp, ketype, rdens, rtensn	*
C* I. Durham/GSC	 3/98	Added rwdasz, rdahsz, kdarwd, kdartp	*
C* I. Durham/GSC	 3/98	Added rhshsz, khwid, klwidh		*
C* I. Durham/GSC	 3/98	Added kcolr2				*
C* S. Jacobs/NCEP	 6/98	Changed pip size from int to real	*
C* A. Hardy/GSC         10/98   Added kcsywd and rcsysz                 *
C* S. Jacobs/NCEP	 5/99	Added rrfilt				*
C* S. Gilbert/NCEP	 5/07	Removed CONTUR.CMN and corres. init. 	*
C* S. Guan/NCEP         07/18   Change hipowo.cia to hipowo.nws         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'XYDEF.CMN'
C*
C------------------------------------------------------------------------
	ddev = ' '
	curdev = ' '
	jtmtyp = 0
	mtmtyp = 0
C
C*	Default attributes.
C
	kcolr  = 1
	kcolr2 = 1
	kltyp  = 1
	klthw  = 1
	klwid  = 1
	klwhw  = 2
	ksltyp = 1
	kslstr = 1
	ksldir = 1
	rslsiz = 1.
	kslwid = 1
	kmark  = 1
	kmkhw  = 1
	kmkwid = 1
	rmksz  = 1.
	ktxfn  = 1
	ktxhw  = 1
	ktxwid = 1
	rtxsz  = 1.
	kbrdr  = 111
	krrotn = 1
	kjust  = 1
	rwasz  = 1.
	rwahsz = 1.
	rwbsz  = 1.
	rwdasz = 1.
	rhshsz = 1.
	rdahsz = 1.
	kbrwid = 1
	kbrtyp = 1
	karwid = 1
	kartyp = 1
	khwid  = 1
	kdarwd = 1
	klwidh = 1
	kdartp = 1
	ktktyp = 1
	rticsz = 1.
C
	rskysz = 1.
	ksktyp = 1
	kskwid = 1
	rwtrsz = 1.
	kwtwid = 1
	rptnsz = 1.
	kptwid = 1
	rpwtsz = 1.
	kpwwid = 1
	rctsz  = 1.
	kctwid = 1
	rcersz = 1.
	kcewid = 1
	rsprsz = 1.
	kspwid = 1
	rtursz = 1.
	ktuwid = 1
	kfltyp = 1
	rfilsz = 1.
	ksmtyp = 0
	ketype = 1
	rdens  = 0.
	rtensn = 0.
	rszdsh = 1.
        kcsywd = 1
        rcsysz = 1.
	rrfilt = 0.
C
C*	Default front characteristics.
C
	kfcod  = 425
	rpipsz = 1.0
	kpipst = 1
	kpipdr = 1
C
C*	Default view region.
C
	xbndlf = 0.0
	ybndbf = 0.0
	xbndrf = 1.0
	ybndtf = 1.0
C
C*	Default map and graph margins.
C
	igmode = 0
	xlmmgn = 0.0
	ybmmgn = 0.0
	xrmmgn = 0.0
	ytmmgn = 0.0
	cszm   = 1.0
C
	xlgmgn = 0.0
	ybgmgn = 0.0
	xrgmgn = 0.0
	ytgmgn = 0.0
	cszg   = 1.0
C
	mset   = .false.
	mtype  = 0
	mpfil  = 'hipowo.nws'
	cmpfil = 'hipowo.nws'
	gset   = .false.
C
	RETURN
	END
