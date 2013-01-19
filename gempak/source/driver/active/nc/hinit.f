	SUBROUTINE HINIT ( iret )
C************************************************************************
C* HINIT - NC								*
C*									*
C* This subroutine initializes the variables for a device driver.	*
C* Each parameter must be specified for each device.			*
C*									*
C* HINIT ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 2/90	Removed lpat initialization		*
C* S. Schotz/GSC	 4/90	Added weather and dot base scale	*
C* S. Schotz/GSC	 9/90	Load color table & add arrow head size	*
C* K. Brill/NMC		10/91	Changed factor in BSCALW to 1.3		*
C* M. desJardins/NMC	12/91	Changed color initialization		*
C* M. desJardins/NMC	 1/92	GEMPAK 5.1				*
C* A. Chang/EAI		 4/94	Changed ndtyp, nlthw, nmkhw		*
C* A. Chang/EAI		 5/94	Changed scaling code			*
C* S. Jacobs/NMC	 6/94	General clean up			*
C* S. Jacobs/NMC	10/94	Changed text and symbol scaling		*
C* E. Wehner/EAi	10/96	Added scaling factors for pips		*
C* C. Lin/EAI		 6/97	Initialize 'S' coordinates		*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C* S. Maxwell/GSC	 6/97	Removed SAVRES				*
C* M. Linda/GSC		 7/97	Added bscall				*
C* S. Jacobs/NCEP	 2/98	Added crvscl				*
C* S. Jacobs/NCEP	 3/98	Increased the value of lpscal		*
C* I. Durham/GSC	 4/98	Added bscals and bscali			*
C* S. Jacobs/NCEP	 4/98	Modified bscalp and bscalt		*
C* S. Jacobs/NCEP	 7/98	Changed bscalc from 64 to 50		*
C* A. Hardy/GSC		11/98	Added ncirhw                            *
C* S. Jacobs/NCEP	 5/99	Added bscalf				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Initialize device characteristics
C
C*	nnfrm = number of frames ( not used; set to 1 )
C*	ndtyp = device type ( 1 = direct access, 2 = sequential )
C
	nnfrm  = 1
	ndtyp  = 2
C
C*	colcmp = color compute flag ( .true.  = color device )
C*				    ( .false. = monochromatic device )
C
	colcmp = .true.
C
C*	nncolor = number of device colors.  ( A maximum of 32 colors may
C*	be initalized.  nncolor may be larger. )
C
	nncolr = 32
C
C*      nlthw = hw line type flag  ( 0 = sw only, 1 = hw available )
C*      ntxhw = hw text flag       ( 0 = sw only, 1 = hw available )
C*      nwdhw = hw line width flag ( 0 = sw only, 1 = hw available )
C*      nmkhw = hw marker flag     ( 0 = sw only, 1 = hw available )
C
	nlthw  = 0
	ntxhw  = 1
	nwdhw  = 1
	nmkhw  = 0
C
C*	ileft  = left device coordinate
C*	ibot   = bottom device coordinate
C*	iright = right device coordinate
C*	itop   = top device coordinate
C*	aspect = width to height pixel aspect ratio
C
	ileft  = 0
	ibot   = 0
	iright = 32767
	itop   = 32767
	aspect = 1.
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the  'S' coord system
C*      not used in PS driver
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = iright - ileft
        ishght  = itop - ibot
C
C*	bscalc = text scaling factor ( 1.0 is 7 x 9 device units )
C*	bscala = base arrow scaling factor ( length of arrow @ speed 1.0 )
C*	bscalh = base arrow head size factor
C*	bscalb = length of wind barb base
C*	bscalm = marker scaling factor ( 1.0 is 7 x 7 device units )
C*	bscalw = weather symbol sclaing factor (1.0 is 11 x 11 device
C*	 	 units )
C*	bscald = dot width scale factor for weather symbols
C*	lpscal = scaling term for all line patterns
C*	bscall = logo scaling factor
C*      bscalp = scaling for special pattern lines
C*      bscalt = second scaling for special pattern lines (not used)
C*	bscals = hash mark size scaling factor
C*	bscali = hash mark spacing scaling factor
C*	bscalf = point reduction filter scaling factor
C
        bscalc = 50.
        bscala = 256.
        bscalh = 480.
        bscalb = 1120.
        bscalm = 48.
        bscalw = bscalc * 1.3
        bscald = 1.
        lpscal = 140
	bscall = bscalc * 13.5
	bscalp = 75.
	bscalt = 75.
	bscals = 1120.
	bscali = bscals / 4.0
	bscalf = 583.25
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 900.0
C
C*      filflg = hardware fill flag
C
        filflg = .true.
C
C*      evtflg = check for events flag
C
        evtflg = .false.
C
C*	ncirhw = hardware circl flag
C
	ncirhw = 1
C
C*	Do not change anything below this line.
C------------------------------------------------------------------------
	ispanx = ISIGN ( 1, ( iright - ileft ) )
	ispany = ISIGN ( 1, ( itop - ibot ) )
C*
	RETURN
	END
