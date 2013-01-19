	SUBROUTINE HINIT  ( iret )
C************************************************************************
C* HINIT - GIF 								*
C*									*
C* This subroutine initializes the device variables for a device driver.*
C* Each parameter must be specified for each device.			*
C*									*
C* HINIT  ( IRET )							*
C*									*
C* Output parameter:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/90						*
C* M. desJardins/GSFC	 2/91	Changed default sizes, esp. for dots	*
C* J. Nielsen/SUNYA	 3/91	Added color; changed default size	*
C* K. Brill/NMC		10/91	Changed factor in bscalw to 1.3		*
C* M. desJardins/NMC	12/91	Eliminate color initialization		*
C* M. desJardins/NMC	 1/92	Add filflg				*
C* D. Austin		 5/96	modified for GD library gif driver	*
C* T. Lee/GSC		 7/00	Added more device variables		*
C* T. Lee/GSC		12/00	Changed sat color number from 128 to 95	*
C* R. Tian/SAIC		05/02	Added init for ibgfax and ibwfax	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
	iret = NORMAL
	opnfil = .false.
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
C*	nncolr = number of device colors 
C*               ( A maximum of MXCLNM = 32 colors may be initialized.  
C*		   NNCOLR may be larger. )
C
	nncolr = 256
C
C*	number and location of colors for radar and satellite and fax
C
	ibgsat = 1
	nbwsat = 95
	ibgrad = 97 
	nbwrad = 20
	ibgfax = 32 
	nbwfax = 2
C
C*	nlthw = hw line type flag  ( 0 = sw only, 1 = hw available )
C*	ntxhw = hw text flag       ( 0 = sw only, 1 = hw available )
C*	nwdhw = hw line width flag ( 0 = sw only, 1 = hw available )
C*	nmkhw = hw marker flag     ( 0 = sw only, 1 = hw available )
C
	nlthw  = 0
	ntxhw  = 0
	nwdhw  = 0
	nmkhw  = 0
C
C*	ileft = left device coordinate
C*	ibot = bottom device coordinate
C*	iright = right device coordinate
C*	itop = top device coordinate
C*	aspect = width to height pixel aspect ratio
C
	ileft  = 0
	ibot   = 479
	iright = 639
	itop   = 0
	aspect = 1.0
C
C*	bscalc = text scaling factor ( 1.0 is 7 x 9 device units )
C*	bscala = base arrow scaling factor ( length of arrow @ speed 1.0 )
C*	bscalh = base arrow head size factor
C*	bscalb = length of wind barb base
C*	bscalm = marker scaling factor ( 1.0 is 7 x 7 device units )
C*      bscalw = weather symbol scaling factor
C*      bscald = dot width scale factor for weather symbols
C*	lpscal = scaling term for all line patterns
C*	bscall = logo scaling factor
C*	bscalp = scaling for special pattern lines
C*	bscalt = second scaling for special pattern lines (not used)
C*	bscals = hash mark size scaling factor
C*	bscali = hash mark spacing scaling factor
C*	bscalf = point reduction filter scaling factor
C
	bscalc = 1.5
	bscala = 4.0
        bscalh = 12.0
	bscalb = 30.0
	bscalm = 1.5
	bscalw = bscalc * 1.3
	bscald = 1.4
	lpscal = 2
	bscall = bscalc * 13.5
	bscalp = 2.
	bscalt = 2.
	bscals = 30.
	bscali = bscals / 4.0
	bscalf = 18.25
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 25.0
C
C*	filflg = hardware fill flag
C
	filflg = .false.
C
C*	evtflg = check for events flag
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
