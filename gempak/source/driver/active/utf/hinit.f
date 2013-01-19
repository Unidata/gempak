	SUBROUTINE HINIT  ( iret )
C************************************************************************
C* HINIT - UTF								*
C*									*
C* This subroutine initializes the device variables for a device driver.*
C* Each parameter must be specified for each device.			*
C*									*
C* HINIT ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	11/96	Initial Coding				*
C* S. Maxwell/GSC        6/97	Documentation changes                   *
C* S. Maxwell/GSC        6/97	Removed SAVRES		                *
C* S. Jacobs/NCEP	 7/97	Changed nlthw to software only (0);	*
C*				Changed bscalc from 7 to 3.25		*
C* M. Linda/GSC		 7/97	Added bscall				*
C* S. Jacobs/NCEP	 8/97	Added colcmp from HINITA		*
C* S. Jacobs/NCEP	 2/98	Added crvscl				*
C* I. Durham/GSC	 4/98	Added bscals and bscali			*
C* S. Jacobs/NCEP	 4/98	Modified bscalp and bscalt		*
C* S. Jacobs/NCEP	 7/98	Changed bscalc from 3.25 to 2.5		*
C* A. Hardy/GSC		11/98	Added ncirhw                            *
C* S. Jacobs/NCEP	 5/99	Added bscalf				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
C
C*	Initialize device characteristics
C
C*	nnfrm = number of frames ( not used; set to 1 )
C*	ndtyp = device type ( 1 = direct access, 2 = sequential )
C
	nnfrm  = 1
	ndtyp  = 2
C
C*	nlthw = hw line type flag  ( 0 = sw only, 1 = hw available )
C*	ntxhw = hw text flag       ( 0 = sw only, 1 = hw available )
C*	nwdhw = hw line width flag ( 0 = sw only, 1 = hw available )
C*	nmkhw = hw marker flag     ( 0 = sw only, 1 = hw available )
C
	nlthw  = 0
	ntxhw  = 1
	nwdhw  = 0
	nmkhw  = 0
C
C*	aspect = width to height pixel aspect ratio
C
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
C*      bscalp = scaling for special pattern lines
C*      bscalt = second scaling for special pattern lines (not used)
C*	bscals = hash mark size scaling factor
C*	bscali = hash mark spacing scaling factor
C*	bscalf = point reduction filter scaling factor
C
	bscalc = 2.5
	bscala = 7.
        bscalh = 19.
	bscalb = 50.
	bscalm = 3.
	bscalw = bscalc * 1.8
	bscald = 5.
	lpscal = 5
	bscall = bscalc * 13.5
        bscalp = 4.
        bscalt = 4.
	bscals = 50.
	bscali = bscals / 4.0
	bscalf = 36.5
C
C*	colcmp = color compute flag ( .true.  = color device )
C*				    ( .false. = monochromatic device )
C
	colcmp = .false.
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 1024.0
C
C*	filflg = hardware fill flag
C
	filflg = .true.
C
C*	evtflg = check for events flag
C
	evtflg = .false.
C
C*	ncirhw = hardware circl flag
C
	ncirhw = 1
C
C*	colcmp = color compute flag ( .true.  = color device )
C*				    ( .false. = monochromatic device )
C
	colcmp = .false.
C*
	RETURN
	END
