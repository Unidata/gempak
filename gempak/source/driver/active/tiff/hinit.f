	SUBROUTINE HINIT  ( iret )
C************************************************************************
C* HINIT - TIFF								*
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
C* S. Jacobs/NCEP	12/98						*
C* S. Jacobs/NCEP	 5/99	Added bscalf				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
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
	ntxhw  = 0
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
	bscalc = 2.0
	bscala = 5.6
        bscalh = 16.8
	bscalb = 40.
	bscalm = 1.
	bscalw = bscalc * 1.3
	bscald = 2.
	lpscal = 4
	bscall = bscalc * 13.5
        bscalp = 4.
        bscalt = 4.
	bscals = 40.
	bscali = bscals / 4.0
	bscalf = 15.5
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 60.0
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
C*
	RETURN
	END
