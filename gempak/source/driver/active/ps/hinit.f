	SUBROUTINE HINIT ( iret )
C************************************************************************
C* HINIT - PS								*
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
C* M. desJardins/GSFC	12/90						*
C* M. desJardins/GSFC	 2/91	Changed default sizes, esp. for dots	*
C* J. Nielsen/SUNYA	 3/91	Added color; changed default size	*
C* K. Brill/NMC		10/91	Changed factor in bscalw to 1.3		*
C* M. desJardins/NMC	12/91	Eliminate color initialization		*
C* M. desJardins/NMC	 1/92	Add filflg				*
C* S. Jacobs/NCEP	 6/96	Removed opnfil flag			*
C* S. Jacobs/NCEP	 9/96	Moved ISPANX/Y to HINITA		*
C* E. Wehner/EAi	10/96	Added pip scaling factors		*
C* S. Jacobs/NCEP	 3/97	Changed bscalc from 30 to 35		*
C* S. Maxwell/GSC	 6/97	Removed SAVRES				*
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C* M. Linda/GSC		 7/97	Added bscall				*
C* S. Jacobs/NCEP	 2/98	Added crvscl				*
C* I. Durham/GSC	 4/98	Added bscals and bscali			*
C* S. Jacobs/NCEP	 4/98	Modified bscalh, bscalp and bscalt	*
C* S. Jacobs/NCEP	 7/98	Changed bscalc from 35 to 28		*
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
	nlthw  = 1
	ntxhw  = 1
	nwdhw  = 1
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
	bscalc = 28.
	bscala = 56.
        bscalh = 168.
	bscalb = 700.
	bscalm = 22.
	bscalw = bscalc * 1.3
	bscald = 6.
	lpscal = 50
	bscall = bscalc * 13.5
        bscalp = 40.
        bscalt = 40.
	bscals = 700.
	bscali = bscals / 4.0
	bscalf = 416.5
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 600.0
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
	ncirhw = 2
C*
	RETURN
	END
