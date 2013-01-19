	SUBROUTINE HINIT  ( iret )
C************************************************************************
C* HINIT - GN								*
C*									*
C* This subroutine initializes the variables for a device driver.	*
C* Each parameter must be specified for each device.			*
C*									*
C* HINIT  ( IRET ) 							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 2/90	Removed lpat initialization		*
C* S. Schotz/GSC	 4/90	Added weather and dot base scale	*
C* S. Schotz/GSC	 9/90	Load color table and arrow head size	*
C* M. desJardins/NMC	 1/92	Removed color specification		*
C* M. desJardins/NMC	 1/92	Added filflg				*
C* E. Wehner/EAi	10/96	Added scaling factors for pips		*
C* C. Lin/EAi	         6/97	Added 'S' coordinate system		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Maxwell/GSC        6/97   Removed SAVRES                          *
C* M. Linda/GSC		 7/97	Added bscall				*
C* S. Jacobs/NCEP	 2/98	Added crvscl				*
C* I. Durham/GSC	 4/98	Added bscals and bscali			*
C* S. Jacobs/NCEP	 4/98	Modified bscalp and bscalt		*
C* A. Hardy/GSC		11/98	Added ncirhw                            *
C* S. Jacobs/NCEP	 5/99	Added bscalf				*
C************************************************************************
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
	colcmp = .false.
C
C*	nncolr = number of device colors.  ( A maximum of 16 colors may
C*	be initialized.  nncolor may be larger. )
C
	nncolr = 2
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
C*	ileft  = left device coordinate
C*	ibot   = bottom device coordinate
C*	iright = right device coordinate
C*	itop   = top device coordinate
C*	aspect = width to height pixel aspect ratio
C
	ileft  = 1
	ibot   = 1
	iright = 1000
	itop   = 800
	aspect = 1.
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the  'S' coord system
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS(iright - ileft)
        ishght  = ABS(ibot - itop)
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
	bscalc = 1.
	bscala = 5.
	bscalh = 25.
	bscalb = 20.
	bscalm = 1.
	bscalw = bscalc * 2.0
	bscald = 1.4
	lpscal = 1
	bscall = bscalc * 13.5
	bscalp = 2.
	bscalt = 2.
	bscals = 20.
	bscali = bscals / 4.0
	bscalf = 15.0
C
C*	Set the scaling factor for smooth curves.
C
	crvscl = 30.0
C
C*	filflg = fill hardware flag
C
	filflg = .false.
C
C*	evtflg = check for event flag
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
