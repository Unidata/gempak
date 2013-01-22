	SUBROUTINE HINIT_XW  ( iret )
C************************************************************************
C* HINIT_XW - XWP							*
C*									*
C* This subroutine initializes the variables for a device driver.	*
C* Each parameter must be specified for each device.			*
C*									*
C* HINIT_XW  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	10/96	Copied from HINIT of the XW driver	*
C* S. Jacobs/NCEP	 3/97	Added bscalp and bscalt			*
C* C. Lin/EAi            6/97   Added 'S' coordinate characteristics    *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Maxwell/GSC        6/97   Removed SAVRES                          *
C* M. Linda/GSC		 7/97	Added bscall				*
C* S. Jacobs/NCEP	 2/98	Added crvscl				*
C* I. Durham/GSC	 4/98	Added bscals and bscali			*
C* S. Jacobs/NCEP	 4/98	Increased the value of lpscal		*
C* S. Jacobs/NCEP	 4/98	Modified bscalp and bscalt		*
C* S. Jacobs/NCEP	 7/98	Changed bscalc from 1.5 to 1.3		*
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
	colcmp = .true.
C
C*	nncolr = number of device colors.  ( A maximum of 32 colors may
C*	be initialized.  nncolor may be larger. )
C
	nncolr = 32
C
C*	nlthw = hw line type flag  ( 0 = sw only, 1 = hw available )
C*	ntxhw = hw text flag       ( 0 = sw only, 1 = hw available )
C*	nwdhw = hw line width flag ( 0 = sw only, 1 = hw available )
C*	nmkhw = hw marker flag     ( 0 = sw only, 1 = hw available )
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
	ibot   = 849
	iright = 879
	itop   = 0 
	aspect = 1.
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the  'S' coord system
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = iright - ileft
        ishght  = ibot - itop
C
C*	bscalc = text scaling factor ( 1.0 is 7 x 9 device units )
C*	bscala = base arrow scaling factor ( length of arrow @ speed 1.0 )
C*	bscalh = base arrow head scaling factor 
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
	bscalc = 1.3
	bscala = 4.
	bscalh = 12.
	bscalb = 30.
	bscalm = 1.5
	bscalw = bscalc * 1.3
	bscald = 1.4
	lpscal = 4
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
	filflg = .true.
C
C*	evtflg = check for events flag
C
	evtflg = .true.
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
