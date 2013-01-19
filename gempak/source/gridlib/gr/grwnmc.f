	SUBROUTINE GR_WNMC  ( lun, nlun, title, num, gdattm, level, 
     +  		ivcord, parm, nmcprm, nmcvco, nmcgdn, iret )
C************************************************************************
C* GR_WNMC								*
C*									*
C* This subroutine writes NMC and GEMPAK grid identifier information	*
C* to the specified logical unit using a standard format.  TITLE is 	*
C* set to indicate that the title line:					*
C*									*
C* MESG#  NMCGRD# PRM# VCD#  GEMPAK_TIME  LEVEL1  LEVEL2  VCRD   PARM	*
C*									*
C* is to be written first.  The first three parameters written are	*
C* NMC grid identifiers (grid number, parm number, vertical coord.	*
C* number).								*
C*									*
C* GR_WNMC  ( LUN, NLUN, TITLE, NUM, GDATTM, LEVEL, IVCORD, PARM,       *
C*            NMCPRM, NMCVCO, NMCGDN, IRET )				*
C*									*
C* Input parameters:							*
C*	LUN (NLUN)	INTEGER		Logical unit for write		*
C*	NLUN		INTEGER		Number of units for write	*
C*	TITLE		LOGICAL		Flag to write title		*
C*	NUM		INTEGER		GRIB message number		*
C*	GDATTM (2)	CHAR*20		GEMPAK time			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*	NMCPRM		INTEGER		NMC parameter number		*
C*	NMCVCO		INTEGER		NMC vertical coord. number	*
C*	NMCGDN		INTEGER		NMC grid number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		 4/91						*
C************************************************************************
	CHARACTER*(*)	gdattm (*), parm
	INTEGER		level  (*), lun (*)
	LOGICAL		title
C*
	CHARACTER	vcord*4, dt*18, p*12, lev*6
C-----------------------------------------------------------------------
	iret = 0
C
C*	Translate vertical coordinate.
C
	CALL LV_CCRD  ( ivcord, vcord, ier )
C
C*	Move character strings into variables with correct length.
C
	dt     = gdattm (1)
	p      = parm
C
C*	Make level 2 into a string.  Write nothing if level2 = -1.
C
	IF  ( level (2) .eq. -1 )  THEN
	    lev = ' '
	  ELSE
	    CALL ST_INCH ( level (2), lev, ier )
	END IF
C
C*	Do prints for all unit numbers.
C
	DO i = 1, nlun
C
C*	    Write title if requested.
C
	    IF   ( title )  THEN
	        WRITE ( lun (i), 1000 )
1000	        FORMAT( /1X, 'MESG# NMCGRD# PRM#   VCD#    GEMPAK_TIME',
     +                   7X, 'LEVL1 LEVL2  VCRD PARM ' )
	    END IF
C
C*	    Write the grid identifier.
C
	    WRITE ( lun (i), 2000 ) num, nmcgdn, nmcprm, nmcvco, dt,
     +				    level(1), lev, vcord, p
2000	    FORMAT ( I5,2X, 3(I4,3X),1X, A, I6,1X, A,1X, A,1X, A )
C*
	END DO
C*
	RETURN
	END
