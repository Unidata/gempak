	SUBROUTINE GR_WGB2  ( lun, nlun, title, num, gdattm, level, 
     +  		ivcord, parm, idis, icat, iidn, ipdn, 
     +                  ivco, igdn, iret )
C************************************************************************
C* GR_WGB2								*
C*									*
C* This subroutine writes GRIB2 and GEMPAK grid identifier information	*
C* to the specified logical unit using a standard format.  TITLE is 	*
C* set to indicate that the title line:					*
C*									*
C*MESG# GDT# DIS# CAT# ID # PDT# VCD# GEMPAK_TIME LEVEL1 LEVEL2 VCRD PARM*
C*									*
C* is to be written first.  The first three parameters written are	*
C* grid identifiers (grid number, discipline number, category number,	* 
C* product identification template number, and vertical coord. number.	*
C*									*
C* GR_WGB2  ( LUN, NLUN, TITLE, NUM, GDATTM, LEVEL, IVCORD, PARM,       *
C*            IDIS, ICAT, IIDN, IPDN, IVCO, IGDN, IRET )		*
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
C*	IDIS		INTEGER		GRIB2 discipline number		*
C*	ICAT		INTEGER		GRIB2 category number		*
C*	IIDN		INTEGER		GRIB2 id number			*
C*	IPDN		INTEGER		GRIB2 product definition number	*
C*	IVCO		INTEGER		GRIB2 vertical coord. number	*
C*	IGDN		INTEGER		GRIB2 grid definition template #*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* m.gamazaychikov/SAIC	 5/03						*
C* T. Piper/SAIC        05/03   Fixed title format statement and made   *
C*                              integer variables actually integers!    *
C************************************************************************
	CHARACTER*(*)	gdattm (*), parm
	INTEGER		level  (*), lun (*)
	LOGICAL		title
C*
	CHARACTER	vcord*4, dt*16, pm*12, lev*6
C-----------------------------------------------------------------------
        iret = 0
C
C*	Translate vertical coordinate.
C
	CALL LV_CCRD  ( ivcord, vcord, ier )
C
C*	Move character strings into variables with correct length.
C
	dt = gdattm (1)
	pm = parm
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
	DO ii = 1, nlun
C
C*	    Write title if requested.
C
	    IF   ( title )  THEN
                WRITE ( lun(ii), 1000 )
1000	        FORMAT( /1X, 'MESG# GDT# DIS# CAT# ID#  PDT# VCD#',    
     +                 4X, 'GEMPAK_TIME', 4X, 'LEVL1 LEVL2  VCRD PARM')
	    END IF
C
C*	    Write the grid identifier.
C
	    WRITE ( lun(ii), 2000 ) num, igdn, idis, icat, iidn, ipdn,
     +                              ivco, dt, level(1), lev, vcord, pm
2000	    FORMAT ( I5, 2X, 6(I3,2X), 1X, A, I6, 1X, A,1X, A, 1X, A )
C*
	END DO
C*
	RETURN
	END
