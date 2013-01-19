	SUBROUTINE JB_OPEN  ( filnam, ifn, subset, iret )
C************************************************************************
C* JB_OPEN								*
C*									*
C* This subroutine opens a Jack Woollen BUFR data file.         	*
C*									*
C* The subset name is used to restrict parameters that can be read to	*
C* only those from the specified message type.  If no such restriction	*
C* is required or necessary, set subset to blank.			*
C*									*
C* JB_OPEN  ( FILNAM, IFN, SUBSET, IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Name of BUFR data file		*
C*	IFN		INTEGER		File unit number		*
C*	SUBSET		CHAR*8		BUFR subset to read		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C*					-1 = cannot open file		*
C*					-2 = file name is blank		*
C**									*
C* Log:									*
C* K. Brill/EMC		10/96						*
C* K. Brill/EMC		12/96	Remove current file feature		*
C* K. Brill/EMC		 4/97	Added SUBSET				*
C* K. Brill/EMC		 7/98	CALL DATELEN for Y2K compliance		*
C* D. Kidwell/NCEP	11/98	Replace OPEN with CBF_OPEN              *
C* V. KRISHNA KUMAR/NCEP 05/05  Replace call to CBF_OPEN with an        *
C*                              explicit fortran OPEN statement to read *
C*                              a fortran blocked "bufr or prepbufr"    * 
C*                              file to use the latest CCS BUFR library * 
C************************************************************************
	CHARACTER*(*)	filnam, subset
C*
	CHARACTER*8	msgnam
	COMMON 		/SUB/ msgnam
C*-----------------------------------------------------------------------
	iret = 0
	msgnam = subset
C*
	IF ( filnam .eq. ' ' ) THEN
	    iret = -2
	    RETURN
	  ELSE
	    CALL ST_NULL ( filnam, filnam, lenf, ier )
	END IF
C*
        OPEN (unit=ifn,file=filnam,form='unformatted')
C*
	CALL OPENBF ( ifn, 'IN', ifn )
	CALL DATELEN ( 10 )
C*
	RETURN
	END
