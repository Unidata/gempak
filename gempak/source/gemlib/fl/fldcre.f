	SUBROUTINE FL_DCRE  ( filnam, irecsz, lun, iret )
C************************************************************************
C* FL_DCRE								*
C* 									*
C* This subroutine creates a new direct access file and leaves the	*
C* file open.  It returns a logical unit number to be used to access	*
C* the file.								*
C* 									*
C* FL_DCRE  ( FILNAM, IRECSZ, LUN, IRET )				*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*	IRECSZ		INTEGER		Record length in words		*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = cannot create file	*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed from "TYPE=" to "STATUS="	*
C* M. desJardins/GSFC	 3/87	Rewrote 				*
C* M. desJardins/GSFC	 7/87	Added # bytes in IRECSZ			*
C* M. desJardins/GSFC	12/90	Convert to lower case for UNIX		*
C* M. desJardins/NMC	 1/92	Add capability to ignore ST_UCLC	*
C* S. Jacobs/EAI	 9/93	Add check for environmental variables	*
C* G. Krueger/EAI	 3/94	Changed length of char to 132		*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	file*132, flvar*132
C------------------------------------------------------------------------
C*	Get a logical unit number to use. Error return if none available.
C
	CALL FL_GLUN ( lun, iret )
C
C*	Create and open file.
C
	IF ( iret .eq. 0 ) THEN
C
C*	    Convert name to lower case.
C
	    IF  ( MTMACH .eq. MTVAX )  THEN
		file = filnam
	      ELSE IF  ( filnam (1:1) .eq. '^' )  THEN
		file = filnam (2: )
	      ELSE
		CALL ST_UCLC  ( filnam, file, ier )
	    END IF
C
C*	    Decode any environmental variables.
C
	    CALL SS_ENVR ( file, flvar, ier )
C*
	    OPEN ( UNIT   = lun,      FILE   = flvar, STATUS = 'NEW',
     *	           ACCESS = 'DIRECT', IOSTAT = iostat, 
     *		   RECL   = MMRECL * irecsz )
C
C*	   If open failed, free lun and get error code to return.
C
	    IF  ( iostat .ne. 0 )  THEN
		iret = -3
		CALL FL_FLUN ( lun, ier )
		lun = 0
	    END IF
	END IF
C*
	RETURN
	END
