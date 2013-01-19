	SUBROUTINE FL_SUNK  ( filnam, lun, iret )
C************************************************************************
C* FL_SUNK								*
C* 									*
C* This subroutine opens an sequential file and returns a logical	*
C* unit number to be used to access the file.  The file is opened	*
C* as a new file, if possible.  If not, it is opened with status	*
C* of unknown.  Thus, a new version will be created on VMS systems	*
C* and the existing file will be rewritten on UNIX systems.		*
C* 									*
C* FL_SUNK  ( FILNAM, LUN, IRET )					*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = cannot open file		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/90						*
C* J. Whistler/SSAI	 4/91	Use lower case form of file name to open*
C* M. desJardins/NMC	 1/92	Add capability to ignore ST_UCLC	*
C* S. Jacobs/EAI         9/93   Add check for environmental variables   *
C* G. Krueger/EAI        3/94   Changed length of char to 132           *
C* S. Maxwell/GSC	12/96   Modified return code; removed FL_IRET	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	file*132, flvar*132
C------------------------------------------------------------------------
C*	Get a logical unit number to use.  Error if none available.
C
	CALL FL_GLUN  ( lun, iret )
C
C*	Open file as a new file.
C
	IF  ( iret .eq. 0 )  THEN
C
C*	    Convert name to lower case.
C
	    IF  ( MTMACH .eq. MTVAX )  THEN
		file = filnam
	      ELSE IF  ( filnam (1:1) .eq. '^' )  THEN
		file = filnam
	      ELSE
		CALL ST_UCLC  ( filnam, file, ier )
	    END IF
C
C*	    Decode any environmental variables.
C
	    CALL SS_ENVR ( file, flvar, ier )
C*
	    OPEN ( UNIT   = lun,   FILE = flvar,   STATUS = 'NEW',
     +		   ACCESS = 'SEQUENTIAL', IOSTAT = iostat )
C
C*	    If this open failed, try open with status of unknown.
C
	    IF  ( iostat .ne. 0 )  THEN
		OPEN  ( UNIT = lun,    FILE = flvar,   
     +			STATUS = 'UNKNOWN',  ACCESS = 'SEQUENTIAL',   
     +			IOSTAT = iostat )
	    END IF
	    IF  ( iostat .ne. 0 )  THEN
		iret = -1
		CALL FL_FLUN ( lun, ier )
	    END IF
	END IF
C*
	RETURN
	END
