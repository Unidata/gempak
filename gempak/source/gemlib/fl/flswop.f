	SUBROUTINE FL_SWOP  ( filnam, lun, iret )
C************************************************************************
C* FL_SWOP								*
C* 									*
C* This subroutine opens or creates a sequential file and returns a 	*
C* logical unit number to be used to access the file.  The file is 	*
C* opened for write access.						*
C* 									*
C* FL_SWOP  ( FILNAM, LUN, IRET )					*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					<>0 = GEMPAK file error 	*
C*					 -1 = cannot open file		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/86						*
C* M. desJardins/GSFC	 6/88	Added carriagecontrol='list' to create	*
C*				files that can be read & typed		*
C* M. desJardins/GSFC	 2/90	UNIX					*
C* M. desJardins/NMC	 1/92	Add capability to ignore ST_UCLC	*
C* S. Jacobs/EAI         9/93   Add check for environmental variables   *
C* G. Krueger/EAI        3/94   Changed length of char to 132           *
C* M. desJardins/NMC	 8/94	Reorganized				*
C* M. desJardins/NMC	 8/94	Open files as UNKNOWN so IBM will work	*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C* m.gamazaychikov/SAIC	04/08	Modified how ^ character is found	*
C* m.gamazaychikov/SAIC	04/08	Added initialization of icarr, lenf	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	file*132, flvar*132
	LOGICAL		exist
C------------------------------------------------------------------------
C*	First, get a logical unit number and check to see if file exists.
C
	CALL FL_GLUN  ( lun, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*      Find if there is ^ character in the file name
C
        icarr = 0
        lenf = 0
        CALL ST_LSTR ( filnam, lenf, ier)
        DO ic = 1, lenf
           IF  ( filnam (ic:ic) .eq. '^' ) icarr = ic 
        END DO
	IF  ( icarr .ne. 0 )  THEN
		file = filnam(:icarr-1)//filnam ( icarr+1: )
                filnam = file
        END IF
	CALL FL_INQR  ( filnam, exist, file, ier )
C
C*	If file does not exist, translate environmental variables.
C
	IF  ( .not. exist )  THEN
C
C*	    Translate name to lower case.
C
	    IF  ( MTMACH .eq. MTVAX )  THEN
		file = filnam
	      ELSE IF  ( icarr .ne. 0 )  THEN
		file = filnam
	      ELSE
		CALL ST_UCLC  ( filnam, file, ier )
	    END IF
C
C*	    Decode any environmental variables.
C
	    CALL SS_ENVR ( file, flvar, ier )
	    file = flvar
	END IF
C
C*	Open the file with type UNKNOWN.
C
	OPEN  ( UNIT = lun, FILE = file, STATUS = 'UNKNOWN',
     +		ACCESS = 'SEQUENTIAL', IOSTAT = iostat )
C
C*	Set return code.  If open failed, also free lun.
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -1
	    CALL FL_FLUN  ( lun, ier )
	    lun = 0
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
