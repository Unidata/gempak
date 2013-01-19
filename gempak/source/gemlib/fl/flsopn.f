	SUBROUTINE FL_SOPN  ( filnam, lun, iret )
C************************************************************************
C* FL_SOPN								*
C* 									*
C* This subroutine opens an existing sequential file and returns a 	*
C* logical unit number to be used to access the file.  The file is 	*
C* opened READONLY.							*
C* 									*
C* FL_SOPN  ( FILNAM, LUN, IRET )					*
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
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed "TYPE=" to "STATUS="		*
C* M. desJardins/GSFC	 2/90	UNIX					*
C* J. Whistler/SSAI	 6/91	Added SS_ENVR to convert sys. variables	*
C* M. desJardins/NMC	 1/92	Added capability to ignore ST_UCLC	*
C* G. Krueger/EAI        3/94   Changed length of char to 132           *
C* M. desJardins/NMC	 8/94	Use FL_INQR to get case for file name	*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	file*132
	LOGICAL		exist
C------------------------------------------------------------------------
C*	Get a logical unit number to use.  Error if none available.
C
	CALL FL_GLUN  ( lun, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Call to FL_INQR will perform all the case tests and translate
C*	environmental variables.
C
	CALL FL_INQR  ( filnam, exist, file, ier )
C
C*	If the file exists, open it.  If not, attempt open in order to
C*	get correct error number from this machine.
C
	IF  ( exist )  THEN
	    OPEN ( UNIT = lun, FILE = file,  STATUS = 'OLD',
     +		   ACCESS = 'SEQUENTIAL',  IOSTAT = iostat ) 
	  ELSE
	    OPEN ( UNIT = lun, FILE = filnam,  STATUS = 'OLD',
     +		   ACCESS = 'SEQUENTIAL',  IOSTAT = iostat ) 
	END IF
C
C*	If open failed, get error message and free lun.
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -1
	    CALL FL_FLUN ( lun, ier )
	    lun = 0
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
