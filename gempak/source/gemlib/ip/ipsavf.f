	SUBROUTINE IP_SAVF ( file, filnam, iret )
C************************************************************************
C* IP_SAVF								*
C*									*
C* This subroutine makes an output save file name.  If the file name	*
C* does not contain a period, '.nts' is appended.			*
C*									*
C* IP_SAVF ( FILE, FILNAM, IRET )					*
C*									*
C* Input parameters:							*
C*	FILE		CHAR*		Input file name			*
C*									*
C* Output parameters:							*
C*	FILNAM		CHAR*		Output file name		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = invalid file name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89						*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* S. Schotz/GSC	 5/90	Fixed for sub-directory case		*
C* K. Tyle/GSC		 7/96	Renamed from NT_SAVF			*
C* D.W.Plummer/NCEP	 6/97	Increased string length from 72 to 128	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	file, filnam
C*
	CHARACTER	fff*128
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get default if file name is blank.
C
	IF  ( file .eq. ' ' )  THEN
	    filnam = cprogm
	  ELSE
	    filnam = file
	END IF
C
C*	If there is no file type, add .NTS .
C
	ibrc = 1
	idot = INDEX ( filnam ( ibrc: ), '.' )
	IF  ( idot .eq. 0 )  THEN
	    CALL ST_LSTR  ( filnam, lenf, ier )
	    fff    = filnam
	    filnam = fff ( : lenf ) // '.NTS'
	END IF
C*
	RETURN
	END
