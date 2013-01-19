	SUBROUTINE SS_ENVR  ( filnam, file, iret )
C************************************************************************
C* SS_ENVR								*
C* 									*
C* This subroutine checks filnam for an environmental variable.  If	*
C* one is found, it is replaced with the actual path name.  The new	*
C* file name is returned.  Environmental variables are identified in	*
C* two ways:								*
C*	names beginning with $ and terminated with a /			*
C*	names which do not begin with $, but are terminated by a :	*
C*									*
C* SS_ENVR  ( FILNAM, FILE, IRET )					*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C* 									*
C* Output parameters:							*
C*	FILE		CHAR*		Translated file name		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no variable name		*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 5/91						*
C* M. desJardins/NMC	 1/92	Doc; "-->' ;  reorganize;		*
C* S. Jacobs/NMC	 3/94	Increased size of symval to 72 char	*
C* M. Linda/GSC		 4/96	Increased symval and tmpnam to 132	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam, file
C*
	CHARACTER	symval*132, envirn*48, tmpnam*132
C------------------------------------------------------------------------
	iret = -1
	CALL ST_LSTR  ( filnam, lenf, ier )
C
C*	Find the part of the name which is an environmental variable.
C
	iposd = INDEX ( filnam, '$' )
	iposc = INDEX ( filnam, ':' )
	iposs = INDEX ( filnam, '/' )
C
C*	Check for (1) $ and /; (2) $ with no /; (3) : .
C
	IF  ( ( iposd .eq. 1 ) .and. ( iposs .gt. 0 ) )  THEN
	    envirn = filnam ( 1 : iposs-1 )
	    tmpnam = filnam ( iposs+1 : )
	  ELSE IF  ( iposd .eq. 1 )  THEN
	    envirn = filnam
	    tmpnam = ' '
	  ELSE IF  ( iposc .gt. 1 )  THEN
	    envirn = '$' // filnam ( 1 : iposc-1 )
	    tmpnam = filnam ( iposc+1 : )
	  ELSE
	    file = filnam
	    iret = -1
	    RETURN
	END IF
C
C*	Translate the environmental name.
C
	CALL ST_LCUC  ( envirn, envirn, ier )
	CALL SS_GSYM  ( envirn, symval, ier )
C
C*	If the path name was found, append to rest of file name.
C
	IF  ( ier .eq. 0 )  THEN
	    iret = 0
	    CALL ST_LSTR  ( symval, ilens, ier )
	    file = symval ( : ilens ) // '/' // tmpnam
	  ELSE
	    file = filnam
	    iret = -1
	END IF
C*
	RETURN
	END
