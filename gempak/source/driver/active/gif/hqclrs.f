	SUBROUTINE HQCLRS  ( itype, ncolrs, iret )
C************************************************************************
C* HQCLRS - GIF 							*
C* 									*
C* This subroutine returns the number of colors in a color bank.  	*
C* 									*
C* HQCLRS  ( ITYPE, NCOLRS, IRET )					*
C* 									*
C* Input parameters:							*
C*      ITYPE           INTEGER         Image type                      *
C*                                          1 = satellite               *
C*                                          2 = radar                   *
C*                                          3 = fax                     *
C* Output parameters:							*
C*	NCOLRS		INTEGER		Number of colors		*
C*	IRET		INTEGER		Return code			*
C*					0 = normal			*
C*					-1 = invalid image type		*
C**									*
C* Log:									*
C* J. Nielsen-G/TAMU	 8/98	Created					*
C* T. Lee/GSC		 7/00	Removed unnecessary include files	*
C* R. Tian/SAIC		05/02	Added fax image type			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'driver.cmn'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set the color range
C
	IF  ( itype .EQ. 1 )  THEN
	    ncolrs  = nbwsat
	ELSE IF  ( itype .EQ. 2 )  THEN
	    ncolrs  = nbwrad
	ELSE IF  ( itype .EQ. 3 )  THEN
	    ncolrs  = nbwfax
	ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
