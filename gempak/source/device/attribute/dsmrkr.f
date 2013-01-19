	SUBROUTINE DSMRKR  ( imark, imkhw, szmark, imkwid, jmark, 
     +			     jmkhw, size, jmkwid, iret )
C************************************************************************
C* DSMRKR								*
C* 									*
C* This subroutine sets the marker attributes including the marker	*
C* number, the hardware/software flag and the marker size/width 	*
C* multipliers.								*
C*									*
C* DSMRKR  ( IMARK, IMKHW, SZMARK, IMKWID, JMARK, JMKHW, SIZE, JMKWID,	*
C*           IRET )							*
C* 									*
C* Input parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*					  <=0 = no change		*
C*	IMKHW		INTEGER		Sw/hw marker flag		*
C*					  1 = software marker		*
C*					  2 = hardware markers		*
C*	SZMARK		REAL		Marker size multiplier		*
C* 	IMKWID		INTEGER		Marker width multiplier		*
C*									*
C* Output parameters:							*
C*	JMARK		INTEGER		Marker number			*
C*	JMKHW		INTEGER		Sw/hw flag			*
C*	SIZE		REAL		Marker size multiplier		*
C* 	JMKWID		INTEGER		Marker width multiplier		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added marker width			*
C* M. Linda/GSC		 8/96	Increased NNMARK from 16 to 21		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* D. Kidwell/NCEP	12/99	Increased NNMARK from 21 to 22          *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	PARAMETER	( NNMARK = 22 )
C------------------------------------------------------------------------
	iret   = NORMAL
	jmark  = mmark
	jmkhw  = mmkhw
	size   = tmksz
	jmkwid = mmkwid
C
C*	Set new marker characteristics.
C
	IF  ( imark .gt. 0 )  jmark = imark
	IF  ( jmark .gt. NNMARK )  jmark = 1
	IF  ( szmark .gt. 0. )  size = szmark
	IF  ( imkwid .gt. 0  )  jmkwid = imkwid
C
C*	Check hardware flag.
C
	IF  ( imkhw .eq. 1 )  THEN
	    jmkhw = 1
	  ELSE IF  ( imkhw .eq. 2 )  THEN
	    IF  ( nmkhw .eq. 1 )  THEN
	        jmkhw = 2
	      ELSE
	        jmkhw = 1
	    END IF
	END IF
C
C*	Set in device if characteristics have changed.
C
	IF  ( ( ( jmark .ne. mmark ) .or.  ( jmkhw .ne. mmkhw ) .or.
     +          ( jmkwid .ne. mmkwid ) .or.
     +		( size  .ne. tmksz ) ) .and. ( jmkhw .eq. 2 ) )  THEN
	    CALL HSMRKR  ( jmark, size, iret )
	END IF
C
C*	Common block of active device characteristics set.
C
	mmark  = jmark
	mmkhw  = jmkhw
	tmksz  = size
	mmkwid = jmkwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSMARK ( jmark, jmkhw, size, jmkwid, iret )
	END IF
C*
	RETURN
	END
