	SUBROUTINE DSMRKR  ( imark, imkhw, szmark, imkwid, jmark, 
     +			     jmkhw, size, jmkwid, iret )
C************************************************************************
C* DSMRKR								*
C* 									*
C* This subroutine sets the marker attributes including the marker	*
C* number, the hardware/software flag and the marker size/width  	*
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
C* G. Chatters/RDS	 4/84						*
C* M. Vilardo/RDS	 7/84	GEMPLT Version 3.0                      *
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 9/88	Doc and returns				*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 1/90	Added marker width			*
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
	INCLUDE 'DEVACT.CMN'
C*
	INTEGER isend (4), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CSMRKR
	isend (3) = imark
	isend (4) = imkhw
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szmark, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( imkwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
C
C*	    Get the marker output variables.
C
	    CALL GGET  ( ircv, 2, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    jmark = ircv (1)
	    jmkhw = ircv (2)
C
	    CALL GGETR  ( size, 1, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
	    CALL GGET  ( jmkwid, 1, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
C*	    Fill the ACTIVE common block variables.
C
	    mmark = ircv (1)
	    mmkhw = ircv (2)
	    tmksz = size
	    mmkwid = jmkwid
	END IF
C*
	RETURN
	END
