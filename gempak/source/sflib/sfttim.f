	SUBROUTINE SF_TTIM  ( isffln, dattim, iret )
C************************************************************************
C* SF_TTIM								*
C*									*
C* This subroutine sets the time in a surface file.  SF_TSTN must	*
C* be called before this subroutine is called.  Data for this time 	*
C* can be read or written by calling SF_RDAT or SF_WDAT, respectively.	*
C*									*
C* SF_TTIM  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				   	   0 = normal return		*
C*				  	  -3 = file not open		*
C*					 -11 = time not found		*
C*					 -18 = station not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*) 	dattim
C*
	CHARACTER	keynam (2)*4
	INTEGER		lowval (2), ihival (2)
	DATA		keynam / 'DATE', 'TIME' /
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station has been set.
C
	IF  ( .not. stnset (isffln) )  THEN
	    iret = -18
	    RETURN
	END IF
C
C*	Reset search  conditions.
C
	CALL SF_BEGS  ( isffln, ier )
C
C*	Delete existing search condition.
C
	CALL DM_DCSR  ( isffln, ier )
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM  ( dattim, lowval (1), lowval (2), ier )
	DO  i = 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Establish search.
C
	CALL DM_CSRC  ( isffln, .true., 2, keynam, lowval, ihival, ier )
C
C*	Find next time meeting search criteria.
C
	CALL SF_TNXT  ( isffln, dattim, iret )
	IF  ( iret .eq. -9 )  iret = -11
C
C*	Delete search condition.
C
	CALL DM_DCSR  ( isffln, ier )
C*
	RETURN
	END
