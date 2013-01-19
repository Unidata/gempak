	SUBROUTINE GSMRKR  ( imark, imkhw, szmark, imkwid, iret )
C************************************************************************
C* GSMRKR								*
C* 									*
C* This subroutine sets the marker attributes including the marker	*
C* number, the hardware/software flag and the marker size/width 	*
C* multipliers.								*
C*									*
C* NOTE THAT THE CALLING SEQUENCE FOR THIS SUBROUTINE HAS CHANGED FROM	*
C* PREVIOUS VERSIONS OF GEMPAK.						*
C*									*
C* GSMRKR  ( IMARK, IMKHW, SZMARK, IMKWID, IRET )			*
C* 									*
C* Input parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*					  <=0 = no change		*
C*	IMKHW		INTEGER		Sw/hw marker flag		*
C*					  1 = software			*
C*					  2 = hardware			*
C*	SZMARK		REAL		Marker size multiplier		*
C*	IMKWID		INTEGER		Marker line width		*
C*					  <= 0 = no change		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added marker width			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C* 	Check if these are current requested characteristics.
C
	IF  ( ( ( imark .eq. kmark ) .or. ( imark .le. 0 ) ) .and.
     +	      ( ( imkhw .eq. kmkhw ) .or. ( imkhw .lt. 1 ) .or. 
     +					  ( imkhw .gt. 2 ) ) .and.
     +	      ( ( szmark .eq. rmksz ) .or. ( szmark .le. 0. ) ) .and. 
     +        ( ( imkwid .eq. kmkwid) .or. ( imkwid .lt. 1 ) ) )  THEN
C
C*	    Set requested characteristics.
C
	  ELSE
	    IF  ( imark .gt. 0 )  kmark = imark
	    IF  ( ( imkhw .ge. 1 ) .and. ( imkhw .le. 2 ) ) 
     +						kmkhw = imkhw
	    IF  ( szmark .gt. 0. )  rmksz = szmark
            IF  ( imkwid .ge. 1  )  kmkwid = imkwid
C
C*	    Send characteristics to device if not already set.
C
	    IF  ( ( ( kmark .ne. lmark ) .or.  ( kmkhw .ne. lmkhw ) .or.
     +		  ( rmksz .ne. smksz )  .or. ( kmkwid .ne. lmkwid ) )
     +            .and. ( ddev .ne. ' ' ) )  THEN
	        CALL DSMRKR  ( kmark, kmkhw, rmksz, kmkwid, lmark, 
     +			       lmkhw, smksz, lmkwid, iret )
	    END IF
	END IF
C*
	RETURN
	END
