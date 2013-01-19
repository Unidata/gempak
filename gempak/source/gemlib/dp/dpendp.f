	SUBROUTINE DP_ENDP  ( ipkno, iret )
C************************************************************************
C* DP_ENDP								*
C*									*
C* This subroutine releases a packing number for the DP library.	*
C*									*
C* DP_ENDP  ( IPKNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IPKNO		INTEGER	 	Packing number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return 		*
C*					 -1 = invalid packing number	*
C**									*
C* Log:									*
C* I. Graffman/RDS	4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dpcmn.cmn'
C------------------------------------------------------------------------
	IF  ( ( ipkno  .lt. 1 ) .or. ( ipkno .gt. MMFLDP ) )  THEN
	    iret = -1
	  ELSE
	    iflgdp (ipkno) = 0
	END IF
C*
	RETURN
	END
