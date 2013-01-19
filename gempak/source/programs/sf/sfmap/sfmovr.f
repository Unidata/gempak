	SUBROUTINE SFMOVR  ( sx, sy, ox, oy, npt, offxy, iret )
C************************************************************************
C* SFMOVR								*
C*									*
C* This subroutine determines if a station overlaps earlier stations.	*
C*									*
C* SFMOVR  ( SX, SY, OX, OY, NPT, OFFXY, IRET )				*
C*									*
C* Input parameters:							*
C*	SX		REAL		Station x (N coord)		*
C*	SY		REAL		Station y (N coord)		*
C*	OX (NPT)	REAL		Previous x (N coord)		*
C*	OY (NPT)	REAL		Previous y (N coord)		*
C*	NPT		INTEGER		Number of previous stations	*
C*	OFFXY(*)	REAL		Size of station model (N coord)	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = no overlap		*
C*					 -1 = overlap			*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	 6/88	Added looping within program		*
C* G. Huffman/GSC	 1/89	N coord., change IER to IRET		*
C************************************************************************
	REAL		offxy (*), ox (*), oy (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Set bounds for station.
C
	gslef = sx - offxy (1)
	gsrt  = sx + offxy (2)
	gsup  = sy + offxy (3)
	gsdwn = sy - offxy (4)
C
C*	Loop through all stations.
C
	istn = 1
	DO WHILE  ( ( istn .le. npt ) .and. ( iret .eq. 0 ) )
C
C*	    Set bounds for possible overlapping station.
C
	    oslef = ox (istn) - offxy (1)
	    osrt  = ox (istn) + offxy (2)
	    osup  = oy (istn) + offxy (3)
	    osdwn = oy (istn) - offxy (4)
C
C*	    Check for overlap
C
	    IF  ((((oslef .ge. gslef) .and. (oslef .le. gsrt)) .or.
     +		  ((osrt  .ge. gslef) .and. (osrt  .le. gsrt))) .and.
     +		 (((osup  .ge. gsdwn) .and. (osup  .le. gsup)) .or.
     +		  ((osdwn .ge. gsdwn) .and. (osdwn .le. gsup))))  THEN
		iret = -1
	    END IF
C*
	    istn = istn + 1
	END DO
C*
	RETURN
	END
