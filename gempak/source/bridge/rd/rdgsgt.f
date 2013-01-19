	SUBROUTINE RD_GSGT ( btin, lenb, ibpnt, segmnt, lens, iret )
C************************************************************************
C* RD_GSGT								*
C*									*
C* This subroutine gets a segment, skipping over local effects.  '&&'	*
C* signifies the beginning of a local effect.				* 
C*									*
C* RD_GSGT  ( BTIN, LENB, IBPNT, SEGMNT, LENS, IRET )			*
C*									*
C* Input parameters:							*
C*	BTIN		CHAR*		ST_UNPRed Bulletin		*
C*	LENB		INTEGER		Length of bulletin		*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Bulletin pointer		*
C*									*
C* Output parameters:							*
C*      SEGMNT		CHAR*		Segment				*
C*      LENS		INTEGER		Length of segment		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = segment not found	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C************************************************************************
C*
	CHARACTER*(*)	btin, segmnt
C------------------------------------------------------------------------
	iret = 0
	lens = 0
C*
	ip = ibpnt
	IF ( ip .lt. lenb ) THEN
	    ipdelm = INDEX ( btin ( ip:lenb ), '$$' )
	    ipdele = INDEX ( btin ( ip:lenb ), '&&' )
	    IF ( ipdelm .ne. 0 ) THEN
	 	IF ( ipdelm .lt. ipdele .or. ipdele .eq. 0 ) THEN
		    lens = ipdelm - 1	
		  ELSE
		    lens = ipdele - 1
		END IF
		ibpnt = ip + ipdelm + 1
	      ELSE
		IF ( ipdele .ne. 0 ) THEN
		    lens = ipdele - 1
		  ELSE
		    lens = lenb - ip
		END IF
		ibpnt = lenb
	    END IF
	  ELSE
	    iret = -1
	END IF
	segmnt = btin ( ip:ip+lens-1 )
C*
	RETURN
	END
