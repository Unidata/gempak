	SUBROUTINE MR_ORDR  ( nlev, ipt, stndat, idtype, iret )
C************************************************************************
C* MR_ORDR								*
C*									*
C* This subroutine uses the ordered pointers to data to reorder the	*
C* data within the station data array.					*
C*									*
C* MR_ORDR  ( NLEV, IPT, STNDAT, IDTYPE, IRET )				*
C*									*
C* Input parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* K. Brill/NMC		01/92	Start loop at 2 not 1			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		stndat (6,*)
	INTEGER		ipt (*), idtype (*)
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through each level.
C
	DO  i = 2, nlev
C
C*	    K is the pointer to the data that should be at the i'th level.
C
	    k = ipt (i)
C
C*	    Find the Kth level of data for swapping.
C
	    IF  ( k .ne. i )  THEN
		found = .false.
		j = i + 1
		DO WHILE ( .not. found )
		    IF  ( ipt (j) .eq. i ) THEN
			found = .true.
		      ELSE
			j = j + 1
			IF  ( j .gt. nlev ) THEN
			    found = .true.
			    j = 0
			END IF
		    END IF
		END DO
C
C*		Swap the data and pointers.
C
		IF  ( j .gt. 0 ) THEN
		    ip = ipt (i)
		    ipt (i) = ipt (j)
		    ipt (j) = ip
		    d1 = stndat ( 1, k )
		    d2 = stndat ( 2, k )
		    d3 = stndat ( 3, k )
		    d4 = stndat ( 4, k )
		    d5 = stndat ( 5, k )
		    d6 = stndat ( 6, k )
		    stndat ( 1, k ) = stndat ( 1, i )
		    stndat ( 2, k ) = stndat ( 2, i )
		    stndat ( 3, k ) = stndat ( 3, i )
		    stndat ( 4, k ) = stndat ( 4, i )
		    stndat ( 5, k ) = stndat ( 5, i )
		    stndat ( 6, k ) = stndat ( 6, i )
		    stndat ( 1, i ) = d1
		    stndat ( 2, i ) = d2
		    stndat ( 3, i ) = d3
		    stndat ( 4, i ) = d4
		    stndat ( 5, i ) = d5
		    stndat ( 6, i ) = d6
		    itemp           = idtype (k)
		    idtype (k)      = idtype (i)
		    idtype (i)      = itemp
		END IF
	    END IF
	END DO
C*
	RETURN
	END
