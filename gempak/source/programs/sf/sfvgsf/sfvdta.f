	SUBROUTINE SFVDTA  ( vgfile, isffln, dattim, nparm, iploc,
     +			     icolr, iret )
C************************************************************************
C* SFVDTA								*
C*									*
C* This subroutine reads data from a Vector Graphics file and writes it	*
C* to a surface file.							*
C*									*
C* SFVDTA  ( VGFILE, ISFFLN, DATTIM, NPARM, IPLOC, ICOLR, IRET )	*
C*									*
C* Input parameters:							*
C*	VGFILE		CHAR*		Vector graphics file name	*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		Date/time of data		*
C*	NPARM		INTEGER		Number of parameters		*
C*	IPLOC (NPARM)	INTEGER		Data location in output array	*
C*	ICOLR (NPARM)	INTEGER		Color list			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C** Log:								*
C* S. Jacobs/NCEP	 2/99						*
C* S. Jacobs/NCEP	 3/99	Change call to SFVVGF and SFVGET	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	vgfile, dattim
	INTEGER		iploc (*), icolr (*)
C*
	CHARACTER	tim*20, systim*20, stn*8, stid*8, vgfl(2)*160,
     +			curtim*20
	REAL		rdata (MMPARM), data (MMPARM)
	INTEGER		itype
C------------------------------------------------------------------------
	iret = 0
C
C*	Standardize the input time using the system clock.
C
	itype = 1
	CALL CSS_GTIM ( itype, systim, ier )
	CALL TI_STAN ( dattim, systim, tim, ier )
	curtim = ' '
C
C*	Open and scan the VG file. Return the number of groups/stations
C*	found in the file.
C
	CALL ST_CLST ( vgfile, '|', ' ', 2, vgfl, num, ier )
	CALL ST_LSTR ( vgfl(1), lenv, ier )
	CALL SFVVGF ( vgfl(1), lenv, nparm, iploc, icolr, ngrps, ier )
C
C*	Loop over the number of groups.
C
	DO  j = 1, ngrps
C
C*	    Get the data for the next group/station.
C
	    CALL SFVGET ( j, nparm, stid, lens, rdata, ier )
C
	    IF  ( lens .gt. 0 )  THEN
		stn = stid (:lens)
C
C*	    	Set the proper station, then write the data.
C
		CALL SFVSTN ( isffln, stn, tim, curtim, data,
     +			      ihhmm, iret )
		IF  ( iret .lt. 0 )  THEN
		    RETURN
		  ELSE IF  ( iret .eq. 0 )  THEN
		    DO  i = 1, nparm
			IF  ( iploc (i) .gt. 0 )  THEN
			    data ( iploc (i) ) = rdata (i)
			END IF
		    END DO
		    CALL SF_WDAT  ( isffln, ihhmm, data, ier )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
