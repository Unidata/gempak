	SUBROUTINE OACDTA  ( isffln, nparms, nstn, sfdata, slat, slon,
     +			     stnam, cosslt, iret )
C************************************************************************
C* OACDTA								*
C*									*
C* This subroutine reads in the surface data for OABSFC.		*
C*									*
C* OACDTA  ( ISFFLN, NPARMS, NSTN, SFDATA, SLAT, SLON, STNAM,		*
C*	     COSSLT, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NPARMS		INTEGER		Number of parameters		*
C*									*
C* Input and output parameters:						*
C*	NSTN		INTEGER       	Number of stations 		*
C*									*
C* Output parameters:							*
C*	SFDATA		REAL		Surface data			*
C*	 (NPARMS,NSTN)							*
C*	SLAT (NSTN)	REAL		Station latitude		*
C*	SLON (NSTN)	REAL		Station longitude		*
C*	STNAM (NSTN)	CHAR*		Station ID or name		*
C*	COSSLT (NSTN)	REAL		Cosine of latitude		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = too many stations		*
C*					 -7 = too few stations		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* K. Brill/GSC          4/90   Multiple files				*
C* K. Brill/NMC          8/90   Fix for multiple files, duplicate stns	*
C* K. Brill/NMC		 3/91	Allow OA of station parameters		*
C* K. Brill/NMC		06/91	Added COSSLT				*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SNXT     *
C* T. Lee/GSC		 3/99	Added STNAM in call seq.; Increased	*
C*				LLSTFL -> LLDTMX			*
C* A. Hardy/GSC		 3/99	Removed ispri = 0			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( LLDTMX = LLSTFL * MMFILE )
	REAL		sfdata (*), slat (*), slon (*), cosslt (*)
	CHARACTER*(*)	stnam  (*)
C*
	LOGICAL		done, found
	CHARACTER	stid*8, cstnm*8, cdata ( MMPARM )
	REAL		stndat ( MMPARM ), data ( MMPARM )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	idata = nstn * nparms
	done  = .false.
	idup = 0
C
C*	  Loop through each of the stations.
C
	  DO WHILE  ( .not. done )
C
C*	    Read in header for the next station.
C
	    CALL SF_SNXT  ( isffln, stid, istnm, rlat, rlon,
     +			      relv, ispri, ier )
C
C*	    Check for end of file.
C
	    IF  ( ier .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Read in the data.
C
		CALL SF_RDAT  ( isffln, stndat, ihhmm, ier )
		IF  ( ier .eq. 0 )  THEN
C
C*		    Set station and compute output data.
C
		    CALL PC_SSTN  ( stid, istnm, rlat, rlon, relv,
     +				    ispri, ihhmm, 1, ier )
		    CALL PC_CMVS  ( 0., 0, stndat, data, cdata, ier )
C
C*		    Check that all data is not missing.
C
		    idat = 0
		    DO  ij = 1, nparms
			IF  ( .not. ERMISS ( data (ij) ) )  idat = 1
		    END DO
C
C*		    Add data to list.
C
		    IF  ( ( ier .eq. 0 ) .and. ( idat .eq. 1 ) )  THEN
C
C*			Check that another station can be added.
C
			IF  ( nstn .lt. LLDTMX )  THEN
C
C*			    Check for duplicate station.
C
			    iq = 1
			    found = .false.
	                    DO WHILE ( iq .le. nstn .and. .not. found )
	                      IF ( rlat .eq. slat (iq) .and.
     +                             rlon .eq. slon (iq) ) found = .true.
	                      iq = iq + 1
	                    END DO
C
C*			    Save station information.
C
		            IF ( .not. found ) THEN
			      nstn = nstn + 1
			      slat (nstn) = rlat
			      cosslt (nstn) = COS ( rlat * DTR )
			      slon (nstn) = rlon
			      DO  ij = 1, nparms
				idata = idata + 1
				sfdata (idata) = data (ij)
			      END DO	
			      IF  ( stid .eq. ' ' )  THEN
				CALL ST_INCH ( istnm, cstnm, ier )
				IF ( cstnm .ne. '999999' )  THEN
				    stnam (nstn) = cstnm 
				ELSE
				    stnam (nstn) = ' '
				END IF
			      ELSE
				stnam (nstn) = stid
			      END IF
			    ELSE
	                      idup = idup + 1
	                    END IF
			  ELSE
C
C*			    Write error message for too many stations.
C
			    iret = -6
			    CALL ER_WMSG  ( 'OABSFC', iret, ' ', ier )
			END IF
		    END IF
		END IF
	    END IF
	  END DO
	IF  ( nstn .lt. 4 )  THEN
	    iret = -7
	    CALL ER_WMSG  ( 'OABSFC', iret, ' ', ier )
	END IF
	IF ( idup .ne. 0 ) THEN
	   WRITE (6, 1000) idup
1000	   FORMAT ( ' ', i5,' duplicate stations eliminated. ')
	END IF
C*
	RETURN
	END
