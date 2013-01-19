	SUBROUTINE SFMOPT  ( datfil, chtim, device, proj, area, garea,
     +			     nparm, prcons, icolr, map, title, clear,
     +			     filtfc, kprc, panel, ccvals, icclrs, 
     +			     numccc, icrprm, iaddcl, iret )
C************************************************************************
C* SFMOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SFMOPT  ( DATFIL, CHTIM, DEVICE, PROJ, AREA, GAREA, NPARM, PRCONS,	*
C*           ICOLR, MAP, TITLE, CLEAR, FILTFC, KPRC, PANEL,		*
C*           CCVALS, ICCLRS, NUMCCC, ICRPRM, IADDCL, IRET )		*
C*									*
C* Input parameters:							*
C*	DATFIL		CHAR*		Data file name			*
C*	CHTIM		CHAR*		Time 				*
C*	DEVICE		CHAR*		Device 				*
C*	PROJ		CHAR*		Projection 			*
C*	AREA		CHAR*		Area name			*
C*	GAREA		CHAR*		Graphics area name		*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PRCONS(*)	CHAR*		Parameters 			*
C*	ICOLR(*)	INTEGER		Colors 				*
C*	MAP		CHAR*		Map options			*
C*	TITLE		CHAR*		Title				*
C*	CLEAR		LOGICAL		Clear flag			*
C*	FILTFC		REAL		Filter factor			*
C*	KPRC		INTEGER		Repeat plot counter		*
C*	PANEL		CHAR*		Panel input			*
C*      CCVALS (*)      REAL            Values for color coding         *
C*      ICCLRS (*)      INTEGER         Colors for color coding         *
C*      NUMCCC (*)      INTEGER         Number of colors for coding     *
C*      ICRPRM (*)      INTEGER         Pointers to reference parms     *
C*	IADDCL		INTEGER		Number of added reference parms *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	 6/88	Changed					*
C* S. Schotz/GSC	 5/90   Removed call to TM_WMSG			*
C* S. Schotz/GSC	 5/90	Don't display BLNK in first slot 	*
C* S. Schotz/GSC	 5/90	Get respond flag from IP_RESP		*
C* S. Schotz/GSC	 8/90	Write out conditions, removed SCALE	*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		11/91	Declared FILT as CHAR			*
C* K. Brill/NMC		12/91	Display all parameters			*
C* K. Brill/NMC		02/92	Put parms in 4-char array for output	*
C* K. Brill/NMC		03/92	F -> F5.2				*
C* S. Jacobs/NMC	 6/94	Aligned the output for the filter factor*
C* D. Kidwell/NCEP       2/98	Display color coding info               *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		ccvals (*)
	CHARACTER*(*)	datfil, chtim, proj, area, garea, device
	CHARACTER*(*)	prcons (*), map, title, panel
	INTEGER		icolr (*), icclrs (*), numccc (*), icrprm (*)
	LOGICAL		respnd, clear
C*
     	CHARACTER	msg*33, clr*3, filt*3
	CHARACTER	prmo (MMPARM)*4
C------------------------------------------------------------------------
	iret = 0
	IF  ( nparm .eq. 0 )  THEN
	    np = 1
	    prcons (1) = ' '
	  ELSE 
	    np = nparm - iaddcl
	END IF
	IF  ( clear )  THEN
	    clr = 'YES'
	  ELSE
	    clr = 'NO'
	END IF
	IF  ( filtfc .ne. 0. )  THEN
	    filt = 'YES'
	  ELSE
	    filt = 'NO'
	END IF
C
	IF  ( kprc .gt. 1 )  THEN
	    msg = 'Next plot - Time: ' // chtim
	    WRITE ( 6, *) msg
	  ELSE
	    WRITE ( 6, 5000 ) area, garea
            DO  i = 1, nparm
		prmo (i) = prcons (i)
            END DO
	    WRITE ( 6, 5010 ) ( prmo (i) , i = 1, np )
	    WRITE ( 6, 5020 ) ( icolr (j), j = 1, np )
C
	    ilast = 0
	    iend = 0
	    DO  i = 1, np
		IF ( icolr (i) .lt. 0 ) THEN
		    indx = IABS ( icolr (i) )
		    IF ( indx .gt. ilast ) THEN
			istrt = iend + 1
			iend = iend + numccc (indx)
			DO j = istrt, iend, 6
			    jend = MIN ( iend, j + 5 )
			    WRITE ( 6, 5040 ) prmo (i), 
     +				       ( icclrs (k), k = j, jend )
			    IF ( jend .eq. iend ) jend = jend - 1
			    WRITE ( 6, 5050 ) prmo ( icrprm (indx) ), 
     +				       ( ccvals (k), k = j, jend )
			END DO
			ilast = indx
		    END IF
		END IF
	    END DO
C
	    WRITE ( 6, 5030 ) chtim, datfil(1:60), map, title,
     +			      device, proj, clr, filt, filtfc,
     +			      panel
	END IF
	CALL IP_RESP ( respnd, ier)
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
5000	FORMAT ( ' SFMAP PARAMETERS:',//
     +           ' Data area:          ', A, /
     +           ' Graphics area name: ', A )
5010	FORMAT ( ' Valid parameters:   ' , 10 ( 1X, A4 ) )
5020	FORMAT ( ' Parameter colors:  ' ,  10 ( I4, ' ' ) )
5030	FORMAT ( ' Time:              ', A,/
     +           ' File:              ', A,/
     +           ' Map:               ', A,/
     +           ' Title:             ', A,/
     +           ' Device:            ', A,/ 
     +           ' Projection:        ', A,/
     +           ' Clear screen:      ', A,/
     +           ' Filter:            ', A,/
     +           ' Filter factor:     ', F5.2,/
     +           ' Panel:             ', A )
5040	FORMAT ( '     Color coding:  ', 1X, A4, 6 ( I4, 4X ) )
5050 	FORMAT ( '   Reference parm:  ', 1X, A4, 1X, 6F8.2 )
C*
	RETURN
	END
