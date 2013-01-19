	SUBROUTINE AW_GDEF ( bdata, irept, iff, iret )
C************************************************************************
C* AW_GDEF                                                              *
C*                                                                      *
C* This subroutine is the vector graphic product definition block.  The *
C* Earth locatable vector graphic data applies to the vector lines, 	*
C* data plots, wind barbs, and line labels.  This subroutine represents *
C* mode 4, submode 20.                    				*
C* 								        *
C* AW_GDEF ( BDATA, IREPT, IFF, IRET ) 				*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added assignment do loop                *
C* A. Hardy/GSC          3/99   Updated prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changbed b to bdata			*
C************************************************************************
	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        INTEGER         icnt, piset, crdfg, scfint, scfrac, 
     +                  areacd, lblcod, mrftwo, nrftwo
        COMMON          mrftwo, nrftwo       
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In GDEF'
	END IF 
 	iret = 0
	icnt = irept + 4
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the background projection and coordinate flag.
C
        piset = ( ibits ( ibte ( icnt ), 0, 8 ) )
	crdfg = ( ibits ( ibte ( icnt+1 ), 0, 8 ) )
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,50)'PROJECTION INDICATOR','COORDINATE FLAG'
	    write(flun,51)piset, crdfg
 50         format(a20,5x,a20)
 51         format(11x,i2,25x,i2)
	END IF 
C
C*      Get the scale factor.
C
        scfint = ( ibits ( ibte( icnt ), 0, 8 ) )
        scfrac = ( ibits ( ibte( icnt+1 ), 0, 8 ) )
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,50)'SCALE INTEGER','SCALE FRACTION'
	    write(flun,51)scfint, scfrac
	END IF 
C
C*      Get the area code and label code.
C
        areacd = ( ibits ( ibte( icnt ), 0, 8 ) )
        lblcod = ( ibits ( ibte( icnt+1 ), 0, 8 ) )
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,50)'AREA CODE','LABEL CODE'
	    write(flun,51)areacd, lblcod
	END IF 
C
C*      Get reference coordinates.
C 
        CALL AW_ADBT (bdata, icnt, mrfone, iret )
	icnt = icnt + 2
        CALL AW_ADBT (bdata, icnt, nrfone, iret )
	icnt = icnt + 2
            IF ( areacd .gt. 20 ) THEN
                CALL AW_ADBT (bdata, icnt, mrftwo, iret )
	        icnt = icnt + 2
                CALL AW_ADBT (bdata, icnt, nrftwo, iret )
	        icnt = icnt + 2
		IF ( areacd .gt. 30 ) THEN
                    CALL AW_ADBT (bdata, icnt, mrflst, iret )
	            icnt = icnt + 2
                    CALL AW_ADBT (bdata, icnt, nrflst, iret )
	            icnt = icnt + 2
		END IF
             END IF
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,52)'1stM','1stN','2ndM','2ndN','3rdM','3rdN'
	    write(flun,53)mrfone, nrfone, mrftwo, nrftwo, mrflst, nrflst
 52         format(a5,5x,a5,5x,a5,5x,a5,5x,a5,5x,a5)
 53         format(i5,5x,i5,5x,i5,5x,i5,5x,i5,5x,i5)
	END IF 
C
C*      Get valid time of product.
C 
        imon = ( ibits ( ibte( icnt ), 0, 8 ) )
	iday = ( ibits ( ibte( icnt + 1 ), 0, 8 ) )
	ihour= ( ibits ( ibte( icnt + 2 ), 0, 8 ) )
	imin = ( ibits ( ibte( icnt + 3 ), 0, 8 ) )
	icnt = icnt + 4
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,54)'MONTH','DAY','HOUR','MIN OF VALID TIME'
	    write(flun,55)imon, iday, ihour, imin
 54         format(a5,5x,a5,5x,a5,5x,a20)
 55         format(i5,5x,i5,5x,i5,5x,i5)
	END IF 
C
C*      Get end of valid time.
C 
        ievmn = ( ibits ( ibte( icnt ), 0, 8 ) )
	ievdy = ( ibits ( ibte( icnt + 1 ), 0, 8 ) )
	ievhr = ( ibits ( ibte( icnt + 2 ), 0, 8 ) )
	ievmn = ( ibits ( ibte( icnt + 3 ), 0, 8 ) )
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,54)'MONTH','DAY','HOUR','END MIN VALID TIME'
	    write(flun,55)ievmn, ievdy, ievhr, ievmn
	END IF 
C
C*      Check for checksum. 
C
        IF ( dbug .eq. 'y' ) THEN
          IF ( iff .eq. 0 ) write(flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
