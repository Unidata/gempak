	SUBROUTINE POLY_CAP ( lunp, nc, npo, nclip, rlat, rlon, iret )
C************************************************************************
C* POLY_CAP  								*
C*									*
C* This subroutine creates CAP messages.				*
C*									*
C* POLY_CAP ( LUNP, NC, NPO, NCLIP, RLAT, RLON, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNP		INTEGER		LUN for CAP message file	*
C*	NC		INTEGER		Warning categories:		*
C*					  1: Hurricane			*
C*					  2: Storm			*
C*					  3: Gale			*
C*					  4: Small craft advisory	* 
C*	NPO		INTEGER		Nth polygon			*
C*	NCLIP		INTEGER		No. of vertices on a polygon	*
C*	RLAT(NCLIP)	REAL		Latitudes			*
C*	RLON(NCLIP)	REAL		Longitudes			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot open file		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 5/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	REAL		rlat(nclip), rlon(nclip)
	CHARACTER	area*6, areasl*7, clat*10
	CHARACTER	info*6, infosl*7, poly*9, polysl*10, clon*10
	CHARACTER	sp2*2, sp4*4, sp6*6, eol*2, comma*2, indent*9
	CHARACTER	outstr*5000, dattim*12, cyyyy*4
	CHARACTER*60	alert(2), alrtsl, areads, catgry, certan, 
     +			center(2), xcnter, event, headln, id, msgtyp, 
     +			nop, restyp, scope, send, sender, sendnm, 
     +			severe, status, urgncy, warns(4), watch(4), 
     +			web(2), xmlver, power(4), ctime, cname(2), 
     +			xname, cnpo*2, cnc*5
	DATA		alert/'<alert xmlns', 
     +			     '="urn:oasis:names:tc:emergency:cap:1.1">'/
	DATA		alrtsl/'</alert>'/
	DATA		area/'<area>'/, areasl/'</area>'/
	DATA		areads/'<areaDesc>?</areaDesc>'/
	DATA		catgry/'<category>Met</category>'/
	DATA		certan/'<certainty>Likely</certainty>'/
	DATA		headln/'<headline>?</headline>'/
	DATA		id/'<identifier>KWBC1055887203</identifier>'/
	DATA		info/'<info>'/, infosl/'</info>'/
	DATA		event/'<event>?</event>'/
	DATA		msgtyp/'<msgTyp>Alert</msgTyp>'/
	DATA		nop/'<numPoint>?</numPoint>'/
	DATA		poly/'<polygon>'/, polysl/'</polygon>'/
	DATA		restyp/'<responseType>None</responseType>'/
	DATA		scope/'<scope>Public</scope>'/
	DATA		send/'<send>?</send>'/
	DATA		sender/'<sender>?</sender>'/
	DATA		sendnm/'<senderName>?</senderName>'/
	DATA		severe/'<severity>?</severity>'/
	DATA		status/'<status>Actual</status>'/
	DATA		urgncy/'<urgency>Immediate</urgency>'/
	DATA		warns/'HURRICANE FORCE WIND WARNING',
     +			      'STORM WARNING', 'GALE WARNING',
     +			      'SMALL CRAFT ADVISORY'/
	DATA		watch/'HURRICANE FORCE WIND WATCH',
     +			      'STORM WATCH', 'GALE WATCH',
     +			      'SMALL CRAFT ADVISORY'/
	DATA		web/'<web>http://www.opc.ncep.noaa.gov/shtml/',
     +			    'NFDOFFNT2.shtml</web>'/
	DATA		xmlver/'<?xml version="1.0" encoding="UTF-8"?>'/
	DATA		sp2/'  '/, sp4/'    '/, sp6/'      '/
 	DATA		center/	'OCEAN_PREDICTION_CENTER',
     +				'TROPICAL_PREDICTION_CENTER'/
	DATA		power/'Extreme','Severe', 'Moderate', 
     +			      'Moderate'/ 
 	DATA		cname/	'OCEAN PREDICTION CENTER WASHINGTON DC',
     +				'TROPICAL PREDICTION CENTER MIAMI FL'/
C-----------------------------------------------------------------------
	iret = 0
C
	outstr = ''
	eol = CHLF // CHNULL
	comma = ',' // CHNULL
	indent = '        ' // CHNULL
	DO nk = 1, nclip
	    CALL ST_RLCH ( rlat ( nk ), 2, clat, ier )
	    CALL ST_LSTR ( clat, nclat, ier )
	    CALL ST_RLCH ( rlon ( nk ), 2, clon, ier )
	    CALL ST_LSTR ( clon, nclon, ier )
	    CALL ST_LSTR ( outstr, lout, ier )
	    outstr = outstr ( : lout )  // ',' // clat ( : nclat ) 
     +					// ',' // clon ( : nclon )
	    CALL ST_NULL ( outstr, outstr, len, ier )
	    CALL CST_WRAP ( outstr, comma, MXCHAR, eol, indent, outstr,
     +			    ier )
	END DO
C
	CALL ST_LSTR ( xmlver, nxml, ier )
	WRITE ( lunp, 10 ) xmlver ( : nxml )
C
	CALL ST_LSTR ( alert ( 1 ), na1, ier )
	CALL ST_LSTR ( alert ( 2 ), na2, ier )
	WRITE ( lunp, 10 ) alert ( 1 ) ( : na1 ), alert ( 2 ) ( : na2 )
C
	CALL ST_LSTR ( id, nid, ier )
	WRITE ( lunp, 10 ) sp2, id ( : nid )
C
	CALL ST_LSTR ( sender, nsend, ier )
	indx = INDEX ( sender, '?' )
	IF ( idxzon .le. 2 )  THEN
	    xcnter = center ( 1 )
	  ELSE
	    xcnter = center ( 2 )
	END IF
	CALL ST_LSTR ( xcnter, nxcn, ier )
	WRITE ( lunp, 10 ) sp2, sender ( : indx - 1 ), 
     +			   xcnter ( : nxcn ), 
     +			   sender ( indx + 1 : nsend )
C
	CALL ST_LSTR ( send, nsend, ier )
	indx = INDEX ( send, '?' )
	CALL CSS_GTIM ( 1, dattim, ier )
	CALL TI_C2I ( dattim, iyyyy, immdd, ihhmm, ier )
	CALL ST_INCH ( iyyyy, cyyyy, ier )
	ctime = cyyyy // '-' // dattim ( 3 : 4 ) // '-' // 
     +		dattim ( 5 : 6 ) // 'T' // dattim ( 8 : 9 ) //
     +		':' // dattim ( 10 : 11 ) // ':00-00:00'
	CALL ST_LSTR ( ctime, nct, ier )
	WRITE ( lunp, 10 ) sp2, send ( : indx - 1 ), ctime ( : nct ), 
     +			   send ( indx + 1 : nsend )
C
	CALL ST_LSTR ( status, nst, ier )
	WRITE ( lunp, 10 ) sp2, status ( : nst )
C
	CALL ST_LSTR ( msgtyp, nmsg, ier )
	WRITE ( lunp, 10 ) sp2, msgtyp ( : nmsg)
C
	CALL ST_LSTR ( scope, nscope, ier )
	WRITE ( lunp, 10 ) sp2, scope ( : nscope )
C
	CALL ST_LSTR ( info, ninfo, ier )
	WRITE ( lunp, 10 ) sp2, info ( : ninfo )
C
	CALL ST_LSTR ( catgry, ncat, ier )
	WRITE ( lunp, 10 ) sp4, catgry ( : ncat )
C
	CALL ST_LSTR ( event, nevent, ier )
	indx = INDEX ( event, '?' )
	IF ( fwarn )   THEN
	    CALL ST_LSTR ( warns ( nc ), nwarns, ier )
	    WRITE ( lunp, 10 )  sp4, event ( : indx - 1 ), 
     +				warns ( nc ) ( : nwarns ), 
     +				event ( indx + 1 : nevent )
	  ELSE
	    CALL ST_LSTR ( watch ( nc ), nwatch, ier )
	    WRITE ( lunp, 10 )  sp4, event ( : indx - 1 ), 
     +				watch ( nc ) ( : nwatch ), 
     +				event ( indx + 1 : nevent )
	END IF
C
	CALL ST_LSTR ( restyp, nrest, ier )
	WRITE ( lunp, 10 ) sp4, restyp ( : nrest )
C
	CALL ST_LSTR ( urgncy, nurg, ier )
	WRITE ( lunp, 10 ) sp4, urgncy ( : nurg )
C
	CALL ST_LSTR ( severe, nsev, ier )
	indx = INDEX ( severe, '?' )
	CALL ST_LSTR ( power ( nc ), npow, ier )
	WRITE ( lunp, 10 ) sp4, severe ( : indx - 1 ), 
     +			   power ( nc ) ( : npow ), 
     +			   severe ( indx + 1 : nsev ) 
C
	CALL ST_LSTR ( certan, ncert, ier )
	WRITE ( lunp, 10 ) sp4, certan ( : ncert )
C
	CALL ST_LSTR ( sendnm, nsend, ier )
	indx = INDEX ( sendnm, '?' )
	IF ( idxzon .le. 2 )  THEN
	    xname = cname ( 1 )
	  ELSE
	    xname = cname ( 2 )
	END IF
	CALL ST_LSTR ( xname, nx, ier )
	WRITE ( lunp, 10 ) sp4, sendnm ( : indx - 1 ),
     +			   xname ( : nx ), 
     +			   sendnm ( indx + 1 : nsend ) 
C
	CALL ST_LSTR ( headln, nhead, ier )
	indx = INDEX ( headln, '?' )
	IF ( fwarn )  THEN
	    WRITE ( lunp, 10 )  sp4, headln ( : indx - 1 ), 
     +				warns ( nc ) ( : nwarns ), 
     +				headln ( indx + 1 : nhead )
	  ELSE
	    WRITE ( lunp, 10 )  sp4, headln ( : indx - 1 ), 
     +				watch ( nc ) ( : nwatch ), 
     +				headln ( indx + 1 : nhead )
	END IF
	
C
	CALL ST_LSTR ( web ( 1 ), nw1, ier )
	CALL ST_LSTR ( web ( 2 ), nw2, ier )
	WRITE ( lunp, 10 ) sp4, web ( 1 ) ( : nw1 ), web ( 2 ) ( : nw2 )
C
	CALL ST_LSTR ( area, narea, ier )
	WRITE ( lunp, 10 ) sp4, area ( : narea )
C
	CALL ST_LSTR ( areads, naread, ier )
	indx = INDEX ( areads, '?' )
	CALL ST_INCH ( npo, cnpo, ier )
	CALL ST_LSTR ( cnpo, ncnpo, ier )
	WRITE ( lunp, 10 ) sp6, areads ( : indx - 1 ), cnpo ( : ncnpo ),
     +			   areads ( indx + 1 : naread )
C
	CALL ST_LSTR ( nop, iop, ier )
	indx = INDEX ( nop, '?' )
	CALL ST_INCH ( nclip, cnc, ier )
	CALL ST_LSTR ( cnc, ncnc, ier )
	WRITE ( lunp, 10 ) sp6, nop ( : indx - 1 ), cnc ( : ncnc ), 
     +			   nop ( indx + 1 : iop )
C
	CALL ST_LSTR ( poly, npoly, ier )
	WRITE ( lunp, 10 ) sp6, poly ( : npoly )
C
	CALL ST_LSTR ( outstr, lout, ier )
	WRITE ( lunp, 10 ) sp2, sp6, outstr ( 2 : lout )
	CALL ST_LSTR ( polysl, npolysl, ier )
	WRITE ( lunp, 10 ) sp6, polysl ( : npolysl )
C
	CALL ST_LSTR ( areasl, nareas, ier )
	WRITE ( lunp, 10 ) sp4, areasl ( : nareas )
C
	CALL ST_LSTR ( infosl, ninfos, ier )
	WRITE ( lunp, 10 ) sp2, infosl ( : ninfos )
C
	CALL ST_LSTR ( alrtsl, nalert, ier )
	WRITE ( lunp, 10 ) alrtsl ( : nalert )
C
10	FORMAT ( 6A )
C*
	RETURN
	END
