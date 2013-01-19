<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="xmlBufr_common.xsl"/>
  <xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>
  
  <!--
      xmlBufr_zulu.xsl
      
      Purpose: Create ZULU XML document for preparation of the GFA BUFR
      message.  Zulu is a bulletin designator for Icing Hazards and the
      freezing level.  Input is a pre-XML document built internally
      by afcreatexml.c which uses the VGF el structure for input.
      
      Change Log:
      
      L. Hinson/AWC 01/06 Initial Coding
      L. Hinson/AWC 03/06 Updated for 5.9.2
      L. Hinson/AWC 06/08 Updated to set data Significance element on
         freezing levels to either a code figure 11 (Freezing Level) or 
         12 (multiple freezing level).
      L. Hinson/AWC 04/09 Re-Issued
  -->
  
  <xsl:variable name="x">
  </xsl:variable>
  
  <xsl:template match="hdr">
    <productValidTime>
      <xsl:call-template name="timePeriod">
        <xsl:with-param name="issueTime" select="issueTime"/>
        <xsl:with-param name="untilTime" select="untilTime"/>
      </xsl:call-template>
    </productValidTime>
  </xsl:template>
  
  <xsl:template match="smear">
    <xsl:element name="gfaObject">
      <xsl:attribute name="type"> <xsl:value-of select="hazard"/> </xsl:attribute>
      <xsl:element name="productStatus">
        <xsl:call-template name="productStatus">
          <xsl:with-param name="Status" select="Status"/>
        </xsl:call-template>
        <xsl:choose>
          <xsl:when test="hazard='ICE'">
            <xsl:element name="metFeature">
              <xsl:attribute name="type">Airframe Icing</xsl:attribute>
              <xsl:attribute name="bufrCode">15</xsl:attribute> 
              <xsl:call-template name="GFAIdObsOrFcstLoc">
                 <xsl:with-param name="id" select="tag"/>
                 <xsl:with-param name="fcstHr" select="fcstHr"/>
                 <xsl:with-param name="validFrom" select="validFrom"/>
                 <xsl:with-param name="validUntil" select="validUntil"/>
                 <xsl:with-param name="closeFlg" select="closeFlg"/>
                 <xsl:with-param name="nPts" select="nLatLonPts"/>
                 <xsl:with-param name="latPts" select="latPts"/>
                 <xsl:with-param name="lonPts" select="lonPts"/>
                 <xsl:with-param name="dueTo" select="DUE_TO"/>
                 <xsl:with-param name="top" select="Top"/>
		 <xsl:with-param name="base" select="Base"/>
                 <xsl:with-param name="fzltop" select="FzlTop"/>
                 <xsl:with-param name="fzlbase" select="FzlBase"/>
              </xsl:call-template>
              <airframeIcing type="Moderate" bufrCode="4"/>
            </xsl:element>
          </xsl:when>
          <xsl:when test="hazard='FZLVL'">	    
            <xsl:element name="dataSig">
              <xsl:attribute name="type">Freezing level</xsl:attribute>
              <xsl:attribute name="bufrCode">11</xsl:attribute>
              <xsl:call-template name="GFAIdObsOrFcstLoc">
                 <xsl:with-param name="id" select="tag"/>
                 <xsl:with-param name="fcstHr" select="fcstHr"/>
                 <xsl:with-param name="validFrom" select="validFrom"/>
                 <xsl:with-param name="validUntil" select="validUntil"/>
                 <xsl:with-param name="closeFlg" select="closeFlg"/>
                 <xsl:with-param name="nPts" select="nLatLonPts"/>
                 <xsl:with-param name="latPts" select="latPts"/>
                 <xsl:with-param name="lonPts" select="lonPts"/>
                 <xsl:with-param name="dueTo" select="DUE_TO"/>
                 <xsl:with-param name="top" select="Level"/>
		 <xsl:with-param name="base" select="Level"/>
                 <xsl:with-param name="fzltop" select="FzlTop"/>
                 <xsl:with-param name="fzlbase" select="FzlBase"/>
              </xsl:call-template>
            </xsl:element>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:element name="dataSig">
              <xsl:attribute name="type">Multiple freezing level</xsl:attribute>
              <xsl:attribute name="bufrCode">12</xsl:attribute>
              <xsl:call-template name="GFAIdObsOrFcstLoc">
                 <xsl:with-param name="id" select="tag"/>
                 <xsl:with-param name="fcstHr" select="fcstHr"/>
                 <xsl:with-param name="validFrom" select="validFrom"/>
                 <xsl:with-param name="validUntil" select="validUntil"/>
                 <xsl:with-param name="closeFlg" select="closeFlg"/>
                 <xsl:with-param name="nPts" select="nLatLonPts"/>
                 <xsl:with-param name="latPts" select="latPts"/>
                 <xsl:with-param name="lonPts" select="lonPts"/>
                 <xsl:with-param name="dueTo" select="DUE_TO"/>
                 <xsl:with-param name="top" select="Top"/>
		 <xsl:with-param name="base" select="Base"/>
                 <xsl:with-param name="fzltop" select="FzlTop"/>
                 <xsl:with-param name="fzlbase" select="FzlBase"/>
              </xsl:call-template>
            </xsl:element>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="/">
    <gfaInfo>
      <bulletin type="ZULU">
        <xsl:apply-templates/>
      </bulletin>
    </gfaInfo>
  </xsl:template>
</xsl:stylesheet>
