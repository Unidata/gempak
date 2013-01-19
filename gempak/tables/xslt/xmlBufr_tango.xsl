<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="xmlBufr_common.xsl"/>
  <xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>


  <!--
      xmlBufr_tango.xsl
      
      Purpose: Create Tango XML document for preparation of the GFA BUFR
      message.  Tango is a bulletin designator for Turbulence, Strong
      Surface Wind, and LLWS hazards.  Input is a pre-XML document built 
      internally by afcreatexml.c which uses the VGF el structure for input.
      
      Change Log:
      
      L. Hinson/AWC 01/06 Initial Coding
      L. Hinson/AWC 03/06 Updated for 5.9.2
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
          <xsl:when test="hazard='TURB' or hazard='TURB-HI' or hazard='TURB-LO'">
            <xsl:element name="metFeature">
              <xsl:attribute name="type">Turbulence</xsl:attribute>
              <xsl:attribute name="bufrCode">13</xsl:attribute>          
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
                 <xsl:with-param name="fzltop" select="''"/>
                 <xsl:with-param name="fzlbase" select="''"/>
              </xsl:call-template>
              <degreeOfTurb type="Moderate" bufrCode="6"/>
            </xsl:element>
          </xsl:when>
          <xsl:when test="hazard='SFC_WND'">
            <xsl:element name="dataSig">
              <xsl:attribute name="type">Strong Surface Wind</xsl:attribute>
              <xsl:attribute name="bufrCode">10</xsl:attribute>
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
                 <xsl:with-param name="top" select="'SFC'"/>
		 <xsl:with-param name="base" select="base"/>
                 <xsl:with-param name="fzltop" select="''"/>
                 <xsl:with-param name="fzlbase" select="''"/>
              </xsl:call-template>
              <windSpeedLimit type="Above" bufrCode="0"/>
              <windSpeed units="knots">30</windSpeed>
            </xsl:element>
          </xsl:when>
          <xsl:when test="hazard='LLWS'">
            <xsl:element name="metFeature">
              <xsl:attribute name="type">Phenomenon</xsl:attribute>
              <xsl:attribute name="bufrCode">16</xsl:attribute>          
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
                 <xsl:with-param name="top" select="'020'"/>
                 <xsl:with-param name="base" select="'SFC'"/>
                 <xsl:with-param name="fzltop" select="''"/>
                 <xsl:with-param name="fzlbase" select="''"/>
              </xsl:call-template>
              <otherWxPhenomena type="Wind Shear" bufrflag="12"/>
            </xsl:element> 
          </xsl:when>
        </xsl:choose>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="/">
    <gfaInfo>
      <bulletin type="TANGO">
        <xsl:apply-templates/>
      </bulletin>
    </gfaInfo>
  </xsl:template>
</xsl:stylesheet>
