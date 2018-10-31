import os
import math
from awips import ThriftClient
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gempak.request import GetGridNavRequest
from ctypes import *

EARTH_RADIUS = 6371200.0
DEG_TO_RAD = math.pi / 180.0
RAD_TO_DEG = 180.0 / math.pi
TWOPI = math.pi * 2.0
HALFPI = math.pi / 2.0
PI4TH = math.pi / 4.0
PI3RD = math.pi / 3.0


def createPolar(nsflag, clon, lat1, lon1, dx, dy, unit, nx, ny):
    clonr = clon * DEG_TO_RAD
    latr = lat1 * DEG_TO_RAD
    lonr = lon1 * DEG_TO_RAD
    if nsflag == 'N':
        x1 = EARTH_RADIUS * math.tan(PI4TH - latr/2.0) * math.sin(lonr-clonr)
        y1 = -1 * EARTH_RADIUS * math.tan(PI4TH - latr/2.0) * math.cos(lonr-clonr)
    else:
        x1 = EARTH_RADIUS * math.tan(PI4TH + latr/2.0) * math.sin(lonr-clonr)
        y1 = EARTH_RADIUS * math.tan(PI4TH + latr/2.0) * math.cos(lonr-clonr)

    if unit == 'm':
        tdx = dx / (1 + math.sin(PI3RD))
        tdy = dy / (1 + math.sin(PI3RD))
    else:
        tdx = (dx*1000.0) / (1 + math.sin(PI3RD))
        tdy = (dy*1000.0) / (1 + math.sin(PI3RD))

    x2 = x1 + tdx * (nx-1)
    y2 = y1 + tdy * (ny-1)
    xll = min(x1, x2)
    yll = min(y1, y2)
    xur = max(x1, x2)
    yur = max(y1, y2)

    if nsflag == 'N':
        latll = (HALFPI - 2*math.atan2(math.hypot(xll, yll), EARTH_RADIUS)) * RAD_TO_DEG
        rtemp = clonr + math.atan2(xll, -yll)
    else:
        latll = -1 * (HALFPI - 2*math.atan2(math.hypot(xll, yll), EARTH_RADIUS)) * RAD_TO_DEG
        rtemp = clonr + math.atan2(xll, yll)

    if rtemp > math.pi:
        lonll = (rtemp-TWOPI) * RAD_TO_DEG
    elif rtemp < -math.pi:
        lonll = (rtemp+TWOPI) * RAD_TO_DEG
    else:
        lonll = rtemp * RAD_TO_DEG

    if nsflag == 'N':
        latur = (HALFPI - 2*math.atan2(math.hypot(xur, yur), EARTH_RADIUS)) * RAD_TO_DEG
        rtemp = clonr + math.atan2(xur, -yur)
    else:
        latur = -1 * (HALFPI - 2*math.atan2(math.hypot(xur, yur), EARTH_RADIUS)) * RAD_TO_DEG
        rtemp = clonr + math.atan2(xur, yur)

    if rtemp > math.pi:
        lonur = (rtemp-TWOPI) * RAD_TO_DEG
    elif rtemp < -math.pi:
        lonur = (rtemp+TWOPI) * RAD_TO_DEG
    else:
        lonur = rtemp * RAD_TO_DEG

    return [latll, lonll, latur, lonur]


def createConic(nsflag, clon, lat1, lon1, dx, dy, unit, nx, ny, ang1, ang3):
    clonr = clon * DEG_TO_RAD
    latr = lat1 * DEG_TO_RAD
    lonr = lon1 * DEG_TO_RAD

    angle1 = HALFPI - (math.fabs(ang1) * DEG_TO_RAD)
    angle2 = HALFPI - (math.fabs(ang3) * DEG_TO_RAD)

    if ang1 == ang3:
        cc = math.cos(angle1)
    else:
        cc = (math.log(math.sin(angle2)) - math.log(math.sin(angle1))) \
             / (math.log(math.tan(angle2/2.0)) - math.log(math.tan(angle1/2.0)))

    er = EARTH_RADIUS / cc

    if nsflag == 'N':
        x1 = er * math.pow(math.tan((HALFPI-latr)/2.0), cc) * math.sin(cc*(lonr-clonr))
        y1 = -1.0 * er * math.pow(math.tan((HALFPI-latr)/2.0), cc) * math.cos(cc*(lonr-clonr))
    else:
        x1 = er * math.pow(math.tan((HALFPI+latr)/2.0), cc) * math.sin(cc*(lonr-clonr))
        y1 = er * math.pow(math.tan((HALFPI+latr)/2.0), cc) * math.cos(cc*(lonr-clonr))

    alpha = math.pow(math.tan(angle1/2.0), cc) / math.sin(angle1)

    if unit == 'm':
        x2 = x1 + (nx-1) * alpha * dx
        y2 = y1 + (ny-1) * alpha * dy
    else:
        x2 = x1 + (nx-1) * alpha * (dx*1000.0)
        y2 = y1 + (ny-1) * alpha * (dy*1000.0)

    xll = min(x1, x2)
    yll = min(y1, y2)
    xur = max(x1, x2)
    yur = max(y1, y2)

    if nsflag == 'N':
        latll = (HALFPI - 2.0 * math.atan(math.pow(math.hypot(xll, yll)/er, (1/cc)))) * RAD_TO_DEG
        rtemp = math.atan2(xll, -yll) * (1/cc) + clonr
    else:
        latll = (-1.0 * (HALFPI - 2.0 * math.atan(math.pow(math.hypot(xll, yll)/er, (1/cc))))) * RAD_TO_DEG
        rtemp = math.atan2(xll, yll) * (1/cc) + clonr

    if rtemp > math.pi:
        lonll = (rtemp-TWOPI) * RAD_TO_DEG
    elif rtemp < -math.pi:
        lonll = (rtemp+TWOPI) * RAD_TO_DEG
    else:
        lonll = rtemp * RAD_TO_DEG

    if nsflag == 'N':
        latur = (HALFPI - 2.0 * math.atan(math.pow(math.hypot(xur, yur)/er, (1/cc)))) * RAD_TO_DEG
        rtemp = math.atan2(xur, -yur) * (1/cc) + clonr
    else:
        latur = (-1.0 * (HALFPI - 2.0 * math.atan(math.pow(math.hypot(xur, yur)/er, (1/cc))))) * RAD_TO_DEG
        rtemp = math.atan2(xur, yur) * (1/cc) + clonr

    if rtemp > math.pi:
        lonur = (rtemp-TWOPI) * RAD_TO_DEG
    elif rtemp < -math.pi:
        lonur = (rtemp+TWOPI) * RAD_TO_DEG
    else:
        lonur = rtemp * RAD_TO_DEG

    return [latll, lonll, latur, lonur]


class StringConverter(Union):
    _fields_ = [("char", c_char*4), ("int", c_int), ("float", c_float)]


class GridNavRetriever:

    def __init__(self, server, pluginName, modelId, arrayLen):
        self.pluginName = pluginName
        self.modelId = modelId
        self.arrayLen = arrayLen
        self.host = os.getenv("DEFAULT_HOST", server)
        self.port = os.getenv("DEFAULT_PORT", "9581")
        self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getNavBlk(self):
        """ Sends ThriftClient request and writes out received files."""
        req = GetGridNavRequest()
        req.setPluginName(self.pluginName)
        req.setModelId(self.modelId)
        resp = self.client.sendRequest(req)

        for i, rec in enumerate(resp):
            resp[i] = {
                key.decode() if isinstance(key, bytes) else key:
                    val.decode() if isinstance(val, bytes) else val
                for key, val in rec.items()
            }

        nav = []

        for record in resp:
            unit = record['spacingunit']
            sk = record['spatialkey']
            skarr = sk.split('/')

            nx = float(skarr[1])
            ny = float(skarr[2])
            dx = float(skarr[3])
            dy = float(skarr[4])

            sc = StringConverter()
            if record['projtype'] == 'LatLon':
                sc.char = 'CED '
                gemproj = 2.0
                ang1 = 0.0
                ang2 = 0.0
                ang3 = 0.0

                lllat = float(record['lowerleftlat'])
                lllon = float(record['lowerleftlon'])
                urlat = lllat + (dy * (ny-1))
                urlon = lllon + (dx * (nx-1))
                if lllon > 180:
                    lllon -= 360.0
                if urlon > 180:
                    urlon -= 360.0

            if record['projtype'] == 'Polar Stereographic':
                sc.char = 'STR '
                gemproj = 2.0
                if float(record['standard_parallel_1']) < 0.0:
                    ang1 = -90.0
                    nsflag = 'S'
                else:
                    ang1 = 90.0
                    nsflag = 'N'
                ang2 = float(record['central_meridian'])
                ang3 = 0.0

                lat1 = float(record['lowerleftlat'])
                lon1 = float(record['lowerleftlon'])
                coords = createPolar(nsflag, ang2, lat1, lon1, dx, dy, unit, nx, ny)
                lllat = coords[0]
                lllon = coords[1]
                urlat = coords[2]
                urlon = coords[3]

            if record['projtype'] == 'Lambert Conformal':
                sc.char = 'LCC '
                gemproj = 2.0

                ang1 = float(skarr[7])
                ang2 = float(record['central_meridian'])
                ang3 = float(skarr[8])
                if ang1 < 0.0:
                    nsflag = 'S'
                else:
                    nsflag = 'N'

                lat1 = float(record['lowerleftlat'])
                lon1 = float(record['lowerleftlon'])
                coords = createConic(nsflag, ang2, lat1, lon1, dx, dy, unit, nx, ny, ang1, ang3)
                lllat = coords[0]
                lllon = coords[1]
                urlat = coords[2]
                urlon = coords[3]

            # Fill up the output array of floats
            nav.append(gemproj)
            nav.append(sc.float)
            nav.append(1.0)
            nav.append(1.0)
            nav.append(nx)
            nav.append(ny)
            nav.append(lllat)
            nav.append(lllon)
            nav.append(urlat)
            nav.append(urlon)
            nav.append(ang1)
            nav.append(ang2)
            nav.append(ang3)

        for i in range(13, int(self.arrayLen)):
            nav.append(0.0)
        return nav

    def getAnlBlk(self):
        anl = []
        # Type
        anl.append(2.0)
        # Delta
        anl.append(1.0)
        # Extend area
        anl.append(0.0)
        anl.append(0.0)
        anl.append(0.0)
        anl.append(0.0)
        # Grid area
        anl.append(-90.0)
        anl.append(-180.0)
        anl.append(90.0)
        anl.append(180.0)
        # Data area
        anl.append(-90.0)
        anl.append(-180.0)
        anl.append(90.0)
        anl.append(180.0)
        for i in range(18, int(self.arrayLen)):
            anl.append(0.0)
        return anl


def getnavb(server, table, model, arrlen):
    gnr = GridNavRetriever(server, table, model, arrlen)
    return gnr.getNavBlk()


def getanlb(server, table, model, arrlen):
    gnr = GridNavRetriever(server, table, model, arrlen)
    return gnr.getAnlBlk()


# This is the standard boilerplate that runs this script as a main
if __name__ == '__main__':
    # Run Test
    srv = 'edex-cloud.unidata.ucar.edu'
    tbl = 'grid_info'
    mdl = 'NAM40'
    navlen = '256'
    print(getnavb(srv, tbl, mdl, navlen))
    anllen = '128'
    print(getanlb(srv, tbl, mdl, anllen))
