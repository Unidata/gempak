import os
import sys 
from datetime import datetime
from operator import itemgetter
from awips import ThriftClient
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gempak.request import GetGridInfoRequest


class GridInfoRetriever:

    def __init__(self, server, pluginName, modelId, cycle=None, forecast=None):
        self.pluginName = pluginName
        self.modelId = modelId
        self.cycle = cycle
        self.forecast = forecast
        self.host = os.getenv("DEFAULT_HOST", server)
        self.port = os.getenv("DEFAULT_PORT", "9581")
        self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getInfo(self):
        """ Sends ThriftClient request and writes out received files."""
        req = GetGridInfoRequest()
        req.setPluginName(self.pluginName)
        req.setModelId(self.modelId)

        req.setReftime(self.cycle)
        if len(self.cycle) > 2:
            dt = datetime.strptime(self.cycle, '%y%m%d/%H%M')
            ct = datetime.strftime(dt, '%Y-%m-%d %H:%M:%S')
            req.setReftime(ct)

        req.setFcstsec(self.forecast)
        resp = self.client.sendRequest(req)

        # Take care of bytestring encodings in python3
        for i, rec in enumerate(resp):
            resp[i] = {
                key.decode() if isinstance(key, bytes) else key:
                    val.decode() if isinstance(val, bytes) else val
                for key, val in rec.items()
            }

        sortresp = sorted(sorted(resp, key=itemgetter("reftime"), reverse=True), key=itemgetter("fcstsec"))

        grids = []

        count = 0
        for record in sortresp:
            s = '{:<12}'.format(record['param'])

            if sys.byteorder == 'little':
                parm1 = (ord(s[3]) << 24) + (ord(s[2]) << 16) + (ord(s[1]) << 8) + ord(s[0])
                parm2 = (ord(s[7]) << 24) + (ord(s[6]) << 16) + (ord(s[5]) << 8) + ord(s[4])
                parm3 = (ord(s[11]) << 24) + (ord(s[10]) << 16) + (ord(s[9]) << 8) + ord(s[8])
            else:
                parm1 = (ord(s[0]) << 24) + (ord(s[1]) << 16) + (ord(s[2]) << 8) + ord(s[3])
                parm2 = (ord(s[4]) << 24) + (ord(s[5]) << 16) + (ord(s[6]) << 8) + ord(s[7])
                parm3 = (ord(s[8]) << 24) + (ord(s[9]) << 16) + (ord(s[10]) << 8) + ord(s[11])

            dt = datetime.strptime(record['reftime'], '%Y-%m-%d %H:%M:%S.%f')
            dattim = dt.month * 100000000 + dt.day * 1000000 + (dt.year%100) * 10000 + dt.hour * 100 + dt.minute
            fcsth = (int(record['fcstsec']) / 60) / 60
            fcstm = (int(record['fcstsec']) / 60) % 60
            fcst = 100000 + fcsth * 100 + fcstm

            lv1 = float(record['level1'])
            if lv1 == -999999.0:
                lv1 = -1.0
            lv2 = float(record['level2'])
            if lv2 == -999999.0:
                lv2 = -1.0

            vcd = record['vcoord']
            if vcd == 'NONE':
                ivcd = 0
            elif vcd == 'PRES':
                ivcd = 1
            elif vcd == 'THTA':
                ivcd = 2
            elif vcd == 'HGHT':
                ivcd = 3
            elif vcd == 'SGMA':
                ivcd = 4
                if lv1 >= 0.0:
                    lv1 = lv1 * 10000.0
                if lv2 >= 0.0:
                    lv2 = lv2 * 10000.0
            elif vcd == 'DPTH':
                ivcd = 5
                if lv1 >= 0.0:
                    lv1 = lv1 * 100.0
                if lv2 >= 0.0:
                    lv2 = lv2 * 100.0
            elif vcd == 'HYBL':
                ivcd = 6
            else:
                v = '{:<4}'.format(vcd)
                if sys.byteorder == 'little':
                    ivcd = (ord(v[3]) << 24) + (ord(v[2]) << 16) + (ord(v[1]) << 8) + ord(v[0])
                else:
                    ivcd = (ord(v[0]) << 24) + (ord(v[1]) << 16) + (ord(v[2]) << 8) + ord(v[3])
                if vcd == 'POTV':
                    if lv1 >= 0.0:
                        lv1 = lv1 * 1000.0
                    if lv2 >= 0.0:
                        lv2 = lv2 * 1000.0
            grids.append(9999)
            grids.append(dattim)
            grids.append(fcst)
            grids.append(0)
            grids.append(0)
            grids.append(int(lv1))
            grids.append(int(lv2))
            grids.append(ivcd)
            grids.append(parm1)
            grids.append(parm2)
            grids.append(parm3)
            count += 1
            if count > 29998:
                break

        return grids
             

def getinfo(server, table, model, cycle, forecast):
    gir = GridInfoRetriever(server, table, model, cycle, forecast)
    return gir.getInfo()


def getrow(server, table, model, cycle, forecast):
    idata = []
    idata.append(9999)
    idata.append(1)
    return idata


# This is the standard boilerplate that runs this script as a main
if __name__ == '__main__':
    # Run Test
    srv = 'edex-cloud.unidata.ucar.edu'
    tbl = 'grid'
    mdl = 'NAM40'
    print(getrow(srv, tbl, mdl))
    print(getinfo(srv, tbl, mdl))
