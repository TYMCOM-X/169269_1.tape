RUN (QASYS)UDTCRE
COPY UPLCRT.TMP,(QASYS)UPLCRT.DAT
DEC ALL RD RD (QASYS)UPLCRT.DAT
R SETLIC
(QASYS)UPLCRT.DAT,WC
DIR/LIC (QASYS)UPLCRT.DAT
SYSN
   