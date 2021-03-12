:logfile loginn.log
;---------------------------------------------------------------------
;
;                             Build LOGINN.SHR
;
;                (SYS)LOGINN.SHR has protection ALL RUN RUN.
;                (SYS)LOGINN.SHR has license OP SY ST HF JL
;
;
;---------------------------------------------------------------------


daytime
delete *.rel

load/run (ftsys)macro loginn
ssave loginn

declare all run run loginn.shr

r cksum
^loginn.cks
y
@loginn.fil

dir /ext/alph/prot/lic/time/author/words @loginn.fil
typ loginn.cks

daytime


;---------------------------------------------------------------------
 