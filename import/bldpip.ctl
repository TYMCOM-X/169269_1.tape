;; Control file to build UA version of PIP.
;; Files required:
;;	PIP.MAC
;;	LNKPIP.CCL
;; plus the cusps MACRO, LINK, PIP and CREF.

.R MACRO
*PIP=PIP
.R LINK
@LNKPIP
.NSS PIP
  