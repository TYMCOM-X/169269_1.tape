; CMUPPN.CTL 14-Feb-79
; Compile and add module CMUPPN.
;
.path sai:
.com/com cmuppn
.r maklib
*libary=libary/master:cmuppn,cmuppn/replace:cmuppn
*libary=libary/index

; Now for high-segment version.
.com/com cmupph=cmuppn(h)
.r maklib
*libarh=libarh/master:cmupph,cmupph/replace:cmupph
*libarh=libarh/index
   