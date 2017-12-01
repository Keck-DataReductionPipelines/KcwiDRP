;; there is a sharp edge in the BM response that varies with central
;; wave.

function kcwi_bm_ledge_position,cwave
  ;; empirically determined fit based on KCWI 2017 data
  fit=[4044.56, 0.240742]
  return,fit[0]+fit[1]*cwave
end;


