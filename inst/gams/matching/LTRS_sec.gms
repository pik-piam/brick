sum(refMap_LTRS_sec(refVar,sec,vin,hs)$vinExists(t,vin),
  sum((bs,loc,typ,inc)$typInSec(typ,sec),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"LTRS_sec")