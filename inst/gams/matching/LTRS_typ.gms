sum(refMap_LTRS_typ(refVar,typ,vin,hs)$vinExists(t,vin),
  sum((bs,loc,inc),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"LTRS_typ")