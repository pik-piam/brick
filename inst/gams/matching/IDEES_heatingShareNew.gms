sum(refMap_IDEES_heatingShareNew(refVar,hs,sec),
  sum((bs,loc,typ,inc)$typInSec(typ,sec),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
    +
    sum((renAllowedHS(bs,hs2,hs), vinExists(t,vin)),
      v_renovationHS("area",renAllowedHS,vin,reg,loc,typ,inc,t)
    )
  )
)$sameas(ref,"IDEES_heatingShareNew")