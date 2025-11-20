sum(refMap_HeatingSystemSales(refVar,hs),
  sum((bs,loc,typ,inc),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
    +
    sum((renAllowedHS(bs,hs2,hs), vinExists(t,vin)),
      v_renovationHS("area",renAllowedHS,vin,reg,loc,typ,inc,t)
    )
  )
)$sameas(ref,"HeatingSystemSales")