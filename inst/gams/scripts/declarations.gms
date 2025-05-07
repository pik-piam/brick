parameters
p_dt(ttot)        "length of time step in yr"
p_dtVin(ttot,vin) "intersection of time step and vintage cohort in yr"

p_householdSize(reg,loc,typ,inc,ttot) "household size in cap"
p_floorPerCap(reg,loc,typ,inc,ttot)   "average floor space per capita in stock in m2/cap"

p_specCostCon(cost,bs,hs,reg,loc,typ,inc,ttot)             "floor-space specific construction cost in USD/m2"
p_specCostRen(cost,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "floor-space specific renovation cost in USD/m2"
p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot)                  "floor-space specific operation cost in USD/(m2.yr)"
p_specCostDem                                              "floor-space specific demolition cost in USD/m2"

p_carbonPrice(ttot)                "Carbon price in USD/t_CO2eq"
p_carrierPrice(carrier,reg,ttot)   "final energy carrier price in USD/kWh"
p_carrierEmi(carrier,reg,ttot)     "energy carrier emission intensity in t_CO2/kWh"
p_ueDemand(bs,vin,reg,typ)         "floor-space specific useful energy demand for space heating in kWh/yr/m2"
p_feDemand(hs,bs,vin,reg,typ,ttot) "floor-space specific final energy demand for space heating in kWh/yr/m2"
p_eff(hs,reg,typ,ttot)             "technical efficiency of space heating technologies"
p_renDepth(bs,bsr)                 "renovation depth"

p_lccCon(cost,var,bs,hs,reg,loc,typ,inc,ttot) "Estimate of life cycle cost of constructed housing in USD/m2"
p_probDem(reg,typ,ttot2,ttot)                 "probability of a building having reached its end of life"
p_LifeTimeBS(reg)                             "life time of building shell system in yr"
p_LifeTimeHS(hs,reg,typ)                      "life time of heating system in yr"

p_population(reg,loc,typ,inc,ttot)          "number of people in million"
p_floorPerCap(reg,loc,typ,inc,ttot)         "floor space per capita in m2"

p_stockHist(qty,bs,hs,vin,reg,loc,typ,inc,ttot)              "historic stock of buildings in million m2"
p_constructionHist(qty,bs,hs,reg,loc,typ,inc,ttot)           "historic flow of new buildings in million m2/yr"
p_renovationHist(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "historic flow of renovated and untouched buildings in million m2/yr"
p_demolitionHist(qty,bs,hs,vin,reg,loc,typ,inc,ttot)         "historic flow of demolished buildings in million m2/yr"

p_shareDem(vin,reg,typ,ttot)           "minimum share of demolition at end of life"
p_shareRenBS(reg,ttot,ttot)            "minimum share of renovation from the building shell reaching end of life"
p_shareRenHS(hs,reg,typ,ttot,ttot)     "minimum share of renovation from the heating system reaching end of life"
p_shareRenBSinit(reg,ttot,ttot)        "minimum share of renovation from the building shell of initial stock reaching end of life"
p_shareRenHSinit(hs,reg,typ,ttot,ttot) "minimum share of renovation from the heating system of initial stock reaching end of life"

p_interestRate(typ,ttot) "interest rate (incl. implicit) w.r.t. t0 in 1/yr"
p_discountFac(typ,ttot)  "discount factor w.r.t. t0"

p_runtime(reg,loc,typ,inc)                  "model runtime"
p_runtime2(reg)                  "model runtime"
p_handle(reg,loc,typ,inc)                   "parallel model handle parameter"
p_handle2(reg)                   "parallel model handle parameter"
p_repyFullSysLP(solveinfo)                  "model and solver summary: fullSysLP"
p_repyFullSysNLP(reg,loc,typ,inc,solveinfo) "model and solver summary: fullSysNLP"

p_flowVariationWeight               "weight of flow variation in matching objective"
p_slackRenWeight                    "weight of building shell and heating system replacement slack in matching objective"

p_refVals(reference,refVar,reg,ttot) "reference values to match"
p_refValsMed(reference,reg)          "median non-zero reference value to normalise deviations"
p_refWeight(reference)               "Weight of reference"

priceSensBS(var, reg, loc, typ, inc) "price sensitivity of building shell choice"
priceSensHS(var, reg, loc, typ, inc) "price sensitivity of heating system choice"

p_statusQuoPref "preference for replacehing a heating system with the same technology in USD/m2"
;

scalars
t0 "reference year for discounting"

epsilon "offset to avoid log(0)" /1E-5/
;

variables
v_totObj               "total objective value"
v_Obj(reg,loc,typ,inc) "objective value: discounted system cost + heterogeneity preference"

v_SysHeteroPref(reg,loc,typ,inc,ttot) "system-wide heterogeneity preference"
v_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
v_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"
v_statusQuoPref(reg,loc,typ,inc,ttot) "status quo preference when replacing heating systems"

v_slackRenBS(bs,vin,reg,loc,typ,inc,ttot) "difference between actual and min required building shell replacement"
v_slackRenHS(hs,vin,reg,loc,typ,inc,ttot) "difference between actual and min required heating system replacement"

$ifthen.matching "%RUNTYPE%" == "matching"
v_replacementDeviation "total deviation from technology replacement according to life time"

v_flowVariationTot                               "total temporal variation of flows"
v_refVals(reference,refVar,reg,t)            "model variable values at reference aggregation"
v_refValsBasic(reference,refVarGroup,reg,t)  "v_refVals summed to basic value of reference shares"
v_refDeviation(reference,reg,ttot)           "summed squared deviation from reference sources"
v_refDeviationTot                                "total weighted squared deviation of quantities from reference sources"
v_refDeviationVar(reference,refVar,reg,ttot) "deviation from each variable in reference sources"
v_matchingObj                                    "matching objective: reference deviation and flow variation"

v_flowVariation(varFLow,qty,reg,loc,typ,inc,ttot)       "temporal variation of flows [million m2/yr/yr]"
v_flowVariationCon(qty,bs,hs,reg,loc,typ,inc,t)         "temporal variation of construction flow [million m2/yr/yr]"
v_flowVariationRen(qty,bs,hs,bsr,hsr,reg,loc,typ,inc,t) "temporal variation of renovation flow [million m2/yr/yr]"
v_flowVariationDem(qty,bs,hs,reg,loc,typ,inc,t)         "temporal variation of demolition flow [million m2/yr/yr]"
$endif.matching

v_SysCost(reg,loc,typ,inc,ttot) "system cost cost cash flow in USD/yr"
v_ConCost(reg,loc,typ,inc,ttot) "construction cost cash flow in USD/yr"
v_RenCost(reg,loc,typ,inc,ttot) "renovation cost cash flow in USD/yr"
;

positive variables
v_OpeCost(reg,loc,typ,inc,ttot) "operational cost cash flow in USD/yr"
v_DemCost(reg,loc,typ,inc,ttot) "demolition cost cash flow in USD/yr"

v_stock(qty,bs,hs,vin,reg,loc,typ,inc,ttot)              "stock of buildings in million m2"
v_construction(qty,bs,hs,reg,loc,typ,inc,ttot)           "flow of new buildings in million m2/yr"
v_renovation(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "flow of renovated and untouched buildings in million m2/yr"
v_demolition(qty,bs,hs,vin,reg,loc,typ,inc,ttot)         "flow of demolished buildings in million m2/yr"

v_dwelSizeStock(vin,reg,loc,typ,inc,ttot)              "average dwelling size of the stock in m2/dwel"
v_dwelSizeConstruction(reg,loc,typ,inc,ttot)           "average dwelling size of newly constructed buildings in m2/dwel"
v_dwelSizeRenovation(vin,reg,loc,typ,inc,ttot)         "average dwelling size of renovated buildings in m2/dwel"
v_dwelSizeDemolition(vin,reg,loc,typ,inc,ttot)         "average dwelling size of demolished buildings in m2/dwel"

$ifthen.matching "%RUNTYPE%" == "matching"
v_dwelSize_Odyssee(refVar,reg,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
v_vinShare_EUBDB(refVar,reg,ttot)   "vintage shares at the aggregation of EUBDB_vintage"
v_renRate_EuropeanCommissionRenovation(refVar,reg,ttot)
v_heatingShare_Odyssee(refVar,reg,ttot) "share of heating systems in the stock"
v_heatingShare_IDEES(refVar,reg,ttot) "share of heating systems in the stock"
v_heatingShare_IDEES2(refVar,reg,ttot) "share of heating systems in the stock"
$endif.matching
;

equations

q_totObj               "total objective"
q_Obj(reg,loc,typ,inc) "objective: discounted system cost + heterogeneity preference"

q_SysCost(reg,loc,typ,inc,ttot) "system cost (con + ren + ope + dem)"
q_ConCost(reg,loc,typ,inc,ttot) "construction cost"
q_RenCost(reg,loc,typ,inc,ttot) "renovation cost"
q_OpeCost(reg,loc,typ,inc,ttot) "operation cost"
q_DemCost(reg,loc,typ,inc,ttot) "demolition cost"

q_SysHeteroPref(reg,loc,typ,inc,ttot) "system-wide heterogeneity preference"
q_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
q_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"
q_statusQuoPref(reg,loc,typ,inc,ttot) "status quo preference"
q_zeroHeteroPrefCon(reg,loc,typ,inc,ttot) "zero diversity preference for construction (lp)"
q_zeroHeteroPrefRen(reg,loc,typ,inc,ttot) "zero diversity preference for renovation (lp)"

q_stockBalNext(qty,bs,hs,vin,reg,loc,typ,inc,ttot)  "building stock balance: flows into next time step"
q_stockBalPrev(qty,bs,hs,vin,reg,loc,typ,inc,ttot)  "building stock balance: flows from previous time step"
q_housingDemand(reg,loc,typ,inc,ttot)                "demand for floor space"
q_buildingLifeTime(qty,bs,hs,vin,reg,loc,typ,inc,ttot)   "minimum demolition from builing life time"
q_buildingShellLifeTime(qty,bs,vin,reg,loc,typ,inc,ttot) "minimum renovation from building shell life time"
q_heatingSystemLifeTime(qty,hs,vin,reg,loc,typ,inc,ttot) "minimum renovation from heating system life time"

q_dwelSizeStock(vin,reg,loc,typ,inc,ttot)      "dwelling size of the stock in m2/dwel"
q_dwelSizeConstruction(reg,loc,typ,inc,ttot)   "dwelling size of newly constructed buildings in m2/dwel"
q_dwelSizeRenovation(vin,reg,loc,typ,inc,ttot) "dwelling size of renovated buildings in m2/dwel"
q_dwelSizeDemolition(vin,reg,loc,typ,inc,ttot) "dwelling size of demolished buildings in m2/dwel"

q_minDivConBS(bs,hsr,reg,loc,typ,inc,t)             "minimum building shell diversity in construction"
q_minDivConHS(bs,hsr,reg,loc,typ,inc,t)             "minimum heating system diversity in construction"
q_minDivRenBS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum building shell diversity in renovation"
q_minDivRenHS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum heating system diversity in renovation"

q_maxRenRate(reg,ttot) "Maximum renovation rate"

q_replacementDeviation "total deviation from technology replacement according to life time"

q_flowVariationTot                                         "total temporal variation of flows"
q_flowVariation(varFLow,qty,reg,loc,typ,inc,ttot)          "temporal variation of flows"
q_flowVariationCon(qty,bs,hs,reg,loc,typ,inc,ttot)         "temporal variation of construction flow"
q_flowVariationRen(qty,bs,hs,bsr,hsr,reg,loc,typ,inc,ttot) "temporal variation of renovation flow"
q_flowVariationDem(qty,bs,hs,reg,loc,typ,inc,ttot)         "temporal variation of demolition flow"


$ifthen.matching "%RUNTYPE%" == "matching"
q_dwelSize_Odyssee(refVar,reg,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
q_vinShare_EUBDB(refVar,reg,ttot)   "vintage shares at the aggregation of EUBDB_vintage"
q_renRate_EuropeanCommissionRenovation(refVar,reg,ttot)
q_heatingShare_Odyssee(refVar,reg,ttot) "share of heating systems in the stock"
q_heatingShare_IDEES(refVar,reg,ttot) "share of heating systems in the stock"
q_heatingShare_IDEES2(refVar,reg,ttot) "share of heating systems in the stock"

q_refDeviationTot                               "total squared deviation of quantities from reference source"
q_refDeviation(reference,reg,ttot)          "summed squared deviation from reference sources"
q_refDeviationVar(reference,refVar,reg,t)   "deviation from each variable in reference sources"
q_refVals(reference,refVar,reg,t)           "aggregate model variable to reference granularity"
q_refValsBasic(reference,refVarGroup,reg,t) "sum v_refVals to basic value of reference shares"

q_matchingObj "matching objective: reference deviation and flow variation"
q_finiteHeatingShareCon(bs,hs,reg,loc,typ,inc,ttot)
q_finiteHeatingShareRen(bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot)
$endif.matching
;

$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
parameters
p_stockCalibTarget(qty,bs,hs,vin,reg,loc,typ,inc,ttot)              "historic stock of buildings in million m2 as calibration target"
p_constructionCalibTarget(qty,bs,hs,reg,loc,typ,inc,ttot)           "historic flow of new buildings as calibration target in million m2/yr"
p_renovationCalibTarget(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "historic flow of renovated and untouched buildings as calibration target in million m2/yr"
;
$endif.calibration

$ifthenE.calibrationOptimization (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))
parameters
p_diff /0.1/
p_xinitCon(bs, hs, reg, loc, typ, inc, ttot)
p_xinitRen(bs, hs, bsr, hsr, vin, reg, loc, typ, inc, ttot)
p_specCostCalibCon(bs, hs, reg, loc, typ, inc, ttot)
p_specCostCalibRen(bs, hs, bsr, hsr, vin, reg, loc, typ, inc, ttot)
p_xDiffCon(bs, hs, reg, loc, typ, inc, ttot)
p_xDiffRen(renType, bsr, hsr, vin, reg, loc, typ, inc, ttot)

p_f(reg, loc, typ, inc, ttot)
p_fDiffCon(bs, hs, reg, loc, typ, inc, ttot)
p_fDiffRen(renType, bsr, hsr, vin, reg, loc, typ, inc, ttot)

p_renovation(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot)
p_construction(qty,bs,hs,reg,loc,typ,inc,ttot)
p_stock(qty, bs, hs, vin, reg, loc, typ, inc, ttot)
;
$endif.calibrationOptimization
