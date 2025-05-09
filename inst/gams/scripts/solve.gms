*** models ---------------------------------------------------------------------

model fullSysLP "full system linear optimisation"
  /
  q_totObj
  q_Obj
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
$ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
  q_buildingShellLifeTime
$endif.shell
  q_heatingSystemLifeTime
  q_SysHeteroPref
  q_zeroHeteroPrefCon
  q_zeroHeteroPrefRen
  q_statusQuoPref
*  q_minDivConHS
*  q_minDivConBS
*  q_minDivRenBS
*  q_minDivRenHS
*  q_maxRenRate
  /
;

model fullSysNLP "full system linear optimisation"
  /
  q_totObj
  q_Obj
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
$ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
  q_buildingShellLifeTime
$endif.shell
  q_heatingSystemLifeTime
  q_SysHeteroPref
  q_statusQuoPref
  q_HeteroPrefCon
  q_HeteroPrefRen
*  q_maxRenRate
  /
;

$ifthen.matching "%RUNTYPE%" == "matching"
model matching "find stock and flows that best match reference sources"
  /
  q_matchingObj
  q_refDeviationTot
  q_refDeviationVar
  q_refDeviation
  q_refVals
  q_refValsBasic
  q_stockBalNext
  q_stockBalPrev
  q_buildingLifeTime !! TODO: make this a matching target, not a hard constraint
$ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
  q_buildingShellLifeTime
$endif.shell
  q_heatingSystemLifeTime
*  q_dwelSizeStock
*  q_dwelSizeConstruction
*  q_dwelSize_Odyssee
*  q_renRate_EuropeanCommissionRenovation
  q_replacementDeviation
  q_flowVariation
  q_flowVariationTot
  q_flowVariationCon
  q_flowVariationRen
  q_flowVariationDem
*  q_test
  q_testCon
  q_testRen
  /
;
$endif.matching



*** prepare solving ------------------------------------------------------------

* filter subs
$ifthen.filtersubs %FILTERSUBS% == "TRUE"
subs(all_subs) = no;
subs("DEU","rural","SFH","all") = yes;
$endif.filtersubs


* solvers
option lp  = %solverLP%;
option nlp = %solverNLP%;
option qcp = %solverQCP%;

* define macro for solving in parallel mode

$macro solveParallel subs(all_subs) = no; \
fullSysNLP.SolveLink = 3; \
loop(all_subs, \
  subs(all_subs) = yes; \
  solve fullSysNLP minimizing v_totObj using nlp; \
  subs(all_subs) = no; \
  p_handle(all_subs) = fullSysNLP.handle; \
); \
repeat \
  loop(all_subs$handleCollect(p_handle(all_subs)), \
		p_runtime(all_subs) = fullSysNLP.resusd; \
    p_repyFullSysNLP(all_subs,'solvestat') = fullSysNLP.solvestat; \
    p_repyFullSysNLP(all_subs,'modelstat') = fullSysNLP.modelstat; \
    p_repyFullSysNLP(all_subs,'resusd')    = fullSysNLP.resusd; \
    p_repyFullSysNLP(all_subs,'objval')    = fullSysNLP.objval; \
    if(handleStatus(p_handle(all_subs)), \
      fullSysNLP.handle = p_handle(all_subs); \
      display$handleDelete(p_handle(all_subs)) 'trouble deleting handles' ; \
      p_handle(all_subs) = 0; \
    ); \
  ); \
  display$sleep(5) 'sleep some time'; \
until card(p_handle) = 0; \
subs(all_subs) = yes;




*** scenario / calibration run -------------------------------------------------

$ifthen.fullSys "%RUNTYPE%" == "scenario"

* measure stocks and flows in floor area
q("num") = no;
q("area") = yes;


$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totObj using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

$ifthen.parallel "%PARALLEL%" == "TRUE"

solveParallel

$else.parallel

solve fullSysNLP minimizing v_totObj using nlp;

p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

$endif.parallel



$endif.nlp


*** scenario / calibration run -------------------------------------------------

$elseIfE.fullSys (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))

$ifThen.targetFunc "%TARGETFUNCTION%" == "minsquare"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func sum(state3$(p_constructionCalibTarget("area", state3, subs, tcalib)), \
  power(p_constructionCalibTarget("area", state3, subs, tcalib) - v_construction.l("area", state3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3)$(renAllowed(state3, stateFull3) and p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) ne NA), \
  power(p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((vin3, state3), \
  power(p_stockCalibTarget("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func sum((vin3, state3), \
  power(p_stockCalibTarget("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3)$zeroFlow(state3, stateFull3), \
  power(p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));
$endIf.calibTarget

$elseIf.targetFunc "%TARGETFUNCTION%" == "maxlikely"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func - sum(state3, \
  p_constructionCalibTarget("area", state3, subs, tcalib) \
  * log(v_construction.l("area", state3, subs, tcalib) \
    / (sum(state4, \
      v_construction.l("area", state4, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  - sum((vin3, state3, stateFull3)$renAllowed(state3, stateFull3), \
  p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func - sum((vin3, state3), \
  p_stockCalibTarget("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall)) \
  - sum((vin3, state3, stateFull3)$zeroFlow(state3, stateFull3), \
  p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
);

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func - sum((vin3, state3), \
  p_stockCalibTarget("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall));
$endIf.calibTarget

$endIf.targetFunc

$macro extrapolateIntangCon - sum(tcalib, \
  p_specCostCalibCon(state, subs, tcalib)) / card(tcalib);

$macro extrapolateIntangRen - sum(tcalib, \
  p_specCostCalibRen(state, stateFull, vin, subs, tcalib)) / card(tcalib);

*********************************************************************************
*** Preparation of the calibration
*********************************************************************************

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

solveParallel

*** Compute the functional value
p_f(subs, tcalib) = func

*** Store renovation and construction values
p_construction("area", state, subs, t) = v_construction.l("area", state, subs, t);
p_renovation("area", state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics
p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

p_xinitCon(state, subs, tcalib) = p_specCostCon("intangible", state, subs, tcalib);
p_xinitRen(state, stateFull, vinCalib, subs, tcalib)$renAllowed(state, stateFull) = p_specCostRen("intangible", state, stateFull, vinCalib, subs, tcalib);

*** Compute the gradient
loop((bs3, hs3, tcalib2),
  p_xDiffCon(bs, hs, subs, tcalib)$(gradientVarsCon(bs, hs, tcalib)
                                    and (not sameas(bs, bs3) or not sameas(hs, hs3) or not sameas(tcalib, tcalib2)))
                                    = 0;
  p_xDiffCon(bs, hs, subs, tcalib)$(gradientVarsCon(bs, hs, tcalib)
                                    and (sameas(bs, bs3) and sameas(hs, hs3) and sameas(tcalib, tcalib2)))
                                    = p_diff;
  p_specCostCalibCon(state, subs, tcalib) = p_xDiffCon(state, subs, tcalib);

  p_specCostCalibCon(state, subs, t)$(not tcalib(t)) = extrapolateIntangCon

  p_specCostCon("intangible", state, subs, t) = p_xinitCon(state, subs, t) + p_specCostCalibCon(state, subs, t);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fDiffCon(bs3, hs3, subs, tcalib) = func
);

loop(gradientVarsRen(renType2, bsr3, hsr3, vin2, tcalib2),
  p_xDiffRen(renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVarsRen(renType, bsr, hsr, vinCalib, tcalib)
                                                            and (not sameas(renType, renType2)
                                                                 or not sameas(bsr, bsr3) or not sameas(hsr, hsr3)
                                                                 or not sameas(vinCalib, vin2) or not sameas(tcalib, tcalib2)))
                                                          = 0;
  p_xDiffRen(renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVarsRen(renType, bsr, hsr, vinCalib, tcalib)
                                                            and (sameas(renType, renType2)
                                                                 and sameas(bsr, bsr3) and sameas(hsr, hsr3)
                                                                 and sameas(vinCalib, vin2) and sameas(tcalib, tcalib2)))
                                                          = p_diff;
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalibRen(bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hs, hsr)
                                                                           = p_xDiffRen("identRepl", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalibRen(bs, hs, bsr, hsr, vinCalib, subs, tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xDiffRen("newSys", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalibRen(bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hsr, "0") = p_xDiffRen("0", bsr, hsr, vinCalib, subs, tcalib);
  );

  p_specCostCalibRen(state, stateFull, vin, subs, t)$(not tcalib(t))
                                                             = extrapolateIntangRen

  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull)
                                                  = p_xinitRen(state, stateFull, vinCalib, subs, t)
                                                  + p_specCostCalibRen(state, stateFull, vinCalib, subs, t);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fDiffRen(renType2, bsr3, hsr3, vin2, subs, tcalib) = func
);

$endif.fullSys

*** matching run ---------------------------------------------------------------

$ifthen.matching "%RUNTYPE%" == "matching"

* measure stocks and flows in both floor area and number of dwellings
q(qty) = yes;

* measure stocks and flows in floor area
q("num") = no;
*q("area") = yes;

reg(region) = no;
subs(all_subs) = no;

loop(region,
  reg(region) = yes;
  subs(region,loc,typ,inc) = yes;

  solve matching minimizing v_matchingObj using qcp;
  execute_unload "test.gdx";

  reg(region) = no;
  subs(region,loc,typ,inc) = no;
);

reg(region) = yes;
subs(all_subs) = yes;

$endif.matching
