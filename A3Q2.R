#Question2
#decrement table
nu_rate = 0.08; u_rate = 0.1
qw = c(0.25, 0.2, 0.175, rep(0.15,6),1)
qx = c(43, 42, 41, 40, 41, 42, 43, 45, 49, 53)/1e5
aqd = qx*(1-0.5*qw); aqw = qw*(1-0.5*qx)
l = numeric(); l[1] = 100000
ald = numeric(); alw = numeric()
for(i in 1:10){ald[i] = l[i]*aqd[i]; alw[i] = l[i]*aqw[i]; if(i <10){l[i+1] = l[i] - ald[i] - alw[i]}}

cbind(qx, qw, aqd, aqw,round(cbind(l, ald, alw),0))

#unit fund
premium_b = rep(5000,10)
asset_fee = 0.003
bs = 0.005
p_fee_b = c(0.45, rep(0.01, 9))

invest_premium_b = premium_b*(1-p_fee_b)*(1-bs)
unit_fund_value_e = numeric()
unit_fund_value_e[1] = invest_premium_b[1]*(1+u_rate)*(1-asset_fee)
for(i in 2:10){unit_fund_value_e[i] = (unit_fund_value_e[i-1] + invest_premium_b[i])*(1+u_rate)*(1-asset_fee)}

invest_premium = invest_premium_b * l[1:10]/l[1]
unit_fund_value = unit_fund_value_e * c(l[2:10],0)/l[1]
int_unit = (invest_premium+c(0, unit_fund_value[1:9]))*u_rate

transfer_to_nunit = (invest_premium + int_unit - unit_fund_value)[1]
for(i in 2:10){transfer_to_nunit[i] = (invest_premium + int_unit - diff(c(0, unit_fund_value)))[i]}
unit = cbind(invest_premium, int_unit, unit_fund_value, transfer_to_nunit)
unit

#non-unit fund
acq_fee = c(0.45, rep(0,9))*premium_b
p_related_fee_b = premium_b * p_fee_b
bs_margin_b = premium_b*(1-p_fee_b)*bs
exp_b = 58*(1+0.2)^(0:9); exp_b[1] = exp_b[1] + 0.45*premium_b[1]
benefit_e = unit_fund_value_e

p_related_fee = p_related_fee_b * l/l[1]
exp = exp_b * l/l[1]
bs_margin = bs_margin_b * l/l[1]
int_nunit = (p_related_fee + bs_margin - exp)*nu_rate
benefit_death = benefit_e * ald/l[1]
benefit_with = benefit_e * alw/l[1]

transfer = p_related_fee + bs_margin + int_nunit - exp - benefit_death - benefit_with + transfer_to_nunit
cbind(p_related_fee, bs_margin, exp, int_nunit, benefit_death, benefit_with, transfer_to_nunit, transfer)

increase = function(amount){
  nu_rate = 0.08; u_rate = 0.1
  qw = c(0.25, 0.2, 0.175, rep(0.15,6),1)
  qx = c(43, 42, 41, 40, 41, 42, 43, 45, 49, 53)/1e5
  aqd = qx*(1-0.5*qw); aqw = qw*(1-0.5*qx)
  l = numeric(); l[1] = 100000
  ald = numeric(); alw = numeric()
  for(i in 1:10){ald[i] = l[i]*aqd[i]; alw[i] = l[i]*aqw[i]; if(i <10){l[i+1] = l[i] - ald[i] - alw[i]}}
  
  #unit fund
  premium_b = rep(5000,10)
  asset_fee = 0.003
  bs = 0.005
  p_fee_b = c(0.45, rep(0.01, 9))
  
  invest_premium_b = premium_b*(1-p_fee_b)*(1-bs)
  unit_fund_value_e = numeric()
  unit_fund_value_e[1] = invest_premium_b[1]*(1+u_rate)*(1-asset_fee)
  for(i in 2:10){unit_fund_value_e[i] = (unit_fund_value_e[i-1] +  invest_premium_b[i])*(1+u_rate)*(1-asset_fee)}
  policy_value_e = unit_fund_value_e
  policy_value_e[9] = policy_value_e[9] + amount
  invest_premium = invest_premium_b * l[1:10]/l[1]
  unit_fund_value = unit_fund_value_e * c(l[2:10],0)/l[1]
  policy_value = policy_value_e * c(l[2:10],0)/l[1]
  int_unit = (invest_premium+c(0, policy_value[1:9]))*u_rate
  
  transfer_to_nunit = (invest_premium + int_unit - unit_fund_value)[1]
  for(i in 2:10){transfer_to_nunit[i] = (invest_premium + int_unit - diff(c(0, policy_value)))[i]}
  
  
  #non-unit fund
  acq_fee = c(0.45, rep(0,9))*premium_b
  p_related_fee_b = premium_b * p_fee_b
  bs_margin_b = premium_b*(1-p_fee_b)*bs
  exp_b = 58*(1+0.2)^(0:9); exp_b[1] = exp_b[1] + 0.45*premium_b[1]
  benefit_e = unit_fund_value_e
  
  p_related_fee = p_related_fee_b * l/l[1]
  exp = exp_b * l/l[1]
  bs_margin = bs_margin_b * l/l[1]
  int_nunit = (p_related_fee + bs_margin - exp)*nu_rate
  benefit_death = benefit_e * ald/l[1]
  benefit_with = benefit_e * alw/l[1]
  
  transfer = p_related_fee + bs_margin + int_nunit - exp - benefit_death - benefit_with + transfer_to_nunit
  return(transfer[10])
}
npv_new = function(amount){
  nu_rate = 0.08; u_rate = 0.1
  qw = c(0.25, 0.2, 0.175, rep(0.15,6),1)
  qx = c(43, 42, 41, 40, 41, 42, 43, 45, 49, 53)/1e5
  aqd = qx*(1-0.5*qw); aqw = qw*(1-0.5*qx)
  l = numeric(); l[1] = 100000
  ald = numeric(); alw = numeric()
  for(i in 1:10){ald[i] = l[i]*aqd[i]; alw[i] = l[i]*aqw[i]; if(i <10){l[i+1] = l[i] - ald[i] - alw[i]}}
  
  #unit fund
  premium_b = rep(5000,10)
  asset_fee = 0.003
  bs = 0.005
  p_fee_b = c(0.45, rep(0.01, 9))
  
  invest_premium_b = premium_b*(1-p_fee_b)*(1-bs)
  unit_fund_value_e = numeric()
  unit_fund_value_e[1] = invest_premium_b[1]*(1+u_rate)*(1-asset_fee)
  for(i in 2:10){unit_fund_value_e[i] = (unit_fund_value_e[i-1] +  invest_premium_b[i])*(1+u_rate)*(1-asset_fee)}
  policy_value_e = unit_fund_value_e
  policy_value_e[9] = policy_value_e[9] + amount
  invest_premium = invest_premium_b * l[1:10]/l[1]
  unit_fund_value = unit_fund_value_e * c(l[2:10],0)/l[1]
  policy_value = policy_value_e * c(l[2:10],0)/l[1]
  int_unit = (invest_premium+c(0, policy_value[1:9]))*u_rate
  
  transfer_to_nunit = (invest_premium + int_unit - unit_fund_value)[1]
  for(i in 2:10){transfer_to_nunit[i] = (invest_premium + int_unit - diff(c(0, policy_value)))[i]}
  
  
  #non-unit fund
  acq_fee = c(0.45, rep(0,9))*premium_b
  p_related_fee_b = premium_b * p_fee_b
  bs_margin_b = premium_b*(1-p_fee_b)*bs
  exp_b = 58*(1+0.2)^(0:9); exp_b[1] = exp_b[1] + 0.45*premium_b[1]
  benefit_e = unit_fund_value_e
  
  p_related_fee = p_related_fee_b * l/l[1]
  exp = exp_b * l/l[1]
  bs_margin = bs_margin_b * l/l[1]
  int_nunit = (p_related_fee + bs_margin - exp)*nu_rate
  benefit_death = benefit_e * ald/l[1]
  benefit_with = benefit_e * alw/l[1]
  
  transfer = p_related_fee + bs_margin + int_nunit - exp - benefit_death - benefit_with + transfer_to_nunit
  return(sum(transfer/(1.125)^(1:10)))
}

#(a)
transfer
sum(transfer/(1.125)^(1:10))

#(b)
uniroot(increase, c(3,4))[1]

#(c)
npv_new(3.713458)
