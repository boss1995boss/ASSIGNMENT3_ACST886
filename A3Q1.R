#Question1
j = 0.0192308
qx = c(0.001046, 0.001199, 0.001375)
benefit_end = 15000*(1+j)^(0:2)
i_exp_b = c(100+0.015*40250, 0, 0)
p_b = c(1,0,0)*40250
a_exp_end = 10*1.05^(0:2)
policy_value_end = c(c(1-0.889132, (1 - 0.924607)*(1+j))*15000/(0.04/1.04), 
                     15000*(1+j)^2) - 15000*(1+j)^(0:2)
int_end = numeric()
int_end[1] = (p_b[1] - i_exp_b[1])*0.08
for(i in 2:3){int_end[i] = (policy_value_end[i-1] - a_exp_end[i-1])*0.08};
l = numeric(); l[1] = 100000
d = numeric()
for(i in 1:3){l[i+1] = l[i]*(1-qx[i]); d[i] = l[i]*qx[i]}
p = p_b*l[1:3]/l[1]; i_exp = i_exp_b*l[1:3]/l[1]; int = int_end*l[1:3]/l[1]
a_exp = a_exp_end*l[2:4]/l[1]
policy_value = policy_value_end*l[2:4]/l[1]
benefit = benefit_end*l[2:4]/l[1]

transfer = p-i_exp+int-a_exp-benefit-diff(c(0,policy_value))

table1 = cbind(l, c(d,NA)); colnames(table1) = c("lx", "dx"); rownames(table1) = c(55:58)
table1

inforce = cbind(p_b, i_exp_b, a_exp_end, int_end, benefit_end, policy_value_end)
inforce
expected = cbind(p, i_exp, a_exp, int, benefit, policy_value, transfer)
expected


##(a)
transfer
npv = sum(transfer/(1.12)^(1:3))
margin = npv / sum(p_b/(1.12)^(0:2))
npv;margin

##(b)
irr = function(r){sum(transfer/(1+r)^(1:3))}
uniroot(irr, c(0,1))[1]
