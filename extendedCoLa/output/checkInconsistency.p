%TPTP representation for the contract:

fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Emily)) & (name(Y,Frank))) & (date(D,60278))) & (objectDollar(O,45))) & (paid(X,Y,O,D)))) => (! [X,Y,O] : (? [D] : ((((name(X,Frank)) & (name(Y,Emily))) & (objectOtherObject(O,notebook))) & (mustDeliver(X,Y,O,D))))))).

%TPTP representation for the performance:

fof(performance, axiom, ((! [X,Y,O,D] : (((((name(X,Emily)) & (name(Y,Frank))) & (date(D,60278))) & (objectDollar(O,45))) & (paid(X,Y,O,D)))) & (~ (! [X,Y,O] : (? [D] : ((((name(X,Frank)) & (name(Y,Emily))) & (objectOtherObject(O,notebook))) & (delivered(X,Y,O,D)))))))).

fof(mustCondition, axiom, (! [X,Y,O,D] :(mustDeliver(X,Y,O,D) => delivered(X,Y,O,D)) & (mustPay(X,Y,O,D) => paid(X,Y,O,D)) & (mustCharge(X,Y,O,D) => charged(X,Y,O,D)) & (mustRefund(X,Y,O,D) => refunded(X,Y,O,D)))).

fof(forbiddenContradiction, axiom, (! [X, Y, D, O] : ((~ mayDeliver(X, Y, O, D) & delivered(X, Y, O, D)) |(~ mayPay(X, Y, O, D) & paid(X, Y, O, D)) |(~ mayCharge(X, Y, O, D) & charged(X, Y, O, D)) |(~ mayRefund(X, Y, O, D) & refunded(X, Y, O, D)) => $false))).

fof(mustWithTemporalQuantifierContradiction, axiom, (! [X, Y, O, D] : ((mustDeliverAfter(X, Y, O, D) & ~ deliveredAfter(X, Y, O, D)) |(mustPayAfter(X, Y, O, D) & ~ paidAfter(X, Y, O, D)) |(mustChargeAfter(X, Y, O, D) & ~ chargedAfter(X, Y, O, D)) |(mustRefundAfter(X, Y, O, D) & ~ refundedAfter(X, Y, O, D)) |(mustDeliverBefore(X, Y, O, D) & ~ deliveredBefore(X, Y, O, D)) |(mustPayBefore(X, Y, O, D) & ~ paidBefore(X, Y, O, D)) |(mustChargeBefore(X, Y, O, D) & ~ chargedBefore(X, Y, O, D)) |(mustRefundBefore(X, Y, O, D) & ~ refundedBefore(X, Y, O, D)) => $false))).