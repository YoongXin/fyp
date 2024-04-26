%TPTP representation for the contract:

fof(contract, axiom, (((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,60312))) & (objectPound(O,5000))) & (paid(X,Y,O,D)))) => (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,60314))) & (objectOtherObject(O,car))) & (mustDeliver(X,Y,O,D))))) & ((! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,60319))) & (objectReport(O,receipt))) & (mustDeliver(X,Y,O,D)))) & (~ (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,60319))) & (objectReport(O,receipt))) & (mayDeliver(X,Y,O,D)))))))).

%TPTP representation for the performance:

fof(performance, axiom, (empty(empty))).

fof(mustCondition, axiom, (! [X,Y,O,D] :(mustDeliver(X,Y,O,D) => delivered(X,Y,O,D)) & (mustPay(X,Y,O,D) => paid(X,Y,O,D)) & (mustCharge(X,Y,O,D) => charged(X,Y,O,D)) & (mustRefund(X,Y,O,D) => refunded(X,Y,O,D)))).

fof(forbiddenContradiction, axiom, (! [X, Y, D, O] : ((~ mayDeliver(X, Y, O, D) & delivered(X, Y, O, D)) |(~ mayPay(X, Y, O, D) & paid(X, Y, O, D)) |(~ mayCharge(X, Y, O, D) & charged(X, Y, O, D)) |(~ mayRefund(X, Y, O, D) & refunded(X, Y, O, D)) => $false))).

fof(mustWithTemporalQuantifierContradiction, axiom, (! [X, Y, O, D] : ((mustDeliverAfter(X, Y, O, D) & ~ deliveredAfter(X, Y, O, D)) |(mustPayAfter(X, Y, O, D) & ~ paidAfter(X, Y, O, D)) |(mustChargeAfter(X, Y, O, D) & ~ chargedAfter(X, Y, O, D)) |(mustRefundAfter(X, Y, O, D) & ~ refundedAfter(X, Y, O, D)) |(mustDeliverBefore(X, Y, O, D) & ~ deliveredBefore(X, Y, O, D)) |(mustPayBefore(X, Y, O, D) & ~ paidBefore(X, Y, O, D)) |(mustChargeBefore(X, Y, O, D) & ~ chargedBefore(X, Y, O, D)) |(mustRefundBefore(X, Y, O, D) & ~ refundedBefore(X, Y, O, D)) => $false))).