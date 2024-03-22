from Contract import Contract
from Definition import *
from Expression import *
from Statement import *
from Condition import *

def bikeDelivery():
    con = Contract()

    con.statement(TemporalStatement('"Bob"', 'MAY', 'Deliver', 'Report ""receipt""', 'to "Alice"',
                                    TemporalExpression('On', 'ANYDATE'), valid=True))
    con.statement(TemporalStatement('"Bob"', 'SHANT', 'Charge', 'Amount ""delivery fee""', 'to "Alice"',
                                    TemporalExpression('On', 'ANYDATE'), valid=True))
    con.statement(ConditionalStatement(condition=OrCondition(conditions=[
        TemporalActionCondition('"Alice"', 'Paid', '£100', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True),
        TemporalActionCondition('"Alice"', 'Paid', '$120', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True)]), statement=TemporalStatement('"Bob"', 'SHALL', 'Deliver',
                                                                          'OtherObject ""bicycle""', 'to "Alice"',
                                                                          TemporalExpression('ON', '5 April 2021'),
                                                                          valid=True)))
    con.interactiveSimulation()

def bikeDeliverySanction():
    con = Contract()

    con.statement(ConditionalStatement(condition=OrCondition(conditions=[
        TemporalActionCondition('"Alice"', 'Paid', '£100', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True),
        TemporalActionCondition('"Alice"', 'Paid', '$120', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True)]), statement=TemporalStatement('"Bob"', 'SHALL', 'Deliver',
                                                                          'OtherObject ""bicycle""', 'to "Alice"',
                                                                          TemporalExpression('ON', '5 April 2021'),
                                                                          valid=True)))
    con.statement(ConditionalStatement(condition=OrCondition(conditions=[
        TemporalActionCondition('"Alice"', 'Paid', '£100', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True),
        TemporalActionCondition('"Alice"', 'Paid', '$120', 'to "Bob"', TemporalExpression('ON', '1 April 2021'),
                                test=True)]), statement=TemporalStatement('"Bob"', 'SHANT', 'Charge',
                                                                          'Amount ""delivery fee""', 'to "Alice"',
                                                                          TemporalExpression('BEFORE', '8 April 2021'),
                                                                          valid=True)))
    con.statement(ConditionalStatement(condition=AndCondition(conditions=[
        TemporalActionCondition('"Bob"', 'Delivered', 'OtherObject ""bicycle""', 'to "Alice"',
                                TemporalExpression('ON', '5 April 2021'), test=True),
        TemporalActionCondition('"Bob"', 'Charged', 'Amount ""delivery fee""', 'to "Alice"',
                                TemporalExpression('BEFORE', '8 April 2021'), test=False)]),
                                       statement=TemporalStatement('"Bob"', 'MAY', 'Deliver', 'Report ""receipt""',
                                                                   'to "Alice"', TemporalExpression('On', 'ANYDATE'),
                                                                   valid=True)))
    con.statement(ConditionalStatement(
        condition=TemporalActionCondition('"Bob"', 'Delivered', 'OtherObject ""bicycle""', 'to "Alice"',
                                          TemporalExpression('ON', '5 April 2021'), test=False),
        statement=TemporalStatement('"Alice"', 'MAY', 'Charge', '£100', 'to "Bob"',
                                    TemporalExpression('ON', '8 April 2021'), valid=True)))
    con.interactiveSimulation()

def isdaOriginal():
    con = Contract()

    con.statement(TemporalStatement('"ExcessParty"', 'SHALL', 'Pay', 'Amount ""ExcessAmount""', 'to "AnotherParty"',
                                    TemporalExpression('ON', 'THEDATE "unknownOne"'), valid=True))
    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(
        statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'Amount ""AmountA""', 'to "PartyB"',
                                    TemporalExpression('ON', 'SOMEDATE "unknownOne"'), valid=True), test=True),
                                                                          StatementCondition(
                                                                              statement=TemporalStatement('"PartyB"',
                                                                                                          'SHALL',
                                                                                                          'Pay',
                                                                                                          'Amount ""AmountB""',
                                                                                                          'to "PartyA"',
                                                                                                          TemporalExpression(
                                                                                                              'ON',
                                                                                                              'THEDATE "unknownOne"'),
                                                                                                          valid=True),
                                                                              test=True)]),
                                       statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'Amount ""AmountA""',
                                                                   'to "PartyB"',
                                                                   TemporalExpression('ON', 'THEDATE "unknownOne"'),
                                                                   valid=False)))
    con.statement(ConditionalStatement(condition=AndCondition(conditions=[StatementCondition(
        statement=TemporalStatement('"PartyA"', 'SHALL', 'Pay', 'Amount ""AmountA""', 'to "PartyB"',
                                    TemporalExpression('ON', 'SOMEDATE "unknownOne"'), valid=True), test=True),
                                                                          StatementCondition(
                                                                              statement=TemporalStatement('"PartyB"',
                                                                                                          'SHALL',
                                                                                                          'Pay',
                                                                                                          'Amount ""AmountB""',
                                                                                                          'to "PartyA"',
                                                                                                          TemporalExpression(
                                                                                                              'ON',
                                                                                                              'THEDATE "unknownOne"'),
                                                                                                          valid=True),
                                                                              test=True)]),
                                       statement=TemporalStatement('"PartyB"', 'SHALL', 'Pay', 'Amount ""AmountB""',
                                                                   'to "PartyA"',
                                                                   TemporalExpression('ON', 'THEDATE "unknownOne"'),
                                                                   valid=False)))
    con.definition(ConditionalDefinition(
        condition=ExpressionCondition(BooleanExpression('"PartyA"', 'Paid', 'MoreThan', '"PartyB"'), test=True),
        definitions=[IsDefinition('"ExcessParty"', '"PartyA"'),
                     EqualsDefinition('"ExcessAmount"', '"AmountA" - "AmountB"')])
                   )
    con.definition(ConditionalDefinition(
        condition=ExpressionCondition(BooleanExpression('"PartyB"', 'Paid', 'MoreThan', '"PartyA"'), test=True),
        definitions=[IsDefinition('"ExcessParty"', '"PartyB"'),
                     EqualsDefinition('"ExcessAmount"', '"AmountB" - "AmountA"')])
                   )
    con.interactiveSimulation()

def ifThenElese():
    con = Contract()

    con.statement(ConditionalStatement(condition=TemporalActionCondition('"Alice"', 'Charged', '£10', 'to "Bob"',
                                                                         TemporalExpression('ON', '1 April 2021'),
                                                                         test=True),
                                       statement=TemporalStatement('"Alice"', 'SHALL', 'Deliver',
                                                                   'OtherObject ""orange""', 'to "Bob"',
                                                                   TemporalExpression('ON', '4 April 2021'),
                                                                   valid=True)))
    con.statement(ConditionalStatement(condition=TemporalActionCondition('"Alice"', 'Charged', '£10', 'to "Bob"',
                                                                         TemporalExpression('ON', '1 April 2021'),
                                                                         test=False),
                                       statement=TemporalStatement('"Alice"', 'MAY', 'Refund', '£10', 'to "Bob"',
                                                                   TemporalExpression('ON', '5 April 2021'),
                                                                   valid=True)))
    con.interactiveSimulation()

def empty():
    con = Contract()
    con.interactiveSimulation()

empty()
