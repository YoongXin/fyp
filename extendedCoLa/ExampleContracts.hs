module ExampleContracts where

bikeDeliveryOriginal =
    " IF    [1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 " ++ 
    "       OR " ++
    "       [2] it is the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 " ++
    " THEN  [3] it is the case that Bob must deliver OTHEROBJECT bicycle to Alice on the 5 April 2021 " ++
    " C-AND [4] it is the case that Bob may deliver REPORT receipt to Alice on ANYDATE " ++
    "       AND " ++
    "       [5] it is the case that Bob is forbidden to charge AMOUNT \"delivery fee\" to Alice on ANYDATE " 

bikeDeliveryModified =
    " IF    [1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 " ++ 
    "       OR " ++
    "       [2] it is the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 " ++
    " THEN  [3] it is the case that Bob must deliver OTHEROBJECT bicycle to Alice on the 5 April 2021 " ++
    "       AND " ++
    "       [4] it is the case that Bob may deliver REPORT receipt to Alice on ANYDATE " ++
    "       AND " ++
    "       [5] it is the case that Bob is forbidden to charge AMOUNT \"delivery fee\" to Alice on ANYDATE " 

bikeDeliverySanction = 
    " IF   [1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 " ++
    "      OR " ++
    "      [2] it is the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 " ++
    " THEN [3] it is the case that Bob must deliver OTHEROBJECT bicycle to Alice on the 5 April 2021 " ++
    "      AND " ++ 
    "      [4] it is the case that Bob is forbidden to charge AMOUNT \"delivery fee\" to Alice before 8 April 2021 " ++
    " C-AND " ++ 
    " IF   [5] it is the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021 " ++ 
    "      AND " ++ 
    "      [6] it is not the case that Bob charged AMOUNT \"delivery fee\" to Alice before 8 April 2021 " ++ 
    " THEN [7] it is the case that Bob may deliver REPORT receipt to Alice on ANYDATE " ++ 
    " C-AND " ++ 
    "      [8] it is the case that Alice may charge POUNDS 100 to Bob on 8 April 2021 " ++ 
    " IF   [9] it is not the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021 "

isdaOriginal = 
    " IF   [1] it is the case that PartyA shall pay AMOUNT AmountA to PartyB on SOMEDATE unknownOne " ++
    "      AND " ++ 
    "      [2] it is the case that PartyB shall pay AMOUNT AmountB to PartyA on THEDATE unknownOne " ++
    " THEN [3] it is not the case that PartyA shall pay AMOUNT AmountA to PartyB on THEDATE unknownOne " ++ 
    "      AND " ++ 
    "      [4] it is not the case that PartyB shall pay AMOUNT AmountB to PartyA on THEDATE unknownOne " ++
    " C-AND " ++
    "      [5] it is the case that ExcessParty shall pay AMOUNT ExcessAmount to AnotherParty on THEDATE unknownOne " ++
    " C-AND " ++
    " IF   [6] it is the case that PartyA paid more than PartyB " ++ 
    " THEN [7] ExcessParty IS PartyA " ++ 
    "      AND " ++ 
    "      [8] ExcessAmount EQUALS AMOUNT AmountA MINUS AMOUNT AmountB " ++ 
    " C-AND " ++
    " IF   [9] it is the case that PartyB paid more than PartyA " ++ 
    " THEN [10] ExcessParty IS PartyB " ++ 
    "      AND " ++ 
    "      [11] ExcessAmount EQUALS AMOUNT AmountB MINUS AMOUNT AmountA " 