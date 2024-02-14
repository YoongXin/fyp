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
    "      [4] it is the case that Bob is forbidden to charge AMOUNT \"delivery fee\" to Alice on ANYDATE " ++
    " C-AND " ++ 
    " IF   [5] it is the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021 " ++ 
    "      AND " ++ 
    "      [6] it is not the case that Bob charged AMOUNT \"delivery fee\" to Alice on ANYDATE " ++ 
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

isdaModified = 
    " IF    [1] it is the case that PartyA shall pay AMOUNT AmountA to PartyB on the 01 January 1970 " ++ 
    "       AND " ++
    "       [2] it is the case that PartyB shall pay AMOUNT AmountB to PartyA on the 01 January 1970 " ++
    " THEN  [3] it is not the case that PartyA shall pay AMOUNT AmountA to PartyB on the 01 January 1970 " ++
    "       AND " ++ 
    "       [4] it is not the case that PartyB shall pay AMOUNT AmountB to PartyA on the 01 January 1970 " ++
    "       AND " ++
    "       [5] it is the case that Excessparty shall pay AMOUNT ExcessAmount to AnotherParty on THEDATE unknownOne " ++
    " C-AND " ++
    " IF    [6] it is the case that PartyA paid more than PartyB " ++
    " THEN  [7] ExcessParty IS PartyA " ++ 
    "       AND " ++
    "       [8] ExcessAmount EQUALS AMOUNT AmountA MINUS AMOUNT AmountB " ++
    " C-AND " ++
    " IF    [9] it is the case that PartyB paid more than PartyA " ++
    " THEN  [10] ExcessParty IS PartyA " ++ 
    "       AND " ++
    "       [11] ExcessAmount EQUALS AMOUNT AmountB MINUS AMOUNT AmountA " 

guarantor = 
    "       [1] it is the case that Landlord must deliver OTHEROBJECT property to Tenant on the 2 April 2023 " ++
    " C-AND " ++
    " IF    [2] it is the case that Landlord delivered OTHEROBJECT demandOfTenantPayment to Tenant on the 2 April 2023 " ++
    "       AND " ++
    "       [3] it is not the case that Tenant paid AMOUNT AmountA to Landlord before 5 April 2023 " ++
    " THEN  [4] it is the case that Landlord may deliver OTHEROBJECT demandOfGuarantorPayment to Tenant on the 6 April 2023 " ++
    " C-AND " ++
    " IF    [5] it is the case that Landlord delivered OTHEROBJECT demandOfTenantPayment to Tenant on the 2 April 2023 " ++ 
    "       AND " ++ 
    "       [6] it is not the case that Tenant paid AMOUNT AmountA to Landlord before 10 April 2023 " ++ 
    "       AND " ++
    "       [7] it is the case that Landlord delivered OTHEROBJECT demandOfGuarantorPayment to Tenant on the 6 April 2023 " ++
    " THEN  [8] it is the case that Guarantor must pay AMOUNT AmountA to Landlord on the 11 April 2023 " ++
    " C-AND " ++
    " IF    [9] it is not the case that Tenant paid AMOUNT AmountB to Landlord before 11 September 2023 " ++ 
    " THEN  [10] it is the case that Guarantor must pay AMOUNT AmountB to Landlord on the 12 September 2023 " ++
    " C-AND " ++
    " IF    [11] it is the case that HousingBenefitScheme paid AMOUNT AmountC to Tenant on the 1 January 2024 " ++
    "       AND " ++
    "       [12] it is the case that LocalAuthority delivered OTHEROBJECT overpaymentClaim to Tenant on the 7 January 2024 " ++ 
    " THEN  [13] it is the case that Guarantor must pay AMOUNT AmountC to Landlord before 10 January 2024"
