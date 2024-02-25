module ExampleContracts where

emptyContract = " "

simpleDefinition1 = "[1] PartyA IS Alice"
simpleDefinition2 = "[1] AmountA EQUALS 14"
simpleDefinition3 = "[1] AmountA EQUALS POUNDS 12"
simpleDefinition4 = "[1] AmountA EQUALS GBP 12"
simpleDefinition5 = "[1] AmountA EQUALS quid 12"
simpleDefinition6 = "[1] AmountA EQUALS DOLLARS 23"
simpleDefinition7 = "[1] AmountA EQUALS USD 23"
simpleDefinition8 = "[1] AmountA EQUALS buck 23"
simpleDefinition9 = "[1] AmountA EQUALS EUR 6"
simpleDefinition10 = "[1] AmountA EQUALS EUROS 6"
simpleDefinition11 = "[1] AmountA EQUALS 23 PLUS 56"
simpleDefinition12 = "[1] AmountA EQUALS 2000 MINUS 380"
simpleDefinition13 = "[1] AmountA EQUALS 50 TIMES 390"
simpleDefinition14 = "[1] AmountA EQUALS 3900 DIVIDE 30"
simpleDefinition15 = "[1] DateOne IS 2 November 2001"

andDefinition1 = "[1] PartyA IS Alice AND [2] PartyB IS Bob"
andDefinition2 = "[1] AmountA EQUALS POUNDS 20 AND [2] AmountB EQUALS DOLLARS 100"
andDefinition3 = "[1] DateOne IS 3 April 2002 AND [2] DateTwo IS 20 September 2002"
andDefinition4 = "[1] PartyA IS Alice AND [2] AmountA EQUALS POUNDS 100 AND [3] DateA IS 29 February 2024"

conditionalDefinition1 = "IF [1] it is the case that Alice paid POUNDS 100 to Bob on the 14 July 2024 THEN [2] PartyA IS Alice"
conditionalDefinition2 = "IF [1] it is not the case that Alice paid POUNDS 100 to Bob on the 14 July 2024 THEN [2] PartyA IS Carol"
conditionalDefinition3 = "IF [1] it is the case that Alice on 30 April 2021 paid POUNDS 100 to Bob THEN [2] DateOne IS 5 May 2021"
conditionalDefinition4 = "IF [1] it is not the case that Alice on 30 April 2021 paid POUNDS 100 to Bob THEN [2] PartyA IS Carol"
conditionalDefinition5 = "IF [1] it is the case that on SOMEDATE DateOne Mary delivered OTHEROBJECT property to Daniel THEN [2] DateOne IS 5 May 2021"
conditionalDefinition6 = "IF [1] it is not the case that on ANYDATE Kelly delivered OTHEROBJECT \"big house\" to Carol THEN [2] PartyA IS Carol"
conditionalDefinition7 = "IF [1] it is not the case that John delivered OTHEROBJECT property to Lewis 7 weeks after SOMEDATE DateOne THEN [2] DateOne IS 5 May 2021"
conditionalDefinition8 = "IF [1] it is the case that Kelly must charge AMOUNT rental to Carol before 1 January 2024 THEN [2] PartyA IS Carol"
conditionalDefinition9 = "IF [1] it is the case that James refunded more than Robert THEN [2] PartyA IS James"
conditionalDefinition10 = "IF [1] it is not the case that James refunded more than Robert THEN [2] PartyA IS Robert"
conditionalDefinition11 = "[1] PartyA IS Alice IF [2] Alice charged DOLLARS 20 to Bob on 5 May 2023"
conditionalDefinition12 = "[1] AmountA EQUALS POUNDS 30 IF [2] Alice on SOMEDATE unknownOne delivered OTHEROBJECT apple to Bob"
conditionalDefinition13 = "[1] DateOne IS 7 January 2024 IF [2] on THEDATE DateOne Charles paid EUROS 50 to William"
conditionalDefinition14 = "[1] PartyB IS Dominic IF [2] Ken must charge POUNDS 14 to Dominic on the 7 June 2023"
conditionalDefinition15 = "[1] PartyA IS Bob IF [2] Bob delivered more than Clarice"
conditionalDefinition16 = "[1] DateOne IS 7 March 2021 IF [2] Ken paid POUNDS 20 to Anthony on SOMEDATE DateOne ELSE [3] DateOne IS 20 March 2021"
conditionalDefinition17 = "IF [1] Lucy paid POUNDS 20 to Jacob on 6 December 2023 THEN [2] PartyA IS Lucy ELSE [3] PartyA IS Lily"
conditionalDefinition18 = "IF [1] Aiden must deliver OTHEROBJECT strawberry to Mary on 8 November 2023 THEN [2] AmountA EQUALS POUNDS 20 ELSE [3] AmountA EQUALS DOLLARS 15 "

simpleStatement1 = "[1] it is the case that Alice must pay POUNDS 100 to Bob on 1 January 2021"
simpleStatement2 = "[1] it is not the case that Carol may deliver NAMEDOBJECT Fluffy to Darek on the 2 February 2022 "
simpleStatement3 = "[1] it is the case that Eddie on ANYDATE is forbidden to charge AMOUNT extra to Frank"
simpleStatement4 = "[1] it is not the case that Grace on SOMEDATE unknownOne shall refund DOLLARS 5 to Helen"
simpleStatement5 = "[1] it is the case that on THEDATE dateOne Ivana must pay DOLLARS 35 to Joe "
simpleStatement6 = "[1] it is not the case that before 3 March 2023 Kate may deliver OTHEROBJECT paper to Leo"
simpleStatement7 = "[1] it is the case that Mary paid EUROS 40 to Nancy after 4 April 2024"
simpleStatement8 = "[1] it is not the case that Olivia delivered REPORT receipt to Patrick before SOMEDATE unknown"
simpleStatement9 = "[1] Qadir must charge POUNDS 31 to Rachel after THEDATE dateTwo"
simpleStatement10 = "[1] Selena 10 days before SOMEDATE unknown may deliver OTHEROBJECT pen to Teresa"
simpleStatement11 = "[1] 2 years after THEDATE dateOne Umy is forbidden to charge AMOUNT rental to Venus"
simpleStatement12 = "[1] William refunded DOLLARS 10 to Xavier on the 5 May 2025"
simpleStatement13 = "[1] Yamy is forbidden to charge POUNDS 4 to Zoey before 3 weeks after SOMEDATE unknown"

andStatement1 = "[1] it is the case that Alice must pay POUNDS 20 to Bob on the 6 June 2020 AND [2] it is the case that Bob must deliver OTHEROBJECT \"computer\" to Alice on 7 July 2020"
andStatement2 = "[1] it is not the case that John may deliver OTHEROBJECT poster to Lewis on 5 April 2021 AND [2] it is the case that John must pay POUNDS 38 to Carol after 6 April 2021"
andStatement3 = "[1] Dennis is forbidden to charge POUNDS 20 to Katherine on ANYDATE AND [2] Matthew on 4 August 2023 must deliver OTHEROBJECT scarf to Eric"
andStatement4 = "[1] William refunded DOLLARS 10 to Xavier on the 5 May 2025 AND [2] Cindy delivered REPORT receipt to William on 6 May 2025"

orStatement1 = "[1] it is the case that Alice must pay POUNDS 20 to Bob on 9 September 2023 OR [2] it is the case that Alice must pay DOLLARS 30 to Bob on 9 September 2023"
orStatement2 = "[1] it is not the case that Fred may deliver OTHEROBJECT hat to Casey on ANYDATE OR [2] it is the case that Casey is forbidden to pay EUROS 7 to Fred on ANYDATE"
orStatement3 = "[1] Mary on 4 June 2023 must deliver REPORT receipt to James OR [2] on the 7 June 2023 James must refund POUNDS 8 to Mary"
orStatement4 = "[1] Chloe may pay DOLLARS 10 to Joey on SOMEDATE unknown OR [2] Chloe may pay EUROS 13 to Joey on THEDATE unknown"

conditionalStatement1 = "IF [1] it is the case that Bob paid POUNDS 20 to Joe on the 5 September 2023 THEN [2] it is the case that Joe must deliver OTHEROBJECT book to Bob before 14 September 2023"
conditionalStatement2 = "IF [1] it is the case that Carl on SOMEDATE unknownOne delivered OTHEROBJECT phone to Kris THEN [2] it is not the case that Carl must refund POUNDS 500 to Kris after SOMEDATE unknownTwo"
conditionalStatement3 = "[1] Mernard must pay DOLLARS 45 to Linda before 10 December 2023 IF [2] it is the case that Linda delivered OTHEROBJECT bottle to Mernard on 3 December 2023"
conditionalStatement4 = "[1] Yuri before 14 April 2024 must deliver REPORT dissertation to John IF [2] it is the case that Yuri paid AMOUNT \"tuition fee\" to University on 21 September 2021"
conditionalStatement5 = "IF [1] it is the case that Harry paid POUNDS 180 to Tiffany on 7 July 2021 THEN [2] it is the case that Tifanny must deliver OTHEROBJECT glasses to Harry before 10 July 2021 ELSE [3] Tiffany may charge AMOUNT \"late payment fee\" to Harry on ANYDATE"
conditionalStatement6 = "IF [1] Mary charged more than Nancy THEN [2] Selena may deliver OTHEROBJECT \"request for refund\" to Mary on SOMEDATE dateOne"
conditionalStatement7 = "IF [1] Arek paid more than Bern THEN [2] it is the case that Carina must refund POUNDS 5 to Arek on 6 June 2024 ELSE [3] it is the case that Carina must refund POUNDS 5 to Bern on 6 June 2024"
conditionalStatement8 = "[1] it is the case that George must deliver OTHEROBJECT charger to Helen after SOMEDATE unknownOne IF [2] Helen paid POUNDS 12 to George on THEDATE unknownOne ELSE [3] George may charge POUNDS 2 to Helen on SOMEDATE unknownTwo"

definitionAndStatement = "[1] Buyer IS Alice C-AND [2] it is the case that Buyer must pay POUNDS 10 to Seller on the 5 April 2023 C-AND [3] Seller IS Bob"
definitionAndConditionalDefinition = "IF [1] it is the case that Helen paid more than Freddie THEN [2] Freddie IS PartyA ELSE [3] Helen IS ParyA C-AND [3] AmountOne EQUALS POUNDS 30"
definitionAndConditionalStatement = "[1] Carol must pay POUNDS 20 to Eric before SOMEDATE unknown IF [2] Eric delivered OTHEROBJECT notebook to Carol on SOMEDATE unknownTwo C-AND [3] unkwown IS 5 July 2023 C-AND [4] unknownTwo IS 2 July 2023"
statementAndConditionalDefinition = "[1] Joey must pay DOLLARS 64 to Dylan on 8 December 2023 C-AND [2] Carol IS PartyA IF [3] Carol charged DOLLARS 5 to Joey on SOMEDATE unknown"
statementAndConditionalStatement = "[1] Tilen is forbidden to charge AMOUNT extra to Ghalia on the 17 June 2023 C-AND IF [2] Ghalia paid EUROS 40 to Tilen on the 5 June 2023 THEN [3] Tilen must deliver OTHEROBJECT package to Ghalia on 14 June 2023"
conditionalStatementAndConditionalDefinition = "IF [1] Ali paid POUNDS 20 to Jonas on the 5 December 2021 THEN [2] Jonas must deliver OTHEROBJECT service to Ali before 1 January 2024 C-AND [3] PartyA IS Lucy IF [4] Lucy paid more than Benjamin"
definitionAndStatementAndConditionalDefinition = "[1] AmountA EQUALS POUNDS 20 C-AND [2] Alice is forbidden to refund AMOUNT AmountA to Bob on the 30 April 2021 C-AND IF [3] it is the case that Alice on 30 April 2021 paid POUNDS 100 to Bob THEN [4] DateOne IS 5 May 2021"
definitionAndStatementAndConditionalStatement = "[1] DateOne IS 11 May 2021 C-AND [2] Christine must pay EUROS 5 to Nathan on SOMEDATE DateOne C-AND IF [3] Joey delivered OTHEROBJECT pencil to Leo on 5 April 2021 THEN [4] it is the case that Leo must pay EUROS 10 to Joey after 5 April 2021"
definitionAndConditionalStatementAndConditionalDefinition = "[1] AmountOne EQUALS POUNDS 75 C-AND IF [2] Alice paid AMOUNT AmountOne to Bob on the 5 December 2023 THEN [3] Bob must deliver OTHEROBJECT ring to Alice on the 10 December 2023 C-AND IF [4] Neo paid more than Will THEN [5] PartyA IS Neo"
allFourComponentsAnd = "[1] PartyA IS Alice C-AND [2] Alice is forbidden to charge AMOUNT \"delivery fee\" to Bob on ANYDATE C-AND IF [3] Natalie paid more than Rachel THEN [4] Natalie IS PartyB C-AND IF [5] Viola paid POUNDS 20 to Leo on the 30 April 2023 THEN [6] it is the case that Leo may deliver REPORT receipt to Viola on ANYDATE"

consistencyContract1 = "IF [1] it is the case that Carol paid EUROS 30 to David before 20 September 2023 THEN [2] David must deliver OTHEROBJECT orange to Carol before 27 September 2023"
consistencyContract2 = "IF [1] Emily paid DOLLARS 45 to Frank on the 30 November 2023 THEN [2] Frank must deliver OTHEROBJECT notebook to Emily on ANYDATE"
consistencyContract3 = "IF [1] it is the case that Grace refunded POUNDS 20 to Helena on the 6 July 2021 THEN [2] Helena must deliver REPORT \"financial report\" to Grace on SOMEDATE unknownOne"
consistencyContract4 = "IF [1] it is the case that Ivana charged DOLLARS 13 to Jess before SOMEDATE unknownOne THEN [2] it is the case that Ivana must deliver OTHEROBJECT dress to Jess after THEDATE unknownOne"
consistencyContract5 = "IF [1] it is the case that Kathy paid EUROS 3 to Lily on SOMEDATE unknownOne THEN [2] it is the case that Lily must deliver OTHEROBJECT bicycle to Kathy before 7 days after SOMEDATE unknownOne"
consistencyContract6 = "IF [1] Mandy paid POUNDS 17 to Nancy on 7 April 2021 THEN [2] Nancy must deliver OTHEROBJECT bag to Mandy on SOMEDATE unknownOne C-AND [3] unknownOne IS 9 April 2021"
consistencyContract7 = "IF [1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 OR [4] it is the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 THEN [2] it is the case that Bob must deliver OTHEROBJECT bicycle to Alice on the 5 April 2021 ELSE [3] it is the case that Bob must deliver OTHEROBJECT orange to Alice on the 5 April 2021"

tqTestContract1 = "IF [1] it is the case that Alice paid POUNDS 3 to Bob before 3 January 2024 THEN [2] it is the case that Bob must deliver OTHEROBJECT watermelon to Alice before 10 January 2024"
tqTestContract2 = "[1] Cindy must deliver REPORT receipt to Alex after 8 September 2023 IF [2] Alex paid EUROS 10 to Cindy after 1 September 2023 ELSE [3] Cindy is forbidden to refund EUROS 10 to Alex before 10 September 2023"

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
