module SP6.Data.ID.Block where

-- base
import Data.Ix
-- data-default-class
import Data.Default.Class

data BlockID
    = VB000X
    -- Main Line (Ver.M)
    | VBN07D
    | VBN05DA
    | VBN05DB
    | VBN04D
    | VBN03D
    | VBN01D
    | VB000DA
    | VB000DB
    | VB002DA
    | VB002DB
    | VB003D
    | VB005D
    | VB007D
    | VB008D
    | VB010D
    | VB011D
    | VB013D
    | VB015D
    | VB016D
    | VB017D
    | VB018D
    | VB019D
    | VB021D
    | VB023DA
    | VB023DB
    | VB025DA
    | VB025DB
    | VB026DA
    | VB026DB
    | VB028D
    | VB029D
    | VB031D
    | VB033DA
    | VB033DB
    | VB035D
    | VB037D
    | VB039D
    | VB042D
    | VB043D
    | VB045D
    | VB046D
    | VB047D
    | VB048D
    | VB049D
    | VB052D
    | VB054D
    | VB056D
    | VB057D
    | VB059D
    | VB061D
    | VB063D
    | VB065D
    | VB068D
    | VB069D
    | VB071D
    | VB074D
    | VB076DA
    | VB076DB
    | VB077D
    | VB079D
    | VB080DA
    | VB080DB
    | VB081DA
    | VB081DB
    | VB081DC
    | VB082DA
    | VB082DB
    | VB082DC
    | VB084D
    | VB086D
    | VB088D
    | VB090D
    | VB092D
    | VB093D
    | VB095D
    | VB096D
    | VB097D
    | VB099D
    | VB100DA
    | VB100DB
    | VB104D
    | VB106D
    | VB108D
    | VB109D
    | VB111D
    | VB113D
    | VB115D
    | VB118D
    | VB120D
    | VB121D
    | VB122D
    | VB124D
    | VB126D
    | VB127D
    | VB129D
    | VB131D
    | VB133D
    | VB136D
    | VB138D
    | VB139D
    | VB141D
    | VB142D
    | VB143D
    | VB144D
    | VB147DA
    | VB147DB
    | VB148DA
    | VB148DB
    | VB148DC
    | VB149D
    | VB151D
    | VB153D
    | VB155D
    | VB156D
    | VB157D
    | VB158D
    | VB160D
    | VB161D
    | VB163D
    | VB164D
    | VB167D
    | VB168D
    | VB169DA
    | VB169DB
    | VB171DA
    | VB171DB
    | VB173DA
    | VB173DB
    | VB175DA
    | VB175DB
    | VB177D
    | VB178DA
    | VB178DB
    | VB178DC
    | VB180D
    | VB181DA
    | VB181DB
    | VB183D
    | VB184DA
    | VB184DB
    | VB185DA
    | VB185DB
    | VB185DC
    | VB186D
    | VB187D
    | VB189D
    | VB191DA
    | VB191DB
    | VB193D
    | VB195D
    | VB198D
    | VB199D
    | VB201D
    | VB203D
    | VB205D
    | VB206D
    | VB207D
    | VB208D
    | VB210D
    | VB212D
    | VB214D
    | VB215D
    | VB217DA
    | VB217DB
    | VB219DA
    | VB219DB
    | VB219DC
    | VB220DA
    | VB220DB
    | VB221D
    | VB223D
    | VB225DA
    | VB225DB
    | VB227D
    | VB229D
    | VB231D
    | VB233D
    | VB235D
    | VB237D
    | VB238D
    | VB239DA
    | VB239DB
    | VB241D
    | VB242D
    | VB243D
    | VB245D
    | VB246DA
    | VB246DB
    | VB248D
    | VB250D
    | VB251D
    | VB253D
    | VB255DA
    | VB255DB
    | VB257D
    | VB259D
    | VB262D
    | VB263D
    | VB264D
    | VB265D
    | VB266D
    | VB267DA
    | VB267DB
    | VB269D
    | VB270D
    | VB272D
    | VB275D
    | VB277D
    | VB278D
    | VB279D
    | VB280D
    | VB282D
    | VB283D
    | VB285D
    | VB286D
    | VB288DA
    | VB288DB
    | VB290D
    | VB291D
    | VB294D
    | VB296D
    | VB297D
    | VB299D
    | VB301D
    | VB302D
    | VB304DA
    | VB304DB
    | VB306D
    | VB307D
    | VB309DA
    | VB309DB
    | VB311D
    | VB313D
    | VB315D
    | VB318D
    | VB320D
    | VB322D
    | VB324D
    | VB326D
    | VB327D
    | VB328D
    | VB331D
    | VB333D
    | VB334D
    | VB336D
    | VB339D
    | VB340D
    | VB342DA
    | VB342DB
    | VB344D
    | VB346D
    | VB348D
    | VB350D
    | VB352D
    | VB354D
    | VB356D
    | VB358D
    | VB359D
    | VB360DA
    | VB360DB
    | VB362D
    | VB363DA
    | VB363DB
    | VBN07U
    | VBN05UA
    | VBN05UB
    | VBN04UA
    | VBN04UB
    | VBN03U
    | VBN01U
    | VB000UA
    | VB000UB
    | VB002UA
    | VB002UB
    | VB003U
    | VB005U
    | VB007U
    | VB008U
    | VB010U
    | VB012U
    | VB014UA
    | VB014UB
    | VB016U
    | VB018U
    | VB019U
    | VB021U
    | VB023U
    | VB024UA
    | VB024UB
    | VB026UA
    | VB026UB
    | VB027U
    | VB029U
    | VB030U
    | VB031U
    | VB032U
    | VB035U
    | VB037UA
    | VB037UB
    | VB039U
    | VB041U
    | VB043U
    | VB045U
    | VB046U
    | VB048U
    | VB050U
    | VB052U
    | VB053U
    | VB055U
    | VB056U
    | VB058U
    | VB059U
    | VB061U
    | VB063U
    | VB065U
    | VB066U
    | VB068U
    | VB069U
    | VB072U
    | VB074U
    | VB076U
    | VB077UA
    | VB077UB
    | VB079U
    | VB080UA
    | VB080UB
    | VB081U
    | VB082UA
    | VB082UB
    | VB082UC
    | VB082UD
    | VB084UA
    | VB084UB
    | VB085U
    | VB088U
    | VB090U
    | VB092U
    | VB093U
    | VB095U
    | VB096UA
    | VB096UB
    | VB097U
    | VB099U
    | VB101U
    | VB102U
    | VB103U
    | VB104U
    | VB106U
    | VB108U
    | VB110U
    | VB112U
    | VB113U
    | VB116U
    | VB118U
    | VB120UA
    | VB120UB
    | VB122U
    | VB124U
    | VB126U
    | VB127U
    | VB129U
    | VB131U
    | VB133U
    | VB136U
    | VB137U
    | VB138U
    | VB140U
    | VB141U
    | VB142U
    | VB144U
    | VB145U
    | VB147UA
    | VB147UB
    | VB148UA
    | VB148UB
    | VB149U
    | VB150U
    | VB151U
    | VB153U
    | VB155UA
    | VB155UB
    | VB156U
    | VB157U
    | VB158U
    | VB160U
    | VB161U
    | VB162U
    | VB164U
    | VB166U
    | VB168U
    | VB169UA
    | VB169UB
    | VB171UA
    | VB171UB
    | VB173UA
    | VB173UB
    | VB175U
    | VB176U
    | VB178UA
    | VB178UB
    | VB180U
    | VB182U
    | VB183U
    | VB184UA
    | VB184UB
    | VB185UA
    | VB185UB
    | VB186UA
    | VB186UB
    | VB187U
    | VB189UA
    | VB189UB
    | VB191U
    | VB193U
    | VB194U
    | VB197U
    | VB199U
    | VB201U
    | VB203U
    | VB205U
    | VB206U
    | VB208U
    | VB210U
    | VB212U
    | VB213U
    | VB214U
    | VB215U
    | VB217U
    | VB218U
    | VB219UA
    | VB219UB
    | VB219UC
    | VB220UA
    | VB220UB
    | VB221U
    | VB223UA
    | VB223UB
    | VB225U
    | VB227U
    | VB229U
    | VB231U
    | VB233U
    | VB235U
    | VB236U
    | VB237U
    | VB239U
    | VB240U
    | VB242UA
    | VB242UB
    | VB244U
    | VB245U
    | VB246UA
    | VB246UB
    | VB248U
    | VB249U
    | VB251U
    | VB253UA
    | VB253UB
    | VB255U
    | VB257U
    | VB259U
    | VB261U
    | VB263UA
    | VB263UB
    | VB264U
    | VB265U
    | VB267UA
    | VB267UB
    | VB268U
    | VB270U
    | VB272U
    | VB274U
    | VB276U
    | VB277U
    | VB279U
    | VB281U
    | VB282U
    | VB283U
    | VB286U
    | VB287U
    | VB288U
    | VB290U
    | VB293U
    | VB295U
    | VB297U
    | VB298U
    | VB300U
    | VB302UA
    | VB302UB
    | VB303U
    | VB305U
    | VB306U
    | VB307U
    | VB309UA
    | VB309UB
    | VB311U
    | VB313U
    | VB314U
    | VB316U
    | VB318U
    | VB320U
    | VB323U
    | VB324U
    | VB325U
    | VB326U
    | VB328U
    | VB329U
    | VB331U
    | VB333U
    | VB335U
    | VB337U
    | VB339U
    | VB340U
    | VB341U
    | VB342U
    | VB344U
    | VB346U
    | VB348U
    | VB350U
    | VB352U
    | VB355U
    | VB357U
    | VB359U
    | VB360UA
    | VB360UB
    | VB361U
    | VB363UA
    | VB363UB
    | VB364U

    | VB094MA
    | VB094MB
    | VB095M
    
    -- JLA L9

    | VB303L9
    | VB304L9
    | VB306L9

    -- Depot
    | VB318L1
    | VB316L2
    | VB317L2
    | VB318L2
    | VB317L3
    | VB318L3
    | VB317L4
    | VB318L4
    | VB317L5
    | VB318L5
    | VB315L6
    | VB316L6
    | VB318L6
    | VB318L7
    | VB316L8
    | VB317L8
    | VB318L8
    | VB312L9
    | VB314L9
    | VB315L9
    | VB316L9
    | VB318L9
    | VB312L10
    | VB314L10
    | VB315L10
    | VB316L10
    | VB317L10
    | VB318L10
    | VB317L11
    | VB318L11
    | VB316L12A
    | VB316L12B
    | VB318L12
    | VB316L13
    | VB317L13
    | VB318L13
    | VB317L14
    | VB318L14
    | VB316E
    | VB316M
    | VB317M
    | VB318S
    | VB319S
    | VB320S
    | VB313SA
    | VB313SB
    | VB313TA
    | VB313TB
    | VB315T
    | VB316T
    | VB317T
    | VB318TA
    | VB318TB
    | VB320T
    | VB313UL
    | VB314UL

    | VB308L9
    | VB309L9
    | VB310L9A
    | VB310L9B
--     | VB311L9 (Ver. AA手動改修）

    | VB309L10A
    | VB309L10B
    | VB310L10A
    | VB310L10B
--     | VB311L10 (Ver. AA手動改修）

    --  Stabling lines
    | VB302S1
    | VB304S1
    | VB302S2
    | VB304S2
    | VB302S3
    | VB304S3
    | VB302S4A
    | VB302S4B
    | VB304S4
    | VB305S4
    | VB301S5
    | VB303S5
    | VB304S5
    | VB301S6
    | VB302S6
    | VB303S6
    | VB305S6
    | VB300S7
    | VB302S7
    | VB304S7
    | VB306S7
    | VB307S7A
    | VB307S7B
    | VB300S8
    | VB302S8
    | VB304S8
    | VB297S9A
    | VB297S9B
    | VB297S9C
    | VB299S9A
    | VB299S9B
    | VB300S9
    | VB301S9
    | VB303S9A
    | VB303S9B

    | VB304S9 -- 特殊！

    | VB300S10
    | VB301S10
    | VB306S11
    | VB307S11A
    | VB307S11B
    | VB309S11

    -- T806H
    | VB_SL1
    | VB_SL2
    | VB_SL3
    | VB_SL4
    | VB_SL5
    | VB_SL6
    | VB_SL7
    -- T830H
    | VB_SR1
    | VB_SR2
    | VB_SR3
    | VB_SR4
    | VB_SR5
    | VB_SR6
    | VB_SR7
    -- Shunting Neck Right
    | VB_SNR
    deriving (Show, Ord, Eq, Enum, Bounded, Ix)

instance Default BlockID where def = VB000X
