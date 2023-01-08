module SP6.Data.SegmentDiagram where

import SP6.Data.ID
import SP6.Data.Common

-- array
import Data.Array.IArray

arrTrackBlock :: Array TrackID [BlockID]
arrTrackBlock = safeArray
    [ (T800X,[])
    , (T801G,[VB312L9])
    , (T802G,[VB312L10])
    , (T803G,[VB313SB])
    , (T804G,[VB314UL])
    , (T805G,[VB316M])
    , (T806G,[VB316L6])
    , (T807G,[VB316L8])
    , (T808G,[VB316L9])
    , (T809G,[VB317L10])
    , (T810G,[VB317L11])
    , (T811G,[VB316L12B])
    , (T812G,[VB317L13])
    , (T813G,[VB317L14])
    , (T814G,[VB318S])
    , (T815G,[VB319S])
    , (T816G,[VB313TB])
    , (T817G,[VB315T,VB316T])
    , (T818G,[VB317T])
    , (T819G,[VB318TA])
    , (T820G,[VB318TB])
    , (T801H,[VB309L9,VB310L9A])
    , (T802H,[VB309L10B,VB310L10A])
    , (T803H,[VB310L9B])
    , (T804H,[VB310L10B])
    , (T805H,[VB297S9A,VB297S9B,VB297S9C])
    , (T806H,[VB_SL1,VB_SL2,VB_SL3,VB_SL4,VB_SL5,VB_SL6,VB_SL7])
    , (T807H,[VB302S1])
    , (T808H,[VB302S2])
    , (T809H,[VB302S3])
    , (T810H,[VB302S4A,VB302S4B])
    , (T811H,[VB301S5])
    , (T812H,[VB301S6,VB302S6])
    , (T813H,[VB300S7])
    , (T814H,[VB300S8])
    , (T815H,[VB300S9,VB301S9])
    , (T816H,[VB300S10,VB301S10])
    , (T817H,[VB304S1])
    , (T818H,[VB304S2])
    , (T819H,[VB304S3])
    , (T820H,[VB304S4,VB305S4])
    , (T821H,[VB303S5])
    , (T822H,[VB303S6])
    , (T823H,[VB302S7])
    , (T824H,[VB302S8])
    , (T825H,[VB303S9A,VB303S9B])
    , (T826H,[VB304S5])
    , (T827H,[VB305S6])
    , (T828H,[VB304S7])
    , (T829H,[VB304S8])
    , (T830H,[VB_SR1,VB_SR2,VB_SR3,VB_SR4,VB_SR5,VB_SR6,VB_SR7])
    , (T831H,[VB306S11])
    , (T832H,[VB307S11B])
    , (T833H,[VB_SNR])
    , (T801F,[VBN07D])
    , (T803F,[VBN05DA,VBN05DB])
    , (T805F,[VBN04D,VBN03D])
    , (T807F,[VBN01D])
    , (T809F,[VB000DA])
    , (T811F,[VB000DB,VB002DA])
    , (T813F,[VB002DB])
    , (T815F,[VB003D,VB005D,VB007D,VB008D,VB010D,VB011D,VB013D])
    , (T817F,[VB015D])
    , (T819F,[VB016D])
    , (T821F,[VB017D,VB018D,VB019D,VB021D,VB023DA,VB023DB,VB025DA,VB025DB,VB026DA,VB026DB,VB028D,VB029D])
    , (T823F,[VB031D])
    , (T825F,[VB033DA])
    , (T827F,[VB033DB,VB035D,VB037D,VB039D,VB042D,VB043D,VB045D])
    , (T829F,[VB046D])
    , (T831F,[VB047D])
    , (T833F,[VB048D,VB049D,VB052D,VB054D,VB056D,VB057D,VB059D,VB061D,VB063D,VB065D,VB068D,VB069D,VB071D,VB074D])
    , (T839F,[VB076DA])
    , (T841F,[VB076DB])
    , (T835F,[VB077D])
    , (T837F,[VB079D])
    , (T843F,[VB080DA])
    , (T842F,[VB080UB,VB081U,VB082UA,VB082UB,VB082UC,VB082UD,VB084UA,VB084UB,VB085U,VB088U,VB090U,VB092U,VB093U,VB095U])
    , (T848F,[VB080UA])
    , (T846F,[VB079U])
    , (T840F,[VB077UB])
    , (T838F,[VB077UA])
    , (T844F,[VB076U])
    , (T836F,[VB048U,VB050U,VB052U,VB053U,VB055U,VB056U,VB058U,VB059U,VB061U,VB063U,VB065U,VB066U,VB068U,VB069U,VB072U,VB074U])
    , (T834F,[VB046U])
    , (T832F,[VB045U])
    , (T830F,[VB032U,VB035U,VB037UA,VB037UB,VB039U,VB041U,VB043U])
    , (T828F,[VB031U])
    , (T826F,[VB030U])
    , (T824F,[VB016U,VB018U,VB019U,VB021U,VB023U,VB024UA,VB024UB,VB026UA,VB026UB,VB027U,VB029U])
    , (T822F,[VB014UB])
    , (T820F,[VB014UA])
    , (T818F,[VB003U,VB005U,VB007U,VB008U,VB010U,VB012U])
    , (T816F,[VB002UB])
    , (T814F,[VB000UB,VB002UA])
    , (T812F,[VB000UA])
    , (T810F,[VBN01U])
    , (T808F,[VBN04UB,VBN03U])
    , (T806F,[VBN04UA])
    , (T804F,[VBN05UA,VBN05UB])
    , (T802F,[VBN07U])
    , (T801E,[VB080DB,VB081DA,VB081DB,VB081DC,VB082DA,VB082DB,VB082DC,VB084D,VB086D,VB088D,VB090D,VB092D,VB093D])
    , (T803E,[VB095D])
    , (T805E,[VB096D])
    , (T807E,[VB097D])
    , (T809E,[VB099D])
    , (T811E,[VB100DA])
    , (T813E,[VB100DB,VB104D,VB106D,VB108D,VB109D,VB111D,VB113D,VB115D,VB118D])
    , (T815E,[VB120D])
    , (T817E,[VB121D])
    , (T819E,[VB122D,VB124D,VB126D,VB127D,VB129D,VB131D,VB133D,VB136D,VB138D,VB139D])
    , (T821E,[VB141D])
    , (T823E,[VB142D])
    , (T825E,[VB094MA,VB094MB])
    , (T827E,[VB095M])
    , (T824E,[VB142U,VB144U,VB145U,VB147UA,VB147UB,VB148UA,VB148UB,VB149U,VB150U,VB151U,VB153U,VB155UA])
    , (T822E,[VB141U])
    , (T820E,[VB140U])
    , (T818E,[VB122U,VB124U,VB126U,VB127U,VB129U,VB131U,VB133U,VB136U,VB137U,VB138U])
    , (T816E,[VB120UB])
    , (T814E,[VB120UA])
    , (T812E,[VB102U,VB103U,VB104U,VB106U,VB108U,VB110U,VB112U,VB113U,VB116U,VB118U])
    , (T810E,[VB101U])
    , (T808E,[VB099U])
    , (T806E,[VB097U])
    , (T804E,[VB096UB])
    , (T802E,[VB096UA])
    , (T801D,[VB143D,VB144D,VB147DA,VB147DB,VB148DA,VB148DB,VB148DC,VB149D,VB151D,VB153D,VB155D])
    , (T803D,[VB156D])
    , (T805D,[VB157D])
    , (T807D,[VB158D,VB160D,VB161D,VB163D,VB164D,VB167D])
    , (T809D,[VB168D])
    , (T811D,[VB169DA])
    , (T813D,[VB169DB])
    , (T815D,[VB171DA,VB171DB])
    , (T817D,[VB173DA])
    , (T819D,[VB173DB,VB175DA,VB175DB,VB177D,VB178DA,VB178DB])
    , (T821D,[VB178DC])
    , (T823D,[VB180D])
    , (T825D,[VB181DA,VB181DB,VB183D,VB184DA,VB184DB,VB185DA,VB185DB,VB185DC,VB186D,VB187D])
    , (T827D,[VB189D])
    , (T829D,[VB191DA])
    , (T831D,[VB191DB,VB193D,VB195D,VB198D,VB199D,VB201D,VB203D,VB205D])
    , (T833D,[VB206D])
    , (T835D,[VB207D])
    , (T837D,[VB208D,VB210D,VB212D,VB214D])
    , (T839D,[VB215D])
    , (T841D,[VB217DA])
    , (T842D,[VB217U,VB218U,VB219UA,VB219UB,VB219UC,VB220UA,VB220UB,VB221U])
    , (T840D,[VB215U])
    , (T838D,[VB214U])
    , (T836D,[VB208U,VB210U,VB212U,VB213U])
    , (T834D,[VB206U])
    , (T832D,[VB205U])
    , (T830D,[VB191U,VB193U,VB194U,VB197U,VB199U,VB201U,VB203U])
    , (T828D,[VB189UB])
    , (T826D,[VB189UA])
    , (T824D,[VB180U,VB182U,VB183U,VB184UA,VB184UB,VB185UA,VB185UB,VB186UA,VB186UB,VB187U])
    , (T822D,[VB178UB])
    , (T820D,[VB178UA])
    , (T818D,[VB173UB,VB175U,VB176U])
    , (T816D,[VB173UA])
    , (T814D,[VB171UA,VB171UB])
    , (T812D,[VB169UB])
    , (T810D,[VB169UA])
    , (T808D,[VB168U])
    , (T806D,[VB157U,VB158U,VB160U,VB161U,VB162U,VB164U,VB166U])
    , (T804D,[VB156U])
    , (T802D,[VB155UB])
    , (T801C,[VB217DB,VB219DA,VB219DB,VB219DC,VB220DA,VB220DB,VB221D])
    , (T803C,[VB223D])
    , (T805C,[VB225DA])
    , (T807C,[VB225DB,VB227D,VB229D,VB231D,VB233D,VB235D])
    , (T809C,[VB237D])
    , (T811C,[VB238D])
    , (T813C,[VB239DA,VB239DB])
    , (T843C,[VB241D])
    , (T845C,[VB242D])
    , (T815C,[VB243D])
    , (T817C,[VB245D])
    , (T847C,[VB246DA])
    , (T819C,[VB246DB,VB248D,VB250D,VB251D])
    , (T821C,[VB253D])
    , (T823C,[VB255DA])
    , (T825C,[VB255DB,VB257D,VB259D,VB262D])
    , (T827C,[VB263D])
    , (T829C,[VB264D])
    , (T831C,[VB265D])
    , (T833C,[VB266D])
    , (T835C,[VB267DA])
    , (T837C,[VB267DB,VB269D,VB270D,VB272D,VB275D])
    , (T839C,[VB277D])
    , (T841C,[VB278D])
    , (T842C,[VB279U,VB281U,VB282U,VB283U])
    , (T840C,[VB277U])
    , (T838C,[VB276U])
    , (T836C,[VB268U,VB270U,VB272U,VB274U])
    , (T834C,[VB267UB])
    , (T832C,[VB267UA])
    , (T830C,[VB265U])
    , (T828C,[VB264U])
    , (T826C,[VB263UB])
    , (T824C,[VB255U,VB257U,VB259U,VB261U,VB263UA])
    , (T822C,[VB253UB])
    , (T820C,[VB253UA])
    , (T818C,[VB246UB,VB248U,VB249U,VB251U])
    , (T848C,[VB246UA])
    , (T846C,[VB245U])
    , (T816C,[VB244U])
    , (T814C,[VB242UB])
    , (T844C,[VB242UA])
    , (T812C,[VB239U,VB240U])
    , (T810C,[VB237U])
    , (T808C,[VB236U])
    , (T806C,[VB225U,VB227U,VB229U,VB231U,VB233U,VB235U])
    , (T804C,[VB223UB])
    , (T802C,[VB223UA])
    , (T801B,[VB279D,VB280D,VB282D,VB283D,VB285D])
    , (T803B,[VB286D])
    , (T805B,[VB288DA])
    , (T807B,[VB288DB,VB290D,VB291D,VB294D,VB296D,VB297D,VB299D])
    , (T809B,[VB301D])
    , (T811B,[VB302D])
    , (T813B,[VB304DA])
    , (T815B,[VB304DB])
    , (T817B,[VB306D])
    , (T819B,[VB307D])
    , (T821B,[VB309DA])
    , (T823B,[VB309DB,VB311D,VB313D,VB315D,VB318D,VB320D,VB322D])
    , (T825B,[VB324D])
    , (T827B,[VB326D])
    , (T828B,[VB326U,VB328U,VB329U,VB331U,VB333U,VB335U,VB337U,VB339U])
    , (T826B,[VB325U])
    , (T824B,[VB324U])
    , (T822B,[VB309UB,VB311U,VB313U,VB314U,VB316U,VB318U,VB320U,VB323U])
    , (T820B,[VB309UA])
    , (T818B,[VB307U])
    , (T816B,[VB306U])
    , (T814B,[VB305U])
    , (T812B,[VB303U])
    , (T810B,[VB302UB])
    , (T808B,[VB302UA])
    , (T806B,[VB288U,VB290U,VB293U,VB295U,VB297U,VB298U,VB300U])
    , (T804B,[VB287U])
    , (T802B,[VB286U])
    , (T829B,[VB303L9])
    , (T831B,[VB304L9])
    , (T833B,[VB306L9])
    , (T801A,[VB327D,VB328D,VB331D,VB333D,VB334D,VB336D,VB339D])
    , (T803A,[VB340D])
    , (T805A,[VB342DA])
    , (T807A,[VB342DB,VB344D,VB346D,VB348D,VB350D,VB352D,VB354D,VB356D,VB358D])
    , (T809A,[VB359D])
    , (T811A,[VB360DA])
    , (T813A,[VB360DB])
    , (T815A,[VB362D,VB363DA])
    , (T817A,[VB363DB])
    , (T816A,[VB364U])
    , (T814A,[VB363UA,VB363UB])
    , (T812A,[VB361U])
    , (T810A,[VB360UB])
    , (T808A,[VB360UA])
    , (T806A,[VB342U,VB344U,VB346U,VB348U,VB350U,VB352U,VB355U,VB357U,VB359U])
    , (T804A,[VB341U])
    , (T802A,[VB340U])
    ]

arrBlockTrack :: Array BlockID TrackID
arrBlockTrack = safeArray
    [ (VB000X,T800X)
    , (VBN07D,T801F)
    , (VBN05DA,T803F)
    , (VBN05DB,T803F)
    , (VBN04D,T805F)
    , (VBN03D,T805F)
    , (VBN01D,T807F)
    , (VB000DA,T809F)
    , (VB000DB,T811F)
    , (VB002DA,T811F)
    , (VB002DB,T813F)
    , (VB003D,T815F)
    , (VB005D,T815F)
    , (VB007D,T815F)
    , (VB008D,T815F)
    , (VB010D,T815F)
    , (VB011D,T815F)
    , (VB013D,T815F)
    , (VB015D,T817F)
    , (VB016D,T819F)
    , (VB017D,T821F)
    , (VB018D,T821F)
    , (VB019D,T821F)
    , (VB021D,T821F)
    , (VB023DA,T821F)
    , (VB023DB,T821F)
    , (VB025DA,T821F)
    , (VB025DB,T821F)
    , (VB026DA,T821F)
    , (VB026DB,T821F)
    , (VB028D,T821F)
    , (VB029D,T821F)
    , (VB031D,T823F)
    , (VB033DA,T825F)
    , (VB033DB,T827F)
    , (VB035D,T827F)
    , (VB037D,T827F)
    , (VB039D,T827F)
    , (VB042D,T827F)
    , (VB043D,T827F)
    , (VB045D,T827F)
    , (VB046D,T829F)
    , (VB047D,T831F)
    , (VB048D,T833F)
    , (VB049D,T833F)
    , (VB052D,T833F)
    , (VB054D,T833F)
    , (VB056D,T833F)
    , (VB057D,T833F)
    , (VB059D,T833F)
    , (VB061D,T833F)
    , (VB063D,T833F)
    , (VB065D,T833F)
    , (VB068D,T833F)
    , (VB069D,T833F)
    , (VB071D,T833F)
    , (VB074D,T833F)
    , (VB076DA,T839F)
    , (VB076DB,T841F)
    , (VB077D,T835F)
    , (VB079D,T837F)
    , (VB080DA,T843F)
    , (VB080DB,T801E)
    , (VB081DA,T801E)
    , (VB081DB,T801E)
    , (VB081DC,T801E)
    , (VB082DA,T801E)
    , (VB082DB,T801E)
    , (VB082DC,T801E)
    , (VB084D,T801E)
    , (VB086D,T801E)
    , (VB088D,T801E)
    , (VB090D,T801E)
    , (VB092D,T801E)
    , (VB093D,T801E)
    , (VB095D,T803E)
    , (VB096D,T805E)
    , (VB097D,T807E)
    , (VB099D,T809E)
    , (VB100DA,T811E)
    , (VB100DB,T813E)
    , (VB104D,T813E)
    , (VB106D,T813E)
    , (VB108D,T813E)
    , (VB109D,T813E)
    , (VB111D,T813E)
    , (VB113D,T813E)
    , (VB115D,T813E)
    , (VB118D,T813E)
    , (VB120D,T815E)
    , (VB121D,T817E)
    , (VB122D,T819E)
    , (VB124D,T819E)
    , (VB126D,T819E)
    , (VB127D,T819E)
    , (VB129D,T819E)
    , (VB131D,T819E)
    , (VB133D,T819E)
    , (VB136D,T819E)
    , (VB138D,T819E)
    , (VB139D,T819E)
    , (VB141D,T821E)
    , (VB142D,T823E)
    , (VB143D,T801D)
    , (VB144D,T801D)
    , (VB147DA,T801D)
    , (VB147DB,T801D)
    , (VB148DA,T801D)
    , (VB148DB,T801D)
    , (VB148DC,T801D)
    , (VB149D,T801D)
    , (VB151D,T801D)
    , (VB153D,T801D)
    , (VB155D,T801D)
    , (VB156D,T803D)
    , (VB157D,T805D)
    , (VB158D,T807D)
    , (VB160D,T807D)
    , (VB161D,T807D)
    , (VB163D,T807D)
    , (VB164D,T807D)
    , (VB167D,T807D)
    , (VB168D,T809D)
    , (VB169DA,T811D)
    , (VB169DB,T813D)
    , (VB171DA,T815D)
    , (VB171DB,T815D)
    , (VB173DA,T817D)
    , (VB173DB,T819D)
    , (VB175DA,T819D)
    , (VB175DB,T819D)
    , (VB177D,T819D)
    , (VB178DA,T819D)
    , (VB178DB,T819D)
    , (VB178DC,T821D)
    , (VB180D,T823D)
    , (VB181DA,T825D)
    , (VB181DB,T825D)
    , (VB183D,T825D)
    , (VB184DA,T825D)
    , (VB184DB,T825D)
    , (VB185DA,T825D)
    , (VB185DB,T825D)
    , (VB185DC,T825D)
    , (VB186D,T825D)
    , (VB187D,T825D)
    , (VB189D,T827D)
    , (VB191DA,T829D)
    , (VB191DB,T831D)
    , (VB193D,T831D)
    , (VB195D,T831D)
    , (VB198D,T831D)
    , (VB199D,T831D)
    , (VB201D,T831D)
    , (VB203D,T831D)
    , (VB205D,T831D)
    , (VB206D,T833D)
    , (VB207D,T835D)
    , (VB208D,T837D)
    , (VB210D,T837D)
    , (VB212D,T837D)
    , (VB214D,T837D)
    , (VB215D,T839D)
    , (VB217DA,T841D)
    , (VB217DB,T801C)
    , (VB219DA,T801C)
    , (VB219DB,T801C)
    , (VB219DC,T801C)
    , (VB220DA,T801C)
    , (VB220DB,T801C)
    , (VB221D,T801C)
    , (VB223D,T803C)
    , (VB225DA,T805C)
    , (VB225DB,T807C)
    , (VB227D,T807C)
    , (VB229D,T807C)
    , (VB231D,T807C)
    , (VB233D,T807C)
    , (VB235D,T807C)
    , (VB237D,T809C)
    , (VB238D,T811C)
    , (VB239DA,T813C)
    , (VB239DB,T813C)
    , (VB241D,T843C)
    , (VB242D,T845C)
    , (VB243D,T815C)
    , (VB245D,T817C)
    , (VB246DA,T847C)
    , (VB246DB,T819C)
    , (VB248D,T819C)
    , (VB250D,T819C)
    , (VB251D,T819C)
    , (VB253D,T821C)
    , (VB255DA,T823C)
    , (VB255DB,T825C)
    , (VB257D,T825C)
    , (VB259D,T825C)
    , (VB262D,T825C)
    , (VB263D,T827C)
    , (VB264D,T829C)
    , (VB265D,T831C)
    , (VB266D,T833C)
    , (VB267DA,T835C)
    , (VB267DB,T837C)
    , (VB269D,T837C)
    , (VB270D,T837C)
    , (VB272D,T837C)
    , (VB275D,T837C)
    , (VB277D,T839C)
    , (VB278D,T841C)
    , (VB279D,T801B)
    , (VB280D,T801B)
    , (VB282D,T801B)
    , (VB283D,T801B)
    , (VB285D,T801B)
    , (VB286D,T803B)
    , (VB288DA,T805B)
    , (VB288DB,T807B)
    , (VB290D,T807B)
    , (VB291D,T807B)
    , (VB294D,T807B)
    , (VB296D,T807B)
    , (VB297D,T807B)
    , (VB299D,T807B)
    , (VB301D,T809B)
    , (VB302D,T811B)
    , (VB304DA,T813B)
    , (VB304DB,T815B)
    , (VB306D,T817B)
    , (VB307D,T819B)
    , (VB309DA,T821B)
    , (VB309DB,T823B)
    , (VB311D,T823B)
    , (VB313D,T823B)
    , (VB315D,T823B)
    , (VB318D,T823B)
    , (VB320D,T823B)
    , (VB322D,T823B)
    , (VB324D,T825B)
    , (VB326D,T827B)
    , (VB327D,T801A)
    , (VB328D,T801A)
    , (VB331D,T801A)
    , (VB333D,T801A)
    , (VB334D,T801A)
    , (VB336D,T801A)
    , (VB339D,T801A)
    , (VB340D,T803A)
    , (VB342DA,T805A)
    , (VB342DB,T807A)
    , (VB344D,T807A)
    , (VB346D,T807A)
    , (VB348D,T807A)
    , (VB350D,T807A)
    , (VB352D,T807A)
    , (VB354D,T807A)
    , (VB356D,T807A)
    , (VB358D,T807A)
    , (VB359D,T809A)
    , (VB360DA,T811A)
    , (VB360DB,T813A)
    , (VB362D,T815A)
    , (VB363DA,T815A)
    , (VB363DB,T817A)
    , (VBN07U,T802F)
    , (VBN05UA,T804F)
    , (VBN05UB,T804F)
    , (VBN04UA,T806F)
    , (VBN04UB,T808F)
    , (VBN03U,T808F)
    , (VBN01U,T810F)
    , (VB000UA,T812F)
    , (VB000UB,T814F)
    , (VB002UA,T814F)
    , (VB002UB,T816F)
    , (VB003U,T818F)
    , (VB005U,T818F)
    , (VB007U,T818F)
    , (VB008U,T818F)
    , (VB010U,T818F)
    , (VB012U,T818F)
    , (VB014UA,T820F)
    , (VB014UB,T822F)
    , (VB016U,T824F)
    , (VB018U,T824F)
    , (VB019U,T824F)
    , (VB021U,T824F)
    , (VB023U,T824F)
    , (VB024UA,T824F)
    , (VB024UB,T824F)
    , (VB026UA,T824F)
    , (VB026UB,T824F)
    , (VB027U,T824F)
    , (VB029U,T824F)
    , (VB030U,T826F)
    , (VB031U,T828F)
    , (VB032U,T830F)
    , (VB035U,T830F)
    , (VB037UA,T830F)
    , (VB037UB,T830F)
    , (VB039U,T830F)
    , (VB041U,T830F)
    , (VB043U,T830F)
    , (VB045U,T832F)
    , (VB046U,T834F)
    , (VB048U,T836F)
    , (VB050U,T836F)
    , (VB052U,T836F)
    , (VB053U,T836F)
    , (VB055U,T836F)
    , (VB056U,T836F)
    , (VB058U,T836F)
    , (VB059U,T836F)
    , (VB061U,T836F)
    , (VB063U,T836F)
    , (VB065U,T836F)
    , (VB066U,T836F)
    , (VB068U,T836F)
    , (VB069U,T836F)
    , (VB072U,T836F)
    , (VB074U,T836F)
    , (VB076U,T844F)
    , (VB077UA,T838F)
    , (VB077UB,T840F)
    , (VB079U,T846F)
    , (VB080UA,T848F)
    , (VB080UB,T842F)
    , (VB081U,T842F)
    , (VB082UA,T842F)
    , (VB082UB,T842F)
    , (VB082UC,T842F)
    , (VB082UD,T842F)
    , (VB084UA,T842F)
    , (VB084UB,T842F)
    , (VB085U,T842F)
    , (VB088U,T842F)
    , (VB090U,T842F)
    , (VB092U,T842F)
    , (VB093U,T842F)
    , (VB095U,T842F)
    , (VB096UA,T802E)
    , (VB096UB,T804E)
    , (VB097U,T806E)
    , (VB099U,T808E)
    , (VB101U,T810E)
    , (VB102U,T812E)
    , (VB103U,T812E)
    , (VB104U,T812E)
    , (VB106U,T812E)
    , (VB108U,T812E)
    , (VB110U,T812E)
    , (VB112U,T812E)
    , (VB113U,T812E)
    , (VB116U,T812E)
    , (VB118U,T812E)
    , (VB120UA,T814E)
    , (VB120UB,T816E)
    , (VB122U,T818E)
    , (VB124U,T818E)
    , (VB126U,T818E)
    , (VB127U,T818E)
    , (VB129U,T818E)
    , (VB131U,T818E)
    , (VB133U,T818E)
    , (VB136U,T818E)
    , (VB137U,T818E)
    , (VB138U,T818E)
    , (VB140U,T820E)
    , (VB141U,T822E)
    , (VB142U,T824E)
    , (VB144U,T824E)
    , (VB145U,T824E)
    , (VB147UA,T824E)
    , (VB147UB,T824E)
    , (VB148UA,T824E)
    , (VB148UB,T824E)
    , (VB149U,T824E)
    , (VB150U,T824E)
    , (VB151U,T824E)
    , (VB153U,T824E)
    , (VB155UA,T824E)
    , (VB155UB,T802D)
    , (VB156U,T804D)
    , (VB157U,T806D)
    , (VB158U,T806D)
    , (VB160U,T806D)
    , (VB161U,T806D)
    , (VB162U,T806D)
    , (VB164U,T806D)
    , (VB166U,T806D)
    , (VB168U,T808D)
    , (VB169UA,T810D)
    , (VB169UB,T812D)
    , (VB171UA,T814D)
    , (VB171UB,T814D)
    , (VB173UA,T816D)
    , (VB173UB,T818D)
    , (VB175U,T818D)
    , (VB176U,T818D)
    , (VB178UA,T820D)
    , (VB178UB,T822D)
    , (VB180U,T824D)
    , (VB182U,T824D)
    , (VB183U,T824D)
    , (VB184UA,T824D)
    , (VB184UB,T824D)
    , (VB185UA,T824D)
    , (VB185UB,T824D)
    , (VB186UA,T824D)
    , (VB186UB,T824D)
    , (VB187U,T824D)
    , (VB189UA,T826D)
    , (VB189UB,T828D)
    , (VB191U,T830D)
    , (VB193U,T830D)
    , (VB194U,T830D)
    , (VB197U,T830D)
    , (VB199U,T830D)
    , (VB201U,T830D)
    , (VB203U,T830D)
    , (VB205U,T832D)
    , (VB206U,T834D)
    , (VB208U,T836D)
    , (VB210U,T836D)
    , (VB212U,T836D)
    , (VB213U,T836D)
    , (VB214U,T838D)
    , (VB215U,T840D)
    , (VB217U,T842D)
    , (VB218U,T842D)
    , (VB219UA,T842D)
    , (VB219UB,T842D)
    , (VB219UC,T842D)
    , (VB220UA,T842D)
    , (VB220UB,T842D)
    , (VB221U,T842D)
    , (VB223UA,T802C)
    , (VB223UB,T804C)
    , (VB225U,T806C)
    , (VB227U,T806C)
    , (VB229U,T806C)
    , (VB231U,T806C)
    , (VB233U,T806C)
    , (VB235U,T806C)
    , (VB236U,T808C)
    , (VB237U,T810C)
    , (VB239U,T812C)
    , (VB240U,T812C)
    , (VB242UA,T844C)
    , (VB242UB,T814C)
    , (VB244U,T816C)
    , (VB245U,T846C)
    , (VB246UA,T848C)
    , (VB246UB,T818C)
    , (VB248U,T818C)
    , (VB249U,T818C)
    , (VB251U,T818C)
    , (VB253UA,T820C)
    , (VB253UB,T822C)
    , (VB255U,T824C)
    , (VB257U,T824C)
    , (VB259U,T824C)
    , (VB261U,T824C)
    , (VB263UA,T824C)
    , (VB263UB,T826C)
    , (VB264U,T828C)
    , (VB265U,T830C)
    , (VB267UA,T832C)
    , (VB267UB,T834C)
    , (VB268U,T836C)
    , (VB270U,T836C)
    , (VB272U,T836C)
    , (VB274U,T836C)
    , (VB276U,T838C)
    , (VB277U,T840C)
    , (VB279U,T842C)
    , (VB281U,T842C)
    , (VB282U,T842C)
    , (VB283U,T842C)
    , (VB286U,T802B)
    , (VB287U,T804B)
    , (VB288U,T806B)
    , (VB290U,T806B)
    , (VB293U,T806B)
    , (VB295U,T806B)
    , (VB297U,T806B)
    , (VB298U,T806B)
    , (VB300U,T806B)
    , (VB302UA,T808B)
    , (VB302UB,T810B)
    , (VB303U,T812B)
    , (VB305U,T814B)
    , (VB306U,T816B)
    , (VB307U,T818B)
    , (VB309UA,T820B)
    , (VB309UB,T822B)
    , (VB311U,T822B)
    , (VB313U,T822B)
    , (VB314U,T822B)
    , (VB316U,T822B)
    , (VB318U,T822B)
    , (VB320U,T822B)
    , (VB323U,T822B)
    , (VB324U,T824B)
    , (VB325U,T826B)
    , (VB326U,T828B)
    , (VB328U,T828B)
    , (VB329U,T828B)
    , (VB331U,T828B)
    , (VB333U,T828B)
    , (VB335U,T828B)
    , (VB337U,T828B)
    , (VB339U,T828B)
    , (VB340U,T802A)
    , (VB341U,T804A)
    , (VB342U,T806A)
    , (VB344U,T806A)
    , (VB346U,T806A)
    , (VB348U,T806A)
    , (VB350U,T806A)
    , (VB352U,T806A)
    , (VB355U,T806A)
    , (VB357U,T806A)
    , (VB359U,T806A)
    , (VB360UA,T808A)
    , (VB360UB,T810A)
    , (VB361U,T812A)
    , (VB363UA,T814A)
    , (VB363UB,T814A)
    , (VB364U,T816A)
    , (VB094MA,T825E)
    , (VB094MB,T825E)
    , (VB095M,T827E)
    , (VB303L9,T829B)
    , (VB304L9,T831B)
    , (VB306L9,T833B)
    , (VB318L1,T800X)
    , (VB316L2,T806G)
    , (VB317L2,T806G)
    , (VB318L2,T800X)
    , (VB317L3,T806G)
    , (VB318L3,T800X)
    , (VB317L4,T806G)
    , (VB318L4,T800X)
    , (VB317L5,T806G)
    , (VB318L5,T800X)
    , (VB315L6,T804G)
    , (VB316L6,T806G)
    , (VB318L6,T800X)
    , (VB318L7,T800X)
    , (VB316L8,T807G)
    , (VB317L8,T807G)
    , (VB318L8,T800X)
    , (VB312L9,T801G)
    , (VB314L9,T804G)
    , (VB315L9,T804G)
    , (VB316L9,T808G)
    , (VB318L9,T800X)
    , (VB312L10,T802G)
    , (VB314L10,T805G)
    , (VB315L10,T805G)
    , (VB316L10,T805G)
    , (VB317L10,T809G)
    , (VB318L10,T800X)
    , (VB317L11,T810G)
    , (VB318L11,T800X)
    , (VB316L12A,T805G)
    , (VB316L12B,T811G)
    , (VB318L12,T800X)
    , (VB316L13,T805G)
    , (VB317L13,T812G)
    , (VB318L13,T800X)
    , (VB317L14,T813G)
    , (VB318L14,T800X)
    , (VB316E,T800X)
    , (VB316M,T805G)
    , (VB317M,T814G)
    , (VB318S,T814G)
    , (VB319S,T815G)
    , (VB320S,T800X)
    , (VB313SA,T800X)
    , (VB313SB,T803G)
    , (VB313TA,T800X)
    , (VB313TB,T816G)
    , (VB315T,T817G)
    , (VB316T,T817G)
    , (VB317T,T818G)
    , (VB318TA,T819G)
    , (VB318TB,T820G)
    , (VB320T,T800X)
    , (VB313UL,T800X)
    , (VB314UL,T804G)
    , (VB308L9,T801H)
    , (VB309L9,T801H)
    , (VB310L9A,T801H)
    , (VB310L9B,T803H)
    , (VB309L10A,T802H)
    , (VB309L10B,T802H)
    , (VB310L10A,T802H)
    , (VB310L10B,T804H)
    , (VB302S1,T807H)
    , (VB304S1,T817H)
    , (VB302S2,T808H)
    , (VB304S2,T818H)
    , (VB302S3,T809H)
    , (VB304S3,T819H)
    , (VB302S4A,T810H)
    , (VB302S4B,T810H)
    , (VB304S4,T820H)
    , (VB305S4,T820H)
    , (VB301S5,T811H)
    , (VB303S5,T821H)
    , (VB304S5,T826H)
    , (VB301S6,T812H)
    , (VB302S6,T812H)
    , (VB303S6,T822H)
    , (VB305S6,T827H)
    , (VB300S7,T813H)
    , (VB302S7,T823H)
    , (VB304S7,T828H)
    , (VB306S7,T830H)
    , (VB307S7A,T830H)
    , (VB307S7B,T830H)
    , (VB300S8,T814H)
    , (VB302S8,T824H)
    , (VB304S8,T829H)
    , (VB297S9A,T805H)
    , (VB297S9B,T805H)
    , (VB297S9C,T805H)
    , (VB299S9A,T806H)
    , (VB299S9B,T806H)
    , (VB300S9,T815H)
    , (VB301S9,T815H)
    , (VB303S9A,T825H)
    , (VB303S9B,T825H)
    , (VB304S9,T825H)
    , (VB300S10,T816H)
    , (VB301S10,T816H)
    , (VB306S11,T831H)
    , (VB307S11A,T831H)
    , (VB307S11B,T832H)
    , (VB309S11,T832H)
    , (VB_SL1,T806H)
    , (VB_SL2,T806H)
    , (VB_SL3,T806H)
    , (VB_SL4,T806H)
    , (VB_SL5,T806H)
    , (VB_SL6,T806H)
    , (VB_SL7,T806H)
    , (VB_SR1,T830H)
    , (VB_SR2,T830H)
    , (VB_SR3,T830H)
    , (VB_SR4,T830H)
    , (VB_SR5,T830H)
    , (VB_SR6,T830H)
    , (VB_SR7,T830H)
    , (VB_SNR,T833H)
    ]
