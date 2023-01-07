module SP6.Data.ID.Point where

-- base
import Data.Ix
-- data-default-class
import Data.Default.Class

data PointID -- PのSBLの並び
    -- KIKD OC808
    = P801G
    | P802G
    | P803G
    | P804G
    | P805G
    | P806G
    | P807G
    | P808G
    | P809G
    | P810G
    | P811G
    | P812G
    | P813G
    | P814G
    | P815G
    | P816G
    | P817G
    | P818G
    | P819G
    | P820G
    | P821G
    | P822G
    | P891G
    -- JSTB OC807
    | P801H
    | P802H
    | P803H
    | P804H
    | P805H
    | P806H
    | P807H
    | P808H
    | P809H
    | P810H
    | P811H
    | P812H
    | P813H
    | P814H
    | P815H
    | P816H
    | P817H
    | P818H
    | P819H
    | P820H
    | P821H
    | P822H
    | P823H
    | P824H
    | P825H
    | P826H
    | P827H
    | P828H
    -- JPW OC806
    | P801F
    | P802F
    | P803F
    | P804F
    | P805F
    | P806F
    | P807F
    | P808F
    | P809F
    | P810F
    -- IGDA OC805
    | P801E
    | P802E
    | P803E
    | P804E
    | P805E
    | P806E
    -- RKPM OC804
    | P801D
    | P802D
    | P803D
    | P804D
    -- IWNR OC803
    | P801C
    | P802C
    | P803C
    | P804C
    -- JLA  OC802
    | P801B
    | P802B
    | P803B
    | P804B
    | P805B
    | P806B
    | P807B
    | P809B
    | P810B
    | P811B
    | P812B
    | P813B
    | P815B
    | P816B
    | P817B
    -- BTDG OC801
    | P801A
    | P802A
    | P803A
    | P804A
    | P805A
    | P806A
        deriving (Show, Ord, Eq, Enum, Bounded, Ix)

instance Default PointID where def = P801G
