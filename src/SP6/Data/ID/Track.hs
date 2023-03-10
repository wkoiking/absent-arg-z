module SP6.Data.ID.Track where

-- base
import Data.Ix
-- data-default-class
import Data.Default.Class

data TrackID
    = T800X
    | T801G
    | T802G
    | T803G
    | T804G
    | T805G
    | T806G
    | T807G
    | T808G
    | T809G
    | T810G
    | T811G
    | T812G
    | T813G
    | T814G
    | T815G
    | T816G
    | T817G
    | T818G
    | T819G
    | T820G
    | T801H
    | T802H
    | T803H
    | T804H
    | T805H
    | T806H
    | T807H
    | T808H
    | T809H
    | T810H
    | T811H
    | T812H
    | T813H
    | T814H
    | T815H
    | T816H
    | T817H
    | T818H
    | T819H
    | T820H
    | T821H
    | T822H
    | T823H
    | T824H
    | T825H
    | T826H
    | T827H
    | T828H
    | T829H
    | T830H
    | T831H
    | T832H
    | T833H
    | T801F
    | T803F
    | T805F
    | T807F
    | T809F
    | T811F
    | T813F
    | T815F
    | T817F
    | T819F
    | T821F
    | T823F
    | T825F
    | T827F
    | T829F
    | T831F
    | T833F
    | T839F
    | T841F
    | T835F
    | T837F
    | T843F
    | T842F
    | T848F
    | T846F
    | T840F
    | T838F
    | T844F
    | T836F
    | T834F
    | T832F
    | T830F
    | T828F
    | T826F
    | T824F
    | T822F
    | T820F
    | T818F
    | T816F
    | T814F
    | T812F
    | T810F
    | T808F
    | T806F
    | T804F
    | T802F
    | T801E
    | T803E
    | T805E
    | T807E
    | T809E
    | T811E
    | T813E
    | T815E
    | T817E
    | T819E
    | T821E
    | T823E
    | T825E
    | T827E
    | T824E
    | T822E
    | T820E
    | T818E
    | T816E
    | T814E
    | T812E
    | T810E
    | T808E
    | T806E
    | T804E
    | T802E
    | T801D
    | T803D
    | T805D
    | T807D
    | T809D
    | T811D
    | T813D
    | T815D
    | T817D
    | T819D
    | T821D
    | T823D
    | T825D
    | T827D
    | T829D
    | T831D
    | T833D
    | T835D
    | T837D
    | T839D
    | T841D
    | T842D
    | T840D
    | T838D
    | T836D
    | T834D
    | T832D
    | T830D
    | T828D
    | T826D
    | T824D
    | T822D
    | T820D
    | T818D
    | T816D
    | T814D
    | T812D
    | T810D
    | T808D
    | T806D
    | T804D
    | T802D
    | T801C
    | T803C
    | T805C
    | T807C
    | T809C
    | T811C
    | T813C
    | T843C
    | T845C
    | T815C
    | T817C
    | T847C
    | T819C
    | T821C
    | T823C
    | T825C
    | T827C
    | T829C
    | T831C
    | T833C
    | T835C
    | T837C
    | T839C
    | T841C
    | T842C
    | T840C
    | T838C
    | T836C
    | T834C
    | T832C
    | T830C
    | T828C
    | T826C
    | T824C
    | T822C
    | T820C
    | T818C
    | T848C
    | T846C
    | T816C
    | T814C
    | T844C
    | T812C
    | T810C
    | T808C
    | T806C
    | T804C
    | T802C
    | T801B
    | T803B
    | T805B
    | T807B
    | T809B
    | T811B
    | T813B
    | T815B
    | T817B
    | T819B
    | T821B
    | T823B
    | T825B
    | T827B
    | T828B
    | T826B
    | T824B
    | T822B
    | T820B
    | T818B
    | T816B
    | T814B
    | T812B
    | T810B
    | T808B
    | T806B
    | T804B
    | T802B
    | T829B
    | T831B
    | T833B
    | T801A
    | T803A
    | T805A
    | T807A
    | T809A
    | T811A
    | T813A
    | T815A
    | T817A
    | T816A
    | T814A
    | T812A
    | T810A
    | T808A
    | T806A
    | T804A
    | T802A
        deriving (Show, Ord, Eq, Enum, Bounded, Ix)

instance Default TrackID where def = T800X
