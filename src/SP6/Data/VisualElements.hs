module SP6.Data.VisualElements where

import SP6.Data.ID

downTracks_5 :: [(TrackID, Double)]
downTracks_5 = [ (T801F, 10) ]

downTracks_4 :: [(TrackID, Double)]
downTracks_4 =
    [ (T805F, 10 )
    , (T807F, 5)
    , (T809F, 10)
    ]

downTracks_3 :: [(TrackID, Double)]
downTracks_3 =
    [ (T813F, 5)
    , (T815F, 25)
    , (T817F, 10)
    , (T819F, 5)
    , (T821F, 20)
    , (T823F, 10)
    , (T825F, 5)
    , (T827F, 20)
    , (T829F, 10)
    , (T831F, 5)
    , (T833F, 35 - trackSepLength)
    , (T839F, 5)
    , (T841F, 5)
    , (T835F, 10)
    ]

downTracks_2 :: [(TrackID, Double)]
downTracks_2 =
    [ (T843F, 5)
    , (T801E, 20)
    , (T803E, 10)
    ]

downTracks_1 :: [(TrackID, Double)]
downTracks_1 =
    [ (T807E, 10) ]

downTracks0 :: [(TrackID, Double)]
downTracks0 =
    [ (T811E, 5)
    , (T813E, 25)
    , (T815E, 10)
    , (T817E, 5)
    , (T819E, 20)
    , (T821E, 10)
    , (T823E, 5)
    , (T801D, 20)
    , (T803D, 10)
    , (T805D, 5)
    , (T807D, 20 - trackSepLength)
    , (T809D, 5)
    , (T811D, 5)
    , (T813D, 10)
    ]

downTracks1 :: [(TrackID, Double)]
downTracks1 =
    [ (T817D, 5)
    , (T819D, 15)
    , (T821D, 10)
    , (T823D, 5)
    , (T825D, 15)
    , (T827D, 10)
    , (T829D, 5)
    , (T831D, 20)
    , (T833D, 10)
    , (T835D, 5)
    , (T837D, 15)
    , (T839D, 10)
    , (T841D, 5)
    , (T801C, 15)
    , (T803C, 10)
    , (T805C, 5)
    , (T807C, 20)
    , (T809C, 10)
    , (T811C, 5)
    , (T813C, 15 - trackSepLength)
    , (T843C, 5)
    ]

downTracks2 :: [(TrackID, Double)]
downTracks2 =
    [ (T815C, 10)
    , (T817C, 5)
    , (T847C, 5)
    , (T819C, 20)
    , (T821C, 10)
    , (T823C, 5)
    , (T825C, 15 - trackSepLength)
    , (T827C, 5)
    ]

downTracks3 :: [(TrackID, Double)]
downTracks3 =
    [ (T831C, 10)
    , (T833C, 5)
    , (T835C, 5)
    , (T837C, 20)
    , (T839C, 10)
    , (T841C, 5)
    , (T801B, 15)
    , (T803B, 10)
    ]

downTracksLeftJLA :: [(TrackID, Double)]
downTracksLeftJLA =
    [ (T805B, 5)
    , (T807B, 20 - trackSepLength)
    , (T809B, 5)
    ]

downTracks4 :: [(TrackID, Double)]
downTracks4 = [ (T815B, 10) ]

downTracksRightJLA :: [(TrackID, Double)]
downTracksRightJLA =
    [ (T821B, 5)
    , (T823B, 25)
    ]

downTracks5 :: [(TrackID, Double)]
downTracks5 =
    [ (T825B, 10)
    , (T827B, 5)
    , (T801A, 20)
    , (T803A, 10)
    , (T805A, 5)
    , (T807A, 20 - trackSepLength)
    , (T809A, 5)
    ]

downTracks6 :: [(TrackID, Double)]
downTracks6 = [ (T813A, 10) ]

downTracks7 :: [(TrackID, Double)]
downTracks7 = [ (T817A, 10) ]

trackSepLength :: Double
trackSepLength = 0.5