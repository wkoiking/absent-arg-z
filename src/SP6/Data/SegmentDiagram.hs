module SP6.Data.SegmentDiagram where

import SP6.Data.ID
import SP6.Data.Common

-- array
import Data.Array.IArray

-- base
import Data.Word (Word16)

data TrackSD = TrackSD {
    sdTrackID :: TrackID,
    sdTrackLength :: Double,
    sdBlocks :: [BlockSD]
} deriving (Show)

data BlockSD = BlockSD {
    sdBlockID :: BlockID,
    sdBlockCode :: Word16,
    sdBlockLength :: Double,
    sdTurnouts :: [TurnoutSD]
} deriving (Show)

data TurnoutSD = TurnoutSD {
    sdPointID :: PointID,
    sdLengthToPoint :: Double,
    sdLengthFromPoint :: Double
} deriving (Show)

asocTrackBlock :: [(TrackID, [BlockID])]
asocTrackBlock = map f trackSDs
 where f trackSD = (sdTrackID trackSD, map sdBlockID $ sdBlocks trackSD)

arrTrackBlock :: Array TrackID [BlockID]
arrTrackBlock = safeArray $ asocTrackBlock

arrBlockTrack :: Array BlockID TrackID
arrBlockTrack = safeArray $ filter ((/= VB000X) . fst) asocBlockTrack

asocBlockTrack :: [(BlockID, TrackID)]
asocBlockTrack = concat
    [ concatMap (\ (t, bs) -> map (, t) bs) asocTrackBlock
    , map ((\ (t, b,  _p, _pos, _vw, _sd) -> (b, t)) . snd) assocSPT
    ]

blockSDs' :: [BlockSD]
blockSDs' = concatMap sdBlocks trackSDs ++ blockSDs ++ blockSPTs

trackSDs :: [TrackSD]
trackSDs =
    -- mainline Ver. T
    [ TrackSD
        { sdTrackID = T801F
        , sdTrackLength = 201.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN07D
                , sdBlockCode = 9400
                , sdBlockLength = 201.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803F
        , sdTrackLength = 109.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN05DA
                , sdBlockCode = 9401
                , sdBlockLength = 52.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801F
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 50.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VBN05DB
                , sdBlockCode = 9421
                , sdBlockLength = 57.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803F
                        , sdLengthToPoint = 56.0
                        , sdLengthFromPoint = 1.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805F
        , sdTrackLength = 332.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN04D
                , sdBlockCode = 9402
                , sdBlockLength = 169.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VBN03D
                , sdBlockCode = 9403
                , sdBlockLength = 163.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN01D
                , sdBlockCode = 9404
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809F
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB000DA
                , sdBlockCode = 9405
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811F
        , sdTrackLength = 217.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB000DB
                , sdBlockCode = 9406
                , sdBlockLength = 126.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P805F
                        , sdLengthToPoint = 34.0
                        , sdLengthFromPoint = 92.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB002DA
                , sdBlockCode = 9422
                , sdBlockLength = 91.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P807F
                        , sdLengthToPoint = 89.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB002DB
                , sdBlockCode = 9407
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815F
        , sdTrackLength = 1159.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB003D
                , sdBlockCode = 9408
                , sdBlockLength = 164.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB005D
                , sdBlockCode = 9423
                , sdBlockLength = 190.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB007D
                , sdBlockCode = 9410
                , sdBlockLength = 174.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB008D
                , sdBlockCode = 9411
                , sdBlockLength = 136.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB010D
                , sdBlockCode = 9412
                , sdBlockLength = 155.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB011D
                , sdBlockCode = 9413
                , sdBlockLength = 182.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB013D
                , sdBlockCode = 9414
                , sdBlockLength = 158.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817F
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB015D
                , sdBlockCode = 9415
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819F
        , sdTrackLength = 54.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB016D
                , sdBlockCode = 9416
                , sdBlockLength = 54.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821F
        , sdTrackLength = 1409.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB017D
                , sdBlockCode = 9417
                , sdBlockLength = 74.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB018D
                , sdBlockCode = 9424
                , sdBlockLength = 130.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB019D
                , sdBlockCode = 9418
                , sdBlockLength = 219.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB021D
                , sdBlockCode = 9419
                , sdBlockLength = 151.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB023DA
                , sdBlockCode = 9420
                , sdBlockLength = 12.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB023DB
                , sdBlockCode = 9425
                , sdBlockLength = 187.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB025DA
                , sdBlockCode = 8400
                , sdBlockLength = 73.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB025DB
                , sdBlockCode = 8439
                , sdBlockLength = 32.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB026DA
                , sdBlockCode = 8440
                , sdBlockLength = 82.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB026DB
                , sdBlockCode = 8401
                , sdBlockLength = 150.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB028D
                , sdBlockCode = 8402
                , sdBlockLength = 107.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB029D
                , sdBlockCode = 8403
                , sdBlockLength = 192.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823F
        , sdTrackLength = 168.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB031D
                , sdBlockCode = 8404
                , sdBlockLength = 168.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T825F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB033DA
                , sdBlockCode = 8405
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827F
        , sdTrackLength = 1268.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB033DB
                , sdBlockCode = 8438
                , sdBlockLength = 194.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB035D
                , sdBlockCode = 8407
                , sdBlockLength = 173.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB037D
                , sdBlockCode = 8408
                , sdBlockLength = 213.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB039D
                , sdBlockCode = 8409
                , sdBlockLength = 305.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB042D
                , sdBlockCode = 8410
                , sdBlockLength = 153.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB043D
                , sdBlockCode = 8411
                , sdBlockLength = 150.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB045D
                , sdBlockCode = 8412
                , sdBlockLength = 80.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T829F
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB046D
                , sdBlockCode = 8413
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T831F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB047D
                , sdBlockCode = 8414
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T833F
        , sdTrackLength = 2802.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB048D
                , sdBlockCode = 8415
                , sdBlockLength = 144.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB049D
                , sdBlockCode = 8416
                , sdBlockLength = 210.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB052D
                , sdBlockCode = 8417
                , sdBlockLength = 221.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB054D
                , sdBlockCode = 8419
                , sdBlockLength = 231.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB056D
                , sdBlockCode = 8420
                , sdBlockLength = 134.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB057D
                , sdBlockCode = 8421
                , sdBlockLength = 130.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB059D
                , sdBlockCode = 8422
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB061D
                , sdBlockCode = 8423
                , sdBlockLength = 232.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB063D
                , sdBlockCode = 8425
                , sdBlockLength = 218.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB065D
                , sdBlockCode = 8426
                , sdBlockLength = 249.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB068D
                , sdBlockCode = 8427
                , sdBlockLength = 150.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB069D
                , sdBlockCode = 8428
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB071D
                , sdBlockCode = 8429
                , sdBlockLength = 236.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB074D
                , sdBlockCode = 8430
                , sdBlockLength = 231.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T839F
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB076DA
                , sdBlockCode = 8432
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T841F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB076DB
                , sdBlockCode = 8433
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T835F
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB077D
                , sdBlockCode = 8434
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T837F
        , sdTrackLength = 119.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB079D
                , sdBlockCode = 8435
                , sdBlockLength = 119.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P809F
                        , sdLengthToPoint = 117.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T843F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB080DA
                , sdBlockCode = 8436
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802F
        , sdTrackLength = 195.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN07U
                , sdBlockCode = 9000
                , sdBlockLength = 195.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804F
        , sdTrackLength = 109.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN05UA
                , sdBlockCode = 9001
                , sdBlockLength = 52.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802F
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 50.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VBN05UB
                , sdBlockCode = 9023
                , sdBlockLength = 57.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804F
                        , sdLengthToPoint = 55.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN04UA
                , sdBlockCode = 9002
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808F
        , sdTrackLength = 294.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN04UB
                , sdBlockCode = 9003
                , sdBlockLength = 73.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VBN03U
                , sdBlockCode = 9004
                , sdBlockLength = 221.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VBN01U
                , sdBlockCode = 9005
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812F
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB000UA
                , sdBlockCode = 9006
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814F
        , sdTrackLength = 212.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB000UB
                , sdBlockCode = 9007
                , sdBlockLength = 119.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806F
                        , sdLengthToPoint = 29.0
                        , sdLengthFromPoint = 90.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB002UA
                , sdBlockCode = 9024
                , sdBlockLength = 93.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P808F
                        , sdLengthToPoint = 91.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB002UB
                , sdBlockCode = 9008
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818F
        , sdTrackLength = 1087.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB003U
                , sdBlockCode = 9009
                , sdBlockLength = 164.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB005U
                , sdBlockCode = 9025
                , sdBlockLength = 190.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB007U
                , sdBlockCode = 9011
                , sdBlockLength = 110.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB008U
                , sdBlockCode = 9012
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB010U
                , sdBlockCode = 9013
                , sdBlockLength = 236.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB012U
                , sdBlockCode = 9014
                , sdBlockLength = 187.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820F
        , sdTrackLength = 51.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB014UA
                , sdBlockCode = 9016
                , sdBlockLength = 51.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822F
        , sdTrackLength = 176.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB014UB
                , sdBlockCode = 9017
                , sdBlockLength = 176.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824F
        , sdTrackLength = 1415.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB016U
                , sdBlockCode = 9018
                , sdBlockLength = 234.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB018U
                , sdBlockCode = 9020
                , sdBlockLength = 102.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB019U
                , sdBlockCode = 9026
                , sdBlockLength = 198.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB021U
                , sdBlockCode = 9021
                , sdBlockLength = 115.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB023U
                , sdBlockCode = 9027
                , sdBlockLength = 107.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB024UA
                , sdBlockCode = 9022
                , sdBlockLength = 79.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB024UB
                , sdBlockCode = 8000
                , sdBlockLength = 105.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB026UA
                , sdBlockCode = 8036
                , sdBlockLength = 83.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB026UB
                , sdBlockCode = 8037
                , sdBlockLength = 39.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB027U
                , sdBlockCode = 8001
                , sdBlockLength = 211.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB029U
                , sdBlockCode = 8002
                , sdBlockLength = 142.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T826F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB030U
                , sdBlockCode = 8003
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T828F
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB031U
                , sdBlockCode = 8004
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T830F
        , sdTrackLength = 1283.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB032U
                , sdBlockCode = 8005
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB035U
                , sdBlockCode = 8006
                , sdBlockLength = 239.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB037UA
                , sdBlockCode = 8029
                , sdBlockLength = 41.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB037UB
                , sdBlockCode = 8007
                , sdBlockLength = 140.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB039U
                , sdBlockCode = 8008
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB041U
                , sdBlockCode = 8009
                , sdBlockLength = 219.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB043U
                , sdBlockCode = 8010
                , sdBlockLength = 201.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T832F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB045U
                , sdBlockCode = 8011
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T834F
        , sdTrackLength = 175.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB046U
                , sdBlockCode = 8012
                , sdBlockLength = 175.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T836F
        , sdTrackLength = 2868.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB048U
                , sdBlockCode = 8013
                , sdBlockLength = 202.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB050U
                , sdBlockCode = 8014
                , sdBlockLength = 239.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB052U
                , sdBlockCode = 8015
                , sdBlockLength = 134.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB053U
                , sdBlockCode = 8016
                , sdBlockLength = 155.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB055U
                , sdBlockCode = 8017
                , sdBlockLength = 117.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB056U
                , sdBlockCode = 8018
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB058U
                , sdBlockCode = 8019
                , sdBlockLength = 130.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB059U
                , sdBlockCode = 8020
                , sdBlockLength = 199.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB061U
                , sdBlockCode = 8021
                , sdBlockLength = 212.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB063U
                , sdBlockCode = 8022
                , sdBlockLength = 147.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB065U
                , sdBlockCode = 8023
                , sdBlockLength = 162.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB066U
                , sdBlockCode = 8024
                , sdBlockLength = 156.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB068U
                , sdBlockCode = 8025
                , sdBlockLength = 149.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB069U
                , sdBlockCode = 8026
                , sdBlockLength = 233.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB072U
                , sdBlockCode = 8027
                , sdBlockLength = 235.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB074U
                , sdBlockCode = 8028
                , sdBlockLength = 231.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T844F
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB076U
                , sdBlockCode = 8030
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T838F
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB077UA
                , sdBlockCode = 8031
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T840F
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB077UB
                , sdBlockCode = 8032
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T846F
        , sdTrackLength = 107.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB079U
                , sdBlockCode = 8033
                , sdBlockLength = 107.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P810F
                        , sdLengthToPoint = 46.0
                        , sdLengthFromPoint = 61.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T848F
        , sdTrackLength = 53.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB080UA
                , sdBlockCode = 8034
                , sdBlockLength = 53.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T842F
        , sdTrackLength = 1543.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB080UB
                , sdBlockCode = 8035
                , sdBlockLength = 44.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB081U
                , sdBlockCode = 8038
                , sdBlockLength = 72.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082UA
                , sdBlockCode = 8039
                , sdBlockLength = 60.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082UB
                , sdBlockCode = 8040
                , sdBlockLength = 9.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082UC
                , sdBlockCode = 7000
                , sdBlockLength = 19.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082UD
                , sdBlockCode = 7041
                , sdBlockLength = 116.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB084UA
                , sdBlockCode = 7042
                , sdBlockLength = 25.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB084UB
                , sdBlockCode = 7001
                , sdBlockLength = 152.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB085U
                , sdBlockCode = 7002
                , sdBlockLength = 250.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB088U
                , sdBlockCode = 7003
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB090U
                , sdBlockCode = 7004
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB092U
                , sdBlockCode = 7005
                , sdBlockLength = 137.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB093U
                , sdBlockCode = 7006
                , sdBlockLength = 203.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB095U
                , sdBlockCode = 7098
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801E
        , sdTrackLength = 1505.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB080DB
                , sdBlockCode = 8437
                , sdBlockLength = 37.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB081DA
                , sdBlockCode = 8441
                , sdBlockLength = 73.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB081DB
                , sdBlockCode = 8442
                , sdBlockLength = 8.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB081DC
                , sdBlockCode = 8443
                , sdBlockLength = 49.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082DA
                , sdBlockCode = 8444
                , sdBlockLength = 12.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082DB
                , sdBlockCode = 7499
                , sdBlockLength = 20.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB082DC
                , sdBlockCode = 7438
                , sdBlockLength = 141.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB084D
                , sdBlockCode = 7401
                , sdBlockLength = 219.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB086D
                , sdBlockCode = 7402
                , sdBlockLength = 220.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB088D
                , sdBlockCode = 7403
                , sdBlockLength = 207.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB090D
                , sdBlockCode = 7404
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB092D
                , sdBlockCode = 7405
                , sdBlockLength = 129.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB093D
                , sdBlockCode = 7406
                , sdBlockLength = 212.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB095D
                , sdBlockCode = 7407
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805E
        , sdTrackLength = 144.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB096D
                , sdBlockCode = 7408
                , sdBlockLength = 144.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801E
                        , sdLengthToPoint = 58.0
                        , sdLengthFromPoint = 86.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807E
        , sdTrackLength = 168.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB097D
                , sdBlockCode = 7409
                , sdBlockLength = 168.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809E
        , sdTrackLength = 90.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB099D
                , sdBlockCode = 7410
                , sdBlockLength = 90.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803E
                        , sdLengthToPoint = 18.0
                        , sdLengthFromPoint = 72.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB100DA
                , sdBlockCode = 7411
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813E
        , sdTrackLength = 1943.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB100DB
                , sdBlockCode = 7412
                , sdBlockLength = 338.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB104D
                , sdBlockCode = 7413
                , sdBlockLength = 230.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB106D
                , sdBlockCode = 7414
                , sdBlockLength = 209.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB108D
                , sdBlockCode = 7415
                , sdBlockLength = 126.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB109D
                , sdBlockCode = 7416
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB111D
                , sdBlockCode = 7417
                , sdBlockLength = 190.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB113D
                , sdBlockCode = 7418
                , sdBlockLength = 218.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB115D
                , sdBlockCode = 7419
                , sdBlockLength = 204.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB118D
                , sdBlockCode = 7420
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815E
        , sdTrackLength = 165.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB120D
                , sdBlockCode = 7421
                , sdBlockLength = 165.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB121D
                , sdBlockCode = 7422
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819E
        , sdTrackLength = 1856.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB122D
                , sdBlockCode = 7423
                , sdBlockLength = 177.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB124D
                , sdBlockCode = 7424
                , sdBlockLength = 181.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB126D
                , sdBlockCode = 7425
                , sdBlockLength = 150.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB127D
                , sdBlockCode = 7426
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB129D
                , sdBlockCode = 7427
                , sdBlockLength = 174.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB131D
                , sdBlockCode = 7428
                , sdBlockLength = 238.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB133D
                , sdBlockCode = 7429
                , sdBlockLength = 235.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB136D
                , sdBlockCode = 7430
                , sdBlockLength = 212.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB138D
                , sdBlockCode = 7431
                , sdBlockLength = 134.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB139D
                , sdBlockCode = 7432
                , sdBlockLength = 150.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821E
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB141D
                , sdBlockCode = 7433
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB142D
                , sdBlockCode = 7434
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB096UA
                , sdBlockCode = 7007
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804E
        , sdTrackLength = 98.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB096UB
                , sdBlockCode = 7008
                , sdBlockLength = 98.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802E
                        , sdLengthToPoint = 57.0
                        , sdLengthFromPoint = 41.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806E
        , sdTrackLength = 172.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB097U
                , sdBlockCode = 7009
                , sdBlockLength = 172.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808E
        , sdTrackLength = 197.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB099U
                , sdBlockCode = 7010
                , sdBlockLength = 197.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804E
                        , sdLengthToPoint = 195.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810E
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB101U
                , sdBlockCode = 7011
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812E
        , sdTrackLength = 1805.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB102U
                , sdBlockCode = 7012
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB103U
                , sdBlockCode = 7099
                , sdBlockLength = 73.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB104U
                , sdBlockCode = 7013
                , sdBlockLength = 210.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB106U
                , sdBlockCode = 7014
                , sdBlockLength = 210.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB108U
                , sdBlockCode = 7015
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB110U
                , sdBlockCode = 7016
                , sdBlockLength = 237.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB112U
                , sdBlockCode = 7017
                , sdBlockLength = 134.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB113U
                , sdBlockCode = 7018
                , sdBlockLength = 209.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB116U
                , sdBlockCode = 7020
                , sdBlockLength = 211.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB118U
                , sdBlockCode = 7021
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB120UA
                , sdBlockCode = 7022
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816E
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB120UB
                , sdBlockCode = 7023
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818E
        , sdTrackLength = 1857.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB122U
                , sdBlockCode = 7024
                , sdBlockLength = 232.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB124U
                , sdBlockCode = 7025
                , sdBlockLength = 175.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB126U
                , sdBlockCode = 7026
                , sdBlockLength = 165.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB127U
                , sdBlockCode = 7027
                , sdBlockLength = 191.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB129U
                , sdBlockCode = 7028
                , sdBlockLength = 173.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB131U
                , sdBlockCode = 7029
                , sdBlockLength = 238.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB133U
                , sdBlockCode = 7030
                , sdBlockLength = 218.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB136U
                , sdBlockCode = 7031
                , sdBlockLength = 159.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB137U
                , sdBlockCode = 7032
                , sdBlockLength = 121.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB138U
                , sdBlockCode = 7033
                , sdBlockLength = 185.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820E
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB140U
                , sdBlockCode = 7034
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822E
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB141U
                , sdBlockCode = 7035
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824E
        , sdTrackLength = 1274.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB142U
                , sdBlockCode = 7036
                , sdBlockLength = 118.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB144U
                , sdBlockCode = 7037
                , sdBlockLength = 98.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB145U
                , sdBlockCode = 7040
                , sdBlockLength = 246.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB147UA
                , sdBlockCode = 7038
                , sdBlockLength = 19.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB147UB
                , sdBlockCode = 7039
                , sdBlockLength = 59.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB148UA
                , sdBlockCode = 6001
                , sdBlockLength = 42.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB148UB
                , sdBlockCode = 6029
                , sdBlockLength = 76.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB149U
                , sdBlockCode = 6002
                , sdBlockLength = 75.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB150U
                , sdBlockCode = 6030
                , sdBlockLength = 108.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB151U
                , sdBlockCode = 6003
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB153U
                , sdBlockCode = 6004
                , sdBlockLength = 192.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB155UA
                , sdBlockCode = 6005
                , sdBlockLength = 36.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801D
        , sdTrackLength = 1302.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB143D
                , sdBlockCode = 7435
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB144D
                , sdBlockCode = 7436
                , sdBlockLength = 225.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB147DA
                , sdBlockCode = 7437
                , sdBlockLength = 51.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB147DB
                , sdBlockCode = 7439
                , sdBlockLength = 105.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB148DA
                , sdBlockCode = 6401
                , sdBlockLength = 10.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB148DB
                , sdBlockCode = 6432
                , sdBlockLength = 13.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB148DC
                , sdBlockCode = 6433
                , sdBlockLength = 75.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB149D
                , sdBlockCode = 6402
                , sdBlockLength = 175.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB151D
                , sdBlockCode = 6403
                , sdBlockLength = 225.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB153D
                , sdBlockCode = 6404
                , sdBlockLength = 131.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB155D
                , sdBlockCode = 6405
                , sdBlockLength = 114.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803D
        , sdTrackLength = 172.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB156D
                , sdBlockCode = 6406
                , sdBlockLength = 172.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB157D
                , sdBlockCode = 6407
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807D
        , sdTrackLength = 1015.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB158D
                , sdBlockCode = 6408
                , sdBlockLength = 198.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB160D
                , sdBlockCode = 6409
                , sdBlockLength = 80.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB161D
                , sdBlockCode = 6410
                , sdBlockLength = 182.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB163D
                , sdBlockCode = 6411
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB164D
                , sdBlockCode = 6412
                , sdBlockLength = 223.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB167D
                , sdBlockCode = 6413
                , sdBlockLength = 154.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809D
        , sdTrackLength = 45.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB168D
                , sdBlockCode = 6414
                , sdBlockLength = 45.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB169DA
                , sdBlockCode = 6415
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813D
        , sdTrackLength = 168.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB169DB
                , sdBlockCode = 6416
                , sdBlockLength = 168.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815D
        , sdTrackLength = 216.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB171DA
                , sdBlockCode = 6417
                , sdBlockLength = 29.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801D
                        , sdLengthToPoint = 29.0
                        , sdLengthFromPoint = 0.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB171DB
                , sdBlockCode = 6418
                , sdBlockLength = 187.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803D
                        , sdLengthToPoint = 181.0
                        , sdLengthFromPoint = 6.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817D
        , sdTrackLength = 47.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB173DA
                , sdBlockCode = 6419
                , sdBlockLength = 47.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819D
        , sdTrackLength = 506.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB173DB
                , sdBlockCode = 6421
                , sdBlockLength = 163.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB175DA
                , sdBlockCode = 6422
                , sdBlockLength = 33.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB175DB
                , sdBlockCode = 6423
                , sdBlockLength = 159.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB177D
                , sdBlockCode = 6424
                , sdBlockLength = 77.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB178DA
                , sdBlockCode = 6425
                , sdBlockLength = 25.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB178DB
                , sdBlockCode = 6426
                , sdBlockLength = 49.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821D
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB178DC
                , sdBlockCode = 6427
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB180D
                , sdBlockCode = 6428
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T825D
        , sdTrackLength = 847.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB181DA
                , sdBlockCode = 6429
                , sdBlockLength = 8.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB181DB
                , sdBlockCode = 6430
                , sdBlockLength = 202.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB183D
                , sdBlockCode = 6431
                , sdBlockLength = 93.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB184DA
                , sdBlockCode = 6434
                , sdBlockLength = 79.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB184DB
                , sdBlockCode = 6435
                , sdBlockLength = 56.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB185DA
                , sdBlockCode = 6436
                , sdBlockLength = 7.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB185DB
                , sdBlockCode = 5400
                , sdBlockLength = 39.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB185DC
                , sdBlockCode = 5422
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB186D
                , sdBlockCode = 5423
                , sdBlockLength = 98.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB187D
                , sdBlockCode = 5401
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827D
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB189D
                , sdBlockCode = 5402
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T829D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB191DA
                , sdBlockCode = 5403
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T831D
        , sdTrackLength = 1445.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB191DB
                , sdBlockCode = 5404
                , sdBlockLength = 221.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB193D
                , sdBlockCode = 5405
                , sdBlockLength = 203.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB195D
                , sdBlockCode = 5406
                , sdBlockLength = 238.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB198D
                , sdBlockCode = 5407
                , sdBlockLength = 88.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB199D
                , sdBlockCode = 5408
                , sdBlockLength = 187.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB201D
                , sdBlockCode = 5409
                , sdBlockLength = 233.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB203D
                , sdBlockCode = 5410
                , sdBlockLength = 221.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB205D
                , sdBlockCode = 5411
                , sdBlockLength = 54.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T833D
        , sdTrackLength = 172.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB206D
                , sdBlockCode = 5412
                , sdBlockLength = 172.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T835D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB207D
                , sdBlockCode = 5413
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T837D
        , sdTrackLength = 689.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB208D
                , sdBlockCode = 5414
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB210D
                , sdBlockCode = 5415
                , sdBlockLength = 242.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB212D
                , sdBlockCode = 5416
                , sdBlockLength = 190.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB214D
                , sdBlockCode = 5417
                , sdBlockLength = 57.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T839D
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB215D
                , sdBlockCode = 5418
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T841D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB217DA
                , sdBlockCode = 5419
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB155UB
                , sdBlockCode = 6006
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804D
        , sdTrackLength = 173.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB156U
                , sdBlockCode = 6007
                , sdBlockLength = 173.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806D
        , sdTrackLength = 1066.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB157U
                , sdBlockCode = 6008
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB158U
                , sdBlockCode = 6009
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB160U
                , sdBlockCode = 6010
                , sdBlockLength = 98.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB161U
                , sdBlockCode = 6011
                , sdBlockLength = 124.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB162U
                , sdBlockCode = 6012
                , sdBlockLength = 236.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB164U
                , sdBlockCode = 6013
                , sdBlockLength = 206.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB166U
                , sdBlockCode = 6014
                , sdBlockLength = 164.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB168U
                , sdBlockCode = 6015
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB169UA
                , sdBlockCode = 6016
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812D
        , sdTrackLength = 176.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB169UB
                , sdBlockCode = 6017
                , sdBlockLength = 176.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814D
        , sdTrackLength = 210.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB171UA
                , sdBlockCode = 6018
                , sdBlockLength = 63.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802D
                        , sdLengthToPoint = 27.0
                        , sdLengthFromPoint = 36.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB171UB
                , sdBlockCode = 6028
                , sdBlockLength = 147.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804D
                        , sdLengthToPoint = 144.0
                        , sdLengthFromPoint = 3.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816D
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB173UA
                , sdBlockCode = 6019
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818D
        , sdTrackLength = 457.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB173UB
                , sdBlockCode = 6020
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB175U
                , sdBlockCode = 6021
                , sdBlockLength = 61.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB176U
                , sdBlockCode = 6022
                , sdBlockLength = 230.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820D
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB178UA
                , sdBlockCode = 6023
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822D
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB178UB
                , sdBlockCode = 6024
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824D
        , sdTrackLength = 850.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB180U
                , sdBlockCode = 6025
                , sdBlockLength = 210.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB182U
                , sdBlockCode = 6026
                , sdBlockLength = 58.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB183U
                , sdBlockCode = 6031
                , sdBlockLength = 86.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB184UA
                , sdBlockCode = 6032
                , sdBlockLength = 71.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB184UB
                , sdBlockCode = 6027
                , sdBlockLength = 60.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB185UA
                , sdBlockCode = 5000
                , sdBlockLength = 6.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB185UB
                , sdBlockCode = 5021
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB186UA
                , sdBlockCode = 5022
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB186UB
                , sdBlockCode = 5023
                , sdBlockLength = 75.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB187U
                , sdBlockCode = 5001
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T826D
        , sdTrackLength = 51.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB189UA
                , sdBlockCode = 5002
                , sdBlockLength = 51.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T828D
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB189UB
                , sdBlockCode = 5003
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T830D
        , sdTrackLength = 1444.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB191U
                , sdBlockCode = 5004
                , sdBlockLength = 191.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB193U
                , sdBlockCode = 5005
                , sdBlockLength = 153.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB194U
                , sdBlockCode = 5006
                , sdBlockLength = 241.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB197U
                , sdBlockCode = 5007
                , sdBlockLength = 203.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB199U
                , sdBlockCode = 5008
                , sdBlockLength = 222.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB201U
                , sdBlockCode = 5009
                , sdBlockLength = 218.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB203U
                , sdBlockCode = 5010
                , sdBlockLength = 216.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T832D
        , sdTrackLength = 51.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB205U
                , sdBlockCode = 5011
                , sdBlockLength = 51.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T834D
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB206U
                , sdBlockCode = 5012
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T836D
        , sdTrackLength = 694.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB208U
                , sdBlockCode = 5013
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB210U
                , sdBlockCode = 5014
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB212U
                , sdBlockCode = 5015
                , sdBlockLength = 199.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB213U
                , sdBlockCode = 5016
                , sdBlockLength = 100.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T838D
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB214U
                , sdBlockCode = 5017
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T840D
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB215U
                , sdBlockCode = 5018
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T842D
        , sdTrackLength = 612.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB217U
                , sdBlockCode = 5019
                , sdBlockLength = 96.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB218U
                , sdBlockCode = 5024
                , sdBlockLength = 120.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219UA
                , sdBlockCode = 5020
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219UB
                , sdBlockCode = 5025
                , sdBlockLength = 2.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219UC
                , sdBlockCode = 5026
                , sdBlockLength = 71.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB220UA
                , sdBlockCode = 4011
                , sdBlockLength = 25.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB220UB
                , sdBlockCode = 4024
                , sdBlockLength = 111.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB221U
                , sdBlockCode = 4012
                , sdBlockLength = 141.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801C
        , sdTrackLength = 601.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB217DB
                , sdBlockCode = 5420
                , sdBlockLength = 161.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219DA
                , sdBlockCode = 5424
                , sdBlockLength = 32.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219DB
                , sdBlockCode = 5421
                , sdBlockLength = 20.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB219DC
                , sdBlockCode = 5425
                , sdBlockLength = 85.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB220DA
                , sdBlockCode = 4411
                , sdBlockLength = 1.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB220DB
                , sdBlockCode = 4423
                , sdBlockLength = 140.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB221D
                , sdBlockCode = 4412
                , sdBlockLength = 162.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB223D
                , sdBlockCode = 4413
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB225DA
                , sdBlockCode = 4414
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807C
        , sdTrackLength = 1134.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB225DB
                , sdBlockCode = 4415
                , sdBlockLength = 179.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB227D
                , sdBlockCode = 4416
                , sdBlockLength = 161.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB229D
                , sdBlockCode = 4417
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB231D
                , sdBlockCode = 4418
                , sdBlockLength = 182.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB233D
                , sdBlockCode = 4419
                , sdBlockLength = 246.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB235D
                , sdBlockCode = 4420
                , sdBlockLength = 151.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809C
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB237D
                , sdBlockCode = 4421
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB238D
                , sdBlockCode = 4422
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813C
        , sdTrackLength = 249.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB239DA
                , sdBlockCode = 4400
                , sdBlockLength = 75.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB239DB
                , sdBlockCode = 4410
                , sdBlockLength = 174.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T843C
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB241D
                , sdBlockCode = 4401
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T845C
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB242D
                , sdBlockCode = 4402
                , sdBlockLength = 180.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803C
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 178.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB243D
                , sdBlockCode = 4403
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB245D
                , sdBlockCode = 4404
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T847C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB246DA
                , sdBlockCode = 4405
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819C
        , sdTrackLength = 679.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB246DB
                , sdBlockCode = 4406
                , sdBlockLength = 146.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB248D
                , sdBlockCode = 4407
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB250D
                , sdBlockCode = 4408
                , sdBlockLength = 145.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB251D
                , sdBlockCode = 3400
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB253D
                , sdBlockCode = 3401
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB255DA
                , sdBlockCode = 3402
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T825C
        , sdTrackLength = 802.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB255DB
                , sdBlockCode = 3403
                , sdBlockLength = 202.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB257D
                , sdBlockCode = 3404
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB259D
                , sdBlockCode = 3405
                , sdBlockLength = 243.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB262D
                , sdBlockCode = 3406
                , sdBlockLength = 157.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB263D
                , sdBlockCode = 3407
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T829C
        , sdTrackLength = 94.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB264D
                , sdBlockCode = 3408
                , sdBlockLength = 94.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801C
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 92.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T831C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB265D
                , sdBlockCode = 3409
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T833C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB266D
                , sdBlockCode = 3410
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T835C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB267DA
                , sdBlockCode = 3411
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T837C
        , sdTrackLength = 932.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB267DB
                , sdBlockCode = 3412
                , sdBlockLength = 142.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB269D
                , sdBlockCode = 3413
                , sdBlockLength = 147.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB270D
                , sdBlockCode = 3414
                , sdBlockLength = 224.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB272D
                , sdBlockCode = 3415
                , sdBlockLength = 230.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB275D
                , sdBlockCode = 3416
                , sdBlockLength = 189.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T839C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB277D
                , sdBlockCode = 3417
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T841C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB278D
                , sdBlockCode = 3418
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB223UA
                , sdBlockCode = 4013
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804C
        , sdTrackLength = 169.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB223UB
                , sdBlockCode = 4014
                , sdBlockLength = 169.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806C
        , sdTrackLength = 1136.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB225U
                , sdBlockCode = 4015
                , sdBlockLength = 229.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB227U
                , sdBlockCode = 4016
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB229U
                , sdBlockCode = 4017
                , sdBlockLength = 193.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB231U
                , sdBlockCode = 4018
                , sdBlockLength = 203.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB233U
                , sdBlockCode = 4019
                , sdBlockLength = 192.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB235U
                , sdBlockCode = 4020
                , sdBlockLength = 153.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB236U
                , sdBlockCode = 4021
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB237U
                , sdBlockCode = 4022
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812C
        , sdTrackLength = 335.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB239U
                , sdBlockCode = 4023
                , sdBlockLength = 108.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB240U
                , sdBlockCode = 4000
                , sdBlockLength = 227.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T844C
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB242UA
                , sdBlockCode = 4001
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814C
        , sdTrackLength = 118.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB242UB
                , sdBlockCode = 4002
                , sdBlockLength = 118.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804C
                        , sdLengthToPoint = 55.0
                        , sdLengthFromPoint = 63.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816C
        , sdTrackLength = 171.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB244U
                , sdBlockCode = 4003
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T846C
        , sdTrackLength = 45.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB245U
                , sdBlockCode = 4004
                , sdBlockLength = 45.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T848C
        , sdTrackLength = 53.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB246UA
                , sdBlockCode = 4005
                , sdBlockLength = 53.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818C
        , sdTrackLength = 659.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB246UB
                , sdBlockCode = 4006
                , sdBlockLength = 172.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB248U
                , sdBlockCode = 4007
                , sdBlockLength = 67.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB249U
                , sdBlockCode = 4008
                , sdBlockLength = 255.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB251U
                , sdBlockCode = 3000
                , sdBlockLength = 165.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB253UA
                , sdBlockCode = 3001
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB253UB
                , sdBlockCode = 3002
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824C
        , sdTrackLength = 846.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB255U
                , sdBlockCode = 3003
                , sdBlockLength = 195.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB257U
                , sdBlockCode = 3004
                , sdBlockLength = 204.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB259U
                , sdBlockCode = 3005
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB261U
                , sdBlockCode = 3006
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB263UA
                , sdBlockCode = 3007
                , sdBlockLength = 62.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T826C
        , sdTrackLength = 52.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB263UB
                , sdBlockCode = 3008
                , sdBlockLength = 52.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T828C
        , sdTrackLength = 95.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB264U
                , sdBlockCode = 3009
                , sdBlockLength = 95.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802C
                        , sdLengthToPoint = 75.0
                        , sdLengthFromPoint = 20.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T830C
        , sdTrackLength = 167.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB265U
                , sdBlockCode = 3010
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T832C
        , sdTrackLength = 51.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB267UA
                , sdBlockCode = 3011
                , sdBlockLength = 51.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T834C
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB267UB
                , sdBlockCode = 3012
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T836C
        , sdTrackLength = 889.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB268U
                , sdBlockCode = 3013
                , sdBlockLength = 236.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB270U
                , sdBlockCode = 3014
                , sdBlockLength = 206.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB272U
                , sdBlockCode = 3015
                , sdBlockLength = 220.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB274U
                , sdBlockCode = 3016
                , sdBlockLength = 227.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T838C
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB276U
                , sdBlockCode = 3017
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T840C
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB277U
                , sdBlockCode = 3018
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T842C
        , sdTrackLength = 745.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB279U
                , sdBlockCode = 3019
                , sdBlockLength = 185.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB281U
                , sdBlockCode = 3020
                , sdBlockLength = 160.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB282U
                , sdBlockCode = 3021
                , sdBlockLength = 135.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB283U
                , sdBlockCode = 3022
                , sdBlockLength = 265.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801B
        , sdTrackLength = 754.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB279D
                , sdBlockCode = 2401
                , sdBlockLength = 136.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB280D
                , sdBlockCode = 2402
                , sdBlockLength = 164.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB282D
                , sdBlockCode = 2403
                , sdBlockLength = 137.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB283D
                , sdBlockCode = 2404
                , sdBlockLength = 159.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB285D
                , sdBlockCode = 2405
                , sdBlockLength = 158.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803B
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB286D
                , sdBlockCode = 2406
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805B
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB288DA
                , sdBlockCode = 2407
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807B
        , sdTrackLength = 1298.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB288DB
                , sdBlockCode = 2408
                , sdBlockLength = 165.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB290D
                , sdBlockCode = 2409
                , sdBlockLength = 118.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB291D
                , sdBlockCode = 2410
                , sdBlockLength = 220.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB294D
                , sdBlockCode = 2411
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB296D
                , sdBlockCode = 2412
                , sdBlockLength = 192.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB297D
                , sdBlockCode = 2413
                , sdBlockLength = 195.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB299D
                , sdBlockCode = 2414
                , sdBlockLength = 203.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809B
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB301D
                , sdBlockCode = 2415
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811B
        , sdTrackLength = 158.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302D
                , sdBlockCode = 2416
                , sdBlockLength = 158.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801B
                        , sdLengthToPoint = 74.0
                        , sdLengthFromPoint = 10.0
                        }
                    , TurnoutSD
                        { sdPointID = P805B
                        , sdLengthToPoint = 10.0
                        , sdLengthFromPoint = 64.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813B
        , sdTrackLength = 83.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304DA
                , sdBlockCode = 2417
                , sdBlockLength = 83.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803B
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 81.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815B
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304DB
                , sdBlockCode = 2418
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817B
        , sdTrackLength = 73.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB306D
                , sdBlockCode = 2419
                , sdBlockLength = 73.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P809B
                        , sdLengthToPoint = 71.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819B
        , sdTrackLength = 186.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB307D
                , sdBlockCode = 2420
                , sdBlockLength = 186.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P815B
                        , sdLengthToPoint = 73.0
                        , sdLengthFromPoint = 5.0
                        }
                    , TurnoutSD
                        { sdPointID = P811B
                        , sdLengthToPoint = 5.0
                        , sdLengthFromPoint = 39 -- 22.5
                        }
                    , TurnoutSD
                        { sdPointID = P817B
                        , sdLengthToPoint = 45 - 39 -- 22.5
                        , sdLengthFromPoint = 58.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821B
        , sdTrackLength = 47.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309DA
                , sdBlockCode = 2421
                , sdBlockLength = 47.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823B
        , sdTrackLength = 1531.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309DB
                , sdBlockCode = 2422
                , sdBlockLength = 231.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB311D
                , sdBlockCode = 2423
                , sdBlockLength = 149.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB313D
                , sdBlockCode = 2424
                , sdBlockLength = 234.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB315D
                , sdBlockCode = 2425
                , sdBlockLength = 243.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB318D
                , sdBlockCode = 2426
                , sdBlockLength = 241.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB320D
                , sdBlockCode = 2427
                , sdBlockLength = 234.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB322D
                , sdBlockCode = 2428
                , sdBlockLength = 199.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T825B
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB324D
                , sdBlockCode = 2429
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827B
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB326D
                , sdBlockCode = 2430
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802B
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB286U
                , sdBlockCode = 2001
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804B
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB287U
                , sdBlockCode = 2002
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806B
        , sdTrackLength = 1342.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB288U
                , sdBlockCode = 2003
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB290U
                , sdBlockCode = 2004
                , sdBlockLength = 224.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB293U
                , sdBlockCode = 2005
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB295U
                , sdBlockCode = 2006
                , sdBlockLength = 234.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB297U
                , sdBlockCode = 2007
                , sdBlockLength = 135.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB298U
                , sdBlockCode = 2008
                , sdBlockLength = 132.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB300U
                , sdBlockCode = 2009
                , sdBlockLength = 197.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808B
        , sdTrackLength = 53.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302UA
                , sdBlockCode = 2010
                , sdBlockLength = 53.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810B
        , sdTrackLength = 114.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302UB
                , sdBlockCode = 2011
                , sdBlockLength = 114.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802B
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 112.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812B
        , sdTrackLength = 125.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB303U
                , sdBlockCode = 2012
                , sdBlockLength = 125.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804B
                        , sdLengthToPoint = 116.0
                        , sdLengthFromPoint = 9.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814B
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB305U
                , sdBlockCode = 2013
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816B
        , sdTrackLength = 76.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB306U
                , sdBlockCode = 2014
                , sdBlockLength = 76.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P810B
                        , sdLengthToPoint = 5.0
                        , sdLengthFromPoint = 71.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818B
        , sdTrackLength = 158.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB307U
                , sdBlockCode = 2015
                , sdBlockLength = 158.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P812B
                        , sdLengthToPoint = 154.0
                        , sdLengthFromPoint = 4.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820B
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309UA
                , sdBlockCode = 2016
                , sdBlockLength = 46.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822B
        , sdTrackLength = 1515.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309UB
                , sdBlockCode = 2017
                , sdBlockLength = 205.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB311U
                , sdBlockCode = 2018
                , sdBlockLength = 188.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB313U
                , sdBlockCode = 2019
                , sdBlockLength = 134.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB314U
                , sdBlockCode = 2020
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB316U
                , sdBlockCode = 2021
                , sdBlockLength = 193.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB318U
                , sdBlockCode = 2022
                , sdBlockLength = 250.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB320U
                , sdBlockCode = 2023
                , sdBlockLength = 234.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB323U
                , sdBlockCode = 2024
                , sdBlockLength = 144.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824B
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB324U
                , sdBlockCode = 2025
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T826B
        , sdTrackLength = 175.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB325U
                , sdBlockCode = 2026
                , sdBlockLength = 175.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T828B
        , sdTrackLength = 1360.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB326U
                , sdBlockCode = 2027
                , sdBlockLength = 152.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB328U
                , sdBlockCode = 2028
                , sdBlockLength = 80.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB329U
                , sdBlockCode = 2029
                , sdBlockLength = 210.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB331U
                , sdBlockCode = 2030
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB333U
                , sdBlockCode = 2031
                , sdBlockLength = 177.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB335U
                , sdBlockCode = 1000
                , sdBlockLength = 214.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB337U
                , sdBlockCode = 1001
                , sdBlockLength = 232.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB339U
                , sdBlockCode = 1002
                , sdBlockLength = 95.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801A
        , sdTrackLength = 1361.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB327D
                , sdBlockCode = 2431
                , sdBlockLength = 185.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB328D
                , sdBlockCode = 2432
                , sdBlockLength = 209.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB331D
                , sdBlockCode = 2433
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB333D
                , sdBlockCode = 2434
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB334D
                , sdBlockCode = 1400
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB336D
                , sdBlockCode = 1401
                , sdBlockLength = 230.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB339D
                , sdBlockCode = 1402
                , sdBlockLength = 144.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803A
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB340D
                , sdBlockCode = 1403
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805A
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB342DA
                , sdBlockCode = 1404
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807A
        , sdTrackLength = 1684.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB342DB
                , sdBlockCode = 1405
                , sdBlockLength = 196.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB344D
                , sdBlockCode = 1406
                , sdBlockLength = 141.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB346D
                , sdBlockCode = 1407
                , sdBlockLength = 209.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB348D
                , sdBlockCode = 1408
                , sdBlockLength = 206.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB350D
                , sdBlockCode = 1409
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB352D
                , sdBlockCode = 1410
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB354D
                , sdBlockCode = 1411
                , sdBlockLength = 215.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB356D
                , sdBlockCode = 1412
                , sdBlockLength = 214.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB358D
                , sdBlockCode = 1413
                , sdBlockLength = 75.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809A
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB359D
                , sdBlockCode = 1414
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811A
        , sdTrackLength = 77.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB360DA
                , sdBlockCode = 1415
                , sdBlockLength = 77.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801A
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 75.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813A
        , sdTrackLength = 168.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB360DB
                , sdBlockCode = 1416
                , sdBlockLength = 168.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815A
        , sdTrackLength = 97.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB362D
                , sdBlockCode = 1417
                , sdBlockLength = 55.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803A
                        , sdLengthToPoint = 20.0
                        , sdLengthFromPoint = 35.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB363DA
                , sdBlockCode = 1419
                , sdBlockLength = 42.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P805A
                        , sdLengthToPoint = 40.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817A
        , sdTrackLength = 226.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB363DB
                , sdBlockCode = 1418
                , sdBlockLength = 226.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T802A
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB340U
                , sdBlockCode = 1003
                , sdBlockLength = 48.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804A
        , sdTrackLength = 170.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB341U
                , sdBlockCode = 1004
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806A
        , sdTrackLength = 1737.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB342U
                , sdBlockCode = 1005
                , sdBlockLength = 211.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB344U
                , sdBlockCode = 1006
                , sdBlockLength = 173.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB346U
                , sdBlockCode = 1007
                , sdBlockLength = 208.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB348U
                , sdBlockCode = 1008
                , sdBlockLength = 207.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB350U
                , sdBlockCode = 1009
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB352U
                , sdBlockCode = 1010
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB355U
                , sdBlockCode = 1011
                , sdBlockLength = 217.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB357U
                , sdBlockCode = 1012
                , sdBlockLength = 200.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB359U
                , sdBlockCode = 1013
                , sdBlockLength = 93.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808A
        , sdTrackLength = 50.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB360UA
                , sdBlockCode = 1014
                , sdBlockLength = 50.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810A
        , sdTrackLength = 76.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB360UB
                , sdBlockCode = 1015
                , sdBlockLength = 76.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802A
                        , sdLengthToPoint = 74.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812A
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB361U
                , sdBlockCode = 1016
                , sdBlockLength = 166.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814A
        , sdTrackLength = 101.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB363UA
                , sdBlockCode = 1017
                , sdBlockLength = 59.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804A
                        , sdLengthToPoint = 24.0
                        , sdLengthFromPoint = 35.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB363UB
                , sdBlockCode = 1019
                , sdBlockLength = 42.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806A
                        , sdLengthToPoint = 40.0
                        , sdLengthFromPoint = 2.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816A
        , sdTrackLength = 228.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB364U
                , sdBlockCode = 1018
                , sdBlockLength = 228.0
                , sdTurnouts = []
                }
            ]
        }
    ] ++
    -- Y Siding
    [ TrackSD
        { sdTrackID = T825E
        , sdTrackLength = 172.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB094MA
                , sdBlockCode = 7800
                , sdBlockLength = 16.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB094MB
                , sdBlockCode = 7899
                , sdBlockLength = 156.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827E
        , sdTrackLength = 106.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB095M
                , sdBlockCode = 7801
                , sdBlockLength = 106.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806E
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 54.0 -- 22.0
                        }
                    , TurnoutSD
                        { sdPointID = P805E
                        , sdLengthToPoint = 0  -- 22.0
                        , sdLengthFromPoint = 50.0
                        }
                    ]
                }
            ]
        }
    ] ++
    -- JLA L9 Ver.Q
    [ TrackSD
        { sdTrackID = T829B
        , sdTrackLength = 168.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB303L9
                , sdBlockCode = 2801
                , sdBlockLength = 168.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806B
                        , sdLengthToPoint = 90.0
                        , sdLengthFromPoint = 75 - 50
                        }
                    , TurnoutSD
                        { sdPointID = P807B
                        , sdLengthToPoint = 50
                        , sdLengthFromPoint = 3.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T831B
        , sdTrackLength = 158.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304L9
                , sdBlockCode = 2802
                , sdBlockLength = 159.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T833B
        , sdTrackLength = 243.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB306L9
                , sdBlockCode = 2803
                , sdBlockLength = 242.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P813B
                        , sdLengthToPoint = 8.0
                        , sdLengthFromPoint = 38.0
                        }
                    , TurnoutSD
                        { sdPointID = P816B
                        , sdLengthToPoint = 38.0
                        , sdLengthFromPoint = 158.0
                        }
                    ]
                }
            ]
        }
    ] ++
    -- Stabling Ver.Q (L9, L10)
    [ TrackSD
        { sdTrackID = T802H
        , sdTrackLength = 109.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309L10B
                , sdBlockCode = 10001
                , sdBlockLength = 82.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P803H
                        , sdLengthToPoint = 22.0
                        , sdLengthFromPoint = 60.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB310L10A
                , sdBlockCode = 10009
                , sdBlockLength = 6.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P805H
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 6.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804H
        , sdTrackLength = 243.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB310L10B
                , sdBlockCode = 10002
                , sdBlockLength = 15 + 51 + 3 + 42 + 44 + 80 + 8 
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801H
        , sdTrackLength = 135.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB309L9
                , sdBlockCode = 10401
                , sdBlockLength = 100.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801H
                        , sdLengthToPoint = 31.0
                        , sdLengthFromPoint = 5
                        }
                    , TurnoutSD
                        { sdPointID = P802H
                        , sdLengthToPoint = 5 -- 6.5 （Ver. AA手動改修）
                        , sdLengthFromPoint = 59 -- 56.0 （Ver. AA手動改修）
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB310L9A
                , sdBlockCode = 10411
                , sdBlockLength = 5.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P804H
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 5.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803H
        , sdTrackLength = 236.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB310L9B
                , sdBlockCode = 10402
                , sdBlockLength = 15 + 51 + 3 + 42 + 44 + 81 -- 155.0 (Ver. AA手動改修）
                , sdTurnouts = []
                }
            ]
        }
    ] ++
    -- Depot Ver.Q
    [ TrackSD
        { sdTrackID = T802G
        , sdTrackLength = 215.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB312L10
                , sdBlockCode = 10004
                , sdBlockLength = 61 + 77 + 64 + 13
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805G
        , sdTrackLength = 222.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB314L10
                , sdBlockCode = 10005
                , sdBlockLength = 81.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P802G
                        , sdLengthToPoint = 51.0
                        , sdLengthFromPoint = 0.0
                        }
                    , TurnoutSD
                        { sdPointID = P804G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 30.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB315L10
                , sdBlockCode = 10010
                , sdBlockLength = 42.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806G
                        , sdLengthToPoint = 31.0
                        , sdLengthFromPoint = 0.0
                        }
                    , TurnoutSD
                        { sdPointID = P815G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 11.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB316L10
                , sdBlockCode = 10006
                , sdBlockLength = 99.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P816G
                        , sdLengthToPoint = 20.0
                        , sdLengthFromPoint = 28 -- 17.5
                        }
                    , TurnoutSD
                        { sdPointID = P817G
                        , sdLengthToPoint = 35 - 28 -- 17.5
                        , sdLengthFromPoint = 44.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809G
        , sdTrackLength = 145.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L10
                , sdBlockCode = 10007
                , sdBlockLength = 105 + 40
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810G
        , sdTrackLength = 150.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L11
                , sdBlockCode = 10050
                , sdBlockLength = 101 + 49
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805G
        , sdTrackLength = 42.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L12A
                , sdBlockCode = 10102
                , sdBlockLength = 42.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P818G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 42.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811G
        , sdTrackLength = 154.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L12B
                , sdBlockCode = 10100
                , sdBlockLength = 123 + 31
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805G
        , sdTrackLength = 46.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L13
                , sdBlockCode = 10152
                , sdBlockLength = 46.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P819G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 46.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812G
        , sdTrackLength = 161.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L13
                , sdBlockCode = 10150
                , sdBlockLength = 101 + 49 + 11
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813G
        , sdTrackLength = 143.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L14
                , sdBlockCode = 10200
                , sdBlockLength = 109 + 36
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806G
        , sdTrackLength = 144.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L2
                , sdBlockCode = 10752
                , sdBlockLength = 39 + 18
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P809G
                        , sdLengthToPoint = 57.0
                        , sdLengthFromPoint = 0.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806G
        , sdTrackLength = 70.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L3
                , sdBlockCode = 10702
                , sdBlockLength = 70.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P812G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 70.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806G
        , sdTrackLength = 106.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317L5
                , sdBlockCode = 10651
                , sdBlockLength = 106.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P811G
                        , sdLengthToPoint = 9.0
                        , sdLengthFromPoint = 97.0
                        }
                    ]
                }
            ]
        }
--     , TrackSD
--         { sdTrackID = T804G
--         , sdTrackLength = 53.0
--         , sdBlocks =
--             [ BlockSD
--                 { sdBlockID = VB316L6A
--                 , sdBlockCode = 10413
--                 , sdBlockLength = 53.0
--                 , sdTurnouts = []
--                 }
--             ]
--         }
    , TrackSD
        { sdTrackID = T806G
        , sdTrackLength = 161.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L6
                , sdBlockCode = 10600
                , sdBlockLength = 161.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P808G
                        , sdLengthToPoint = 10.0
                        , sdLengthFromPoint = 14.0
                        }
                    , TurnoutSD
                        { sdPointID = P810G
                        , sdLengthToPoint = 14.0
                        , sdLengthFromPoint = 123.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T807G
        , sdTrackLength = 196.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L8
                , sdBlockCode = 10501
                , sdBlockLength = 80.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P814G
                        , sdLengthToPoint = 28.0
                        , sdLengthFromPoint = 52.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T801G
        , sdTrackLength = 208.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB312L9
                , sdBlockCode = 10404
                , sdBlockLength = 68 + 66 + 67 + 7
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804G
        , sdTrackLength = 196.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB314L9
                , sdBlockCode = 10405
                , sdBlockLength = 86.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P801G
                        , sdLengthToPoint = 54.0
                        , sdLengthFromPoint = 0.0
                        }
                    , TurnoutSD
                        { sdPointID = P803G
                        , sdLengthToPoint = 0.0
                        , sdLengthFromPoint = 32.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB315L9
                , sdBlockCode = 10410
                , sdBlockLength = 110.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P805G
                        , sdLengthToPoint = 33.0
                        , sdLengthFromPoint = 3.0
                        }
                    , TurnoutSD
                        { sdPointID = P807G
                        , sdLengthToPoint = 3.0
                        , sdLengthFromPoint = 17.0
                        }
                    , TurnoutSD
                        { sdPointID = P813G
                        , sdLengthToPoint = 17.0
                        , sdLengthFromPoint = 37.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808G
        , sdTrackLength = 172.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316L9
                , sdBlockCode = 10407
                , sdBlockLength = 172.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805G
        , sdTrackLength = 155.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB316M
                , sdBlockCode = 10251
                , sdBlockLength = 17 + 154
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P820G
                        , sdLengthToPoint = 17.0
                        , sdLengthFromPoint = 154.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814G
        , sdTrackLength = 52.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB318S
                , sdBlockCode = 10302
                , sdBlockLength = 8 + 38 + 3
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P821G
                        , sdLengthToPoint = 8 + 38
                        , sdLengthFromPoint = 3
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815G
        , sdTrackLength = 164.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB319S
                , sdBlockCode = 10303
                , sdBlockLength = 122 + 35 + 10
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T803G
        , sdTrackLength = 167.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB313SB
                , sdBlockCode = 10350
                , sdBlockLength = 167.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816G
        , sdTrackLength = 182.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB313TB
                , sdBlockCode = 10800
                , sdBlockLength = 24 + 11 + 132 + 11 + 4
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817G
        , sdTrackLength = 257.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB315T
                , sdBlockCode = 10801
                , sdBlockLength = 8 + 139
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB316T
                , sdBlockCode = 10802
                , sdBlockLength = 110
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818G
        , sdTrackLength = 48.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB317T
                , sdBlockCode = 10803
                , sdBlockLength = 11 + 37
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819G
        , sdTrackLength = 45.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB318TA
                , sdBlockCode = 10804
                , sdBlockLength = 45.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P822G
                        , sdLengthToPoint = 10.0
                        , sdLengthFromPoint = 35.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820G
        , sdTrackLength = 174.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB318TB
                , sdBlockCode = 10805
                , sdBlockLength = 7 + 130 + 12 + 21 + 4
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T804G
        , sdTrackLength = 100.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB314UL
                , sdBlockCode = 10412
                , sdBlockLength = 100.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P891G
                        , sdLengthToPoint = 21.0
                        , sdLengthFromPoint = 79.0
                        }
                    ]
                }
            ]
        }
    ] ++
    -- Stabling Ver. AO (SL1, SL10, SL11, SL2, SL3, SL4, SL5, SL6, SL7, SL8, SL9, SN, T806H, T830H)
    [ TrackSD
        { sdTrackID = T807H
        , sdTrackLength = 179.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S1
                , sdBlockCode = 11901
                , sdBlockLength = 179.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T817H
        , sdTrackLength = 179.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S1
                , sdBlockCode = 11902
                , sdBlockLength = 179.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T808H
        , sdTrackLength = 178.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S2
                , sdBlockCode = 11800
                , sdBlockLength = 178.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T818H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S2
                , sdBlockCode = 11801
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T809H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S3
                , sdBlockCode = 11701
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T819H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S3
                , sdBlockCode = 11702
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T810H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S4A
                , sdBlockCode = 11601
                , sdBlockLength = 10.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB302S4B
                , sdBlockCode = 11602
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T820H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S4
                , sdBlockCode = 11603
                , sdBlockLength = 170.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB305S4
                , sdBlockCode = 11604
                , sdBlockLength = 10.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T811H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB301S5
                , sdBlockCode = 11501
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T821H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB303S5
                , sdBlockCode = 11502
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T826H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S5
                , sdBlockCode = 11503
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T812H
        , sdTrackLength = 193.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB301S6
                , sdBlockCode = 11401
                , sdBlockLength = 76.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB302S6
                , sdBlockCode = 11402
                , sdBlockLength = 117.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T822H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB303S6
                , sdBlockCode = 11403
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T827H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB305S6
                , sdBlockCode = 11404
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T813H
        , sdTrackLength = 179.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB300S7
                , sdBlockCode = 11301
                , sdBlockLength = 179.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T823H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S7
                , sdBlockCode = 11302
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T828H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S7
                , sdBlockCode = 11303
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T830H
        , sdTrackLength = -30610.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB306S7
                , sdBlockCode = 11304
                , sdBlockLength = 95.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P822H
                        , sdLengthToPoint = 66.0
                        , sdLengthFromPoint = 29.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB307S7B
                , sdBlockCode = 11310
                , sdBlockLength = 22 + 17
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P824H
                        , sdLengthToPoint = 22
                        , sdLengthFromPoint = 17
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T814H
        , sdTrackLength = 180.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB300S8
                , sdBlockCode = 11201
                , sdBlockLength = 180.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T824H
        , sdTrackLength = 179.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB302S8
                , sdBlockCode = 11202
                , sdBlockLength = 179.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T829H
        , sdTrackLength = 227.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB304S8
                , sdBlockCode = 11203
                , sdBlockLength = 227.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P817H
                        , sdLengthToPoint = 83.0
                        , sdLengthFromPoint = 144.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T805H
        , sdTrackLength = 166.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB297S9A
                , sdBlockCode = 11100
                , sdBlockLength = 8.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB297S9B
                , sdBlockCode = 11101
                , sdBlockLength = 5.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB297S9C
                , sdBlockCode = 11102
                , sdBlockLength = 153.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806H
        , sdTrackLength = 167.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB299S9A
                , sdBlockCode = 11103
                , sdBlockLength = 16.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P806H
                        , sdLengthToPoint = 2.0
                        , sdLengthFromPoint = 14.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB299S9B
                , sdBlockCode = 11104
                , sdBlockLength = 151.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P808H
                        , sdLengthToPoint = 14.0
                        , sdLengthFromPoint = 137.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T815H
        , sdTrackLength = 225.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB300S9
                , sdBlockCode = 11105
                , sdBlockLength = 54.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB301S9
                , sdBlockCode = 11106
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T825H
        , sdTrackLength = 121.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB303S9A
                , sdBlockCode = 11107
                , sdBlockLength = 64.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P815H
                        , sdLengthToPoint = 46.0
                        , sdLengthFromPoint = 18.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB303S9B
                , sdBlockCode = 11108
                , sdBlockLength = 57.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P816H
                        , sdLengthToPoint = 8.0
                        , sdLengthFromPoint = 49.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T816H
        , sdTrackLength = 225.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB300S10
                , sdBlockCode = 11000
                , sdBlockLength = 54.0
                , sdTurnouts = []
                }
            , BlockSD
                { sdBlockID = VB301S10
                , sdBlockCode = 11001
                , sdBlockLength = 171.0
                , sdTurnouts = []
                }
            ]
        }
    , TrackSD
        { sdTrackID = T831H
        , sdTrackLength = 130.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB306S11
                , sdBlockCode = 11505
                , sdBlockLength = 85.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P827H
                        , sdLengthToPoint = 19.0
                        , sdLengthFromPoint = 66.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T832H
        , sdTrackLength = 131.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB307S11B
                , sdBlockCode = 11506
                , sdBlockLength = 127.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P828H
                        , sdLengthToPoint = 116.0
                        , sdLengthFromPoint = 11.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T806H
        , sdTrackLength = 281.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB_SL1
                , sdBlockCode = 11200
                , sdBlockLength = 36.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P807H
                        , sdLengthToPoint = 18.0
                        , sdLengthFromPoint = 18.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL2
                , sdBlockCode = 11300
                , sdBlockLength = 31.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P809H
                        , sdLengthToPoint = 19.0
                        , sdLengthFromPoint = 12.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL3
                , sdBlockCode = 11400
                , sdBlockLength = 41.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P810H
                        , sdLengthToPoint = 23.0
                        , sdLengthFromPoint = 18.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL4
                , sdBlockCode = 11500
                , sdBlockLength = 46.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P811H
                        , sdLengthToPoint = 20.0
                        , sdLengthFromPoint = 26.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL5
                , sdBlockCode = 11600
                , sdBlockLength = 13.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P812H
                        , sdLengthToPoint = 8.0
                        , sdLengthFromPoint = 5.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL6
                , sdBlockCode = 11700
                , sdBlockLength = 50.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P813H
                        , sdLengthToPoint = 33.0
                        , sdLengthFromPoint = 17.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SL7
                , sdBlockCode = 11900
                , sdBlockLength = 64.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P814H
                        , sdLengthToPoint = 17.0
                        , sdLengthFromPoint = 47.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T830H
        , sdTrackLength = 278.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB_SR1
                , sdBlockCode = 11903
                , sdBlockLength = 71.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P818H
                        , sdLengthToPoint = 46.0
                        , sdLengthFromPoint = 25.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR2
                , sdBlockCode = 11703
                , sdBlockLength = 33.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P819H
                        , sdLengthToPoint = 9.0
                        , sdLengthFromPoint = 24.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR3
                , sdBlockCode = 11605
                , sdBlockLength = 42.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P820H
                        , sdLengthToPoint = 13.0
                        , sdLengthFromPoint = 29.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR4
                , sdBlockCode = 11504
                , sdBlockLength = 36.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P821H
                        , sdLengthToPoint = 5.0
                        , sdLengthFromPoint = 31.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR5
                , sdBlockCode = 11405
                , sdBlockLength = 25.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P823H
                        , sdLengthToPoint = 5.0
                        , sdLengthFromPoint = 20.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR6
                , sdBlockCode = 11307
                , sdBlockLength = 21.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P825H
                        , sdLengthToPoint = 20.0
                        , sdLengthFromPoint = 1.0
                        }
                    ]
                }
            , BlockSD
                { sdBlockID = VB_SR7
                , sdBlockCode = 11308
                , sdBlockLength = 50.0
                , sdTurnouts =
                    [ TurnoutSD
                        { sdPointID = P826H
                        , sdLengthToPoint = 1.0
                        , sdLengthFromPoint = 49.0
                        }
                    ]
                }
            ]
        }
    , TrackSD
        { sdTrackID = T833H
        , sdTrackLength = 176.0
        , sdBlocks =
            [ BlockSD
                { sdBlockID = VB_SNR
                , sdBlockCode = 11305
                , sdBlockLength = 176.0
                , sdTurnouts = []
                }
            ]
        }
    ]

blockSDs :: [BlockSD]
blockSDs =
    -- Ver.Q
    [ BlockSD
        { sdBlockID = VB318L1
        , sdBlockCode = 10751
        , sdBlockLength = 186.0
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L10
        , sdBlockCode = 10008
        , sdBlockLength = 9 + 17 + 101
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L11
        , sdBlockCode = 10051
        , sdBlockLength = 16 + 111
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L12
        , sdBlockCode = 10101
        , sdBlockLength = 2 + 16 + 109
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L13
        , sdBlockCode = 10151
        , sdBlockLength = 9 + 118
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L14
        , sdBlockCode = 10201
        , sdBlockLength = 13 + 14 + 100
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L2
        , sdBlockCode = 10750
        , sdBlockLength = 26 + 101
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L3
        , sdBlockCode = 10700
        , sdBlockLength = 26 + 101
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L4
        , sdBlockCode = 10701
        , sdBlockLength = 17 + 110
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L5
        , sdBlockCode = 10650
        , sdBlockLength = 25 + 102
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L6
        , sdBlockCode = 10601
        , sdBlockLength = 27 + 100
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L7
        , sdBlockCode = 10550
        , sdBlockLength = 20 + 107
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L8
        , sdBlockCode = 10500
        , sdBlockLength = 15 + 112
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB318L9
        , sdBlockCode = 10408
        , sdBlockLength = 100.0
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB316E
        , sdBlockCode = 10300
        , sdBlockLength = 76.0
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB320S
        , sdBlockCode = 10304
        , sdBlockLength = 10.0
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB313SA
        , sdBlockCode = 10349
        , sdBlockLength = 10.0
        , sdTurnouts = []
        }
    , BlockSD -- Test TrackのEnd of Track外のブロック
        { sdBlockID = VB313TA
        , sdBlockCode = 10806
        , sdBlockLength = 10.0
        , sdTurnouts = []
        }
    , BlockSD -- Test TrackのEnd of Track外のブロック
        { sdBlockID = VB320T
        , sdBlockCode = 10807
        , sdBlockLength = 53.0
        , sdTurnouts = []
        }
    , BlockSD
        { sdBlockID = VB313UL
        , sdBlockCode = 10409
        , sdBlockLength = 140.0
        , sdTurnouts = []
        }
    ]

blockSPTs :: [BlockSD]
blockSPTs = map ((\ (_, _, _, _, _, a) -> a) . snd) assocSPT

assocSPT :: [(SPT_ID, (TrackID, BlockID, PointID, (Bool, Bool), Double, BlockSD))]
assocSPT =
    [ ( SPT1,
        (T806G, VB317L2, P809G, (True, False), 3
            , BlockSD
                { sdBlockID = VB317L2
                , sdBlockCode = 10753
                , sdBlockLength = 87.0
                , sdTurnouts = []
                }
        ))
    , ( SPT2,
        ( T806G, VB317L4, P812G, (False, True), 3
            , BlockSD
                { sdBlockID = VB317L4
                , sdBlockCode = 10703
                , sdBlockLength = 67.0
                , sdTurnouts = []
                }
        ))
    , ( SPT3,
        ( T807G, VB317L8, P814G, (True, False), 20
            , BlockSD
                { sdBlockID = VB317L8
                , sdBlockCode = 10502
                , sdBlockLength = 116.0
                , sdTurnouts = []
                }
        ))
    , ( SPT4,
        ( T804G, VB315L6, P807G, (False, True), 3
            , BlockSD
                { sdBlockID = VB315L6
                , sdBlockCode = 10413
                , sdBlockLength = 53.0
                , sdTurnouts = []
                }
        ))
    , ( SPT5,
        ( T801H, VB308L9, P801H, (True, False), 3
            , BlockSD
                { sdBlockID = VB308L9
                , sdBlockCode = 2806
                , sdBlockLength = 30.0
                , sdTurnouts = []
                }
        ))
    , ( SPT6,
        ( T802H, VB309L10A, P803H, (True, True), 3
            , BlockSD
                { sdBlockID = VB309L10A
                , sdBlockCode = 2435
                , sdBlockLength = 21.0
                , sdTurnouts = []
                }
        ))
    , ( SPT7,
        ( T814G, VB317M, P821G, (False, True), 3
            , BlockSD
                { sdBlockID = VB317M
                , sdBlockCode = 10250
                , sdBlockLength = 57 + 16 + 15
                , sdTurnouts = []
                }
        ))
    , ( SPT8,
        ( T825H, VB304S9, P816H, (False, True), 3
            , BlockSD
                { sdBlockID = VB304S9
                , sdBlockCode = 2804
                , sdBlockLength = 16.0
                , sdTurnouts = []
                }
        ))
    -- new
    , ( SPT9,
        (T831H, VB307S11A, P827H, (True, False), 3
            , BlockSD
                { sdBlockID = VB307S11A
                , sdBlockCode = 11507
                , sdBlockLength = 45
                , sdTurnouts = []
                }
        ))
    , ( SPT10,
        (T832H, VB309S11, P828H, (True, True), 1
            , BlockSD
                { sdBlockID = VB309S11
                , sdBlockCode = 10414
                , sdBlockLength = 4
                , sdTurnouts = []
                }
        ))
    , ( SPT11,
        (T830H, VB307S7A, P824H, (True, False), 3
            , BlockSD
                { sdBlockID = VB307S7A
                , sdBlockCode = 11399
                , sdBlockLength = 32 + 22
                , sdTurnouts = []
                }
        ))
    ]
