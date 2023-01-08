module SP6.Data.Layout where

-- hascats
import SP6.Data.ID (TrackID, BlockID, VentilationSectionID(..))
import SP6.Data.Common (safeArray2)
import SP6.Data.SegmentDiagram (arrTrackBlock)
import SP6.Data.VisualElements 
    ( downTracks_5
    , downTracks_4
    , downTracks_3
    , downTracks_2
    , downTracks_1
    , downTracks0
    , downTracks1
    , downTracks2
    , downTracks3
    , downTracks5
    , downTracks6
    , downTracks7
    )

-- array
import Data.Array.IArray
-- base
import Data.Maybe (fromMaybe)

test_renderLayout :: IO ()
test_renderLayout = mapM_ (print . fst) downTrackLayout

downTrackLayout :: [(TrackID, VentilationSectionID)]
downTrackLayout  = concat
    [ mapTrack downTracks_5
    , mapTrack downTracks_4
    , mapTrack downTracks_3
    , mapTrack downTracks_2
    , mapTrack downTracks_1
    , mapTrack downTracks0
    , mapTrack downTracks1
    , mapTrack downTracks2
    , mapTrack downTracks3
    , mapTrack downTracks5
    , mapTrack downTracks6
    , mapTrack downTracks7
    ]

mapTrack :: [(TrackID, Double)] -> [(TrackID, VentilationSectionID)]
mapTrack = map $ track . fst

track :: TrackID -> (TrackID, VentilationSectionID)
track trackID = (trackID, fromMaybe VE001 $ arrTrackToVentilationSection ! trackID)

arrTrackToVentilationSection :: Array TrackID (Maybe VentilationSectionID)
arrTrackToVentilationSection = safeArray2 $ zip  downlineTracks [VE001 ..]

downlineTracks :: [TrackID]
downlineTracks = map fst downTrackLayout

-- not used

downlineBlocks :: [BlockID]
downlineBlocks = concatMap (arrTrackBlock !) downlineTracks

