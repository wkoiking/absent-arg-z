module SP6.Data.Layout where

-- hascats
import SP6.Data.ID
import SP6.Data.Common
import SP6.Data.SegmentDiagram
import SP6.Data.VisualElements

-- array
import Data.Array.IArray
-- base
import Data.Maybe (fromMaybe)
import Data.List (sortBy,  nub)
import Data.Function (on)

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

asocVentilationSectionToBlocks :: [(VentilationSectionID, [BlockID])]
asocVentilationSectionToBlocks = sortBy (compare `on` fst) $ zip [VE001 ..] $ map return downlineBlocks

asocTrackToVESection :: [(TrackID, VentilationSectionID)]
asocTrackToVESection = concatMap toAsoc asocVentilationSectionToBlocks
 where toAsoc :: (VentilationSectionID, [BlockID]) -> [(TrackID, VentilationSectionID)]
       toAsoc (sectionID, blocks) = map (, sectionID) tracks
        where tracks = nub $ map (arrBlockTrack !) blocks
