COMMENT ON SCHEMA "aim_lotic" IS 'This dataset was created to monitor the status, condition and trend of national BLM resources in accordance with BLM policies. The methodology used for the collection of these data can be found in TR 1735-2 (AIM National Aquatic Monitoring Framework: Field Protocol for Wadeable Lotic System). https://www.blm.gov/documents/national-office/blm-library/technical-reference/aim-national-aquatic-monitoring-0';

-- aim_lotic.F_Bank_B
COMMENT ON TABLE "aim_lotic"."F_Bank_B" IS 'Bank stability and cover data and bank angle data for boatable reaches. 11 main transects for left and right banks are sampled for bank stability and cover but data is estimated from across the river for one bank per transect. Bank angle is only collected on one bank per transect and ocularly estimated.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."Transect" IS '11 main are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects. Intermediate transects are not collected using the boatable protocol. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."Bank" IS 'Left or right bank of the stream as one looks downstream';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."BankAngleCategory" IS 'Bank Angle - categorical EPA method; Flat(<5 °), gradual (5-29°), steep (30-75°), vertical/near vertical/undercut (<75° or undercut)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."BankType" IS 'Erosional or depositional bank as defined by the field protocol';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."CoveredBankMIM" IS 'Percent of 42 banks with greater than 50% foliar cover provided by perennial vegetation, wood or mineral substrate > 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."CoveredBankOld" IS 'Percent of 42 banks with greater than 50% basal cover provided by perennial vegetation, wood or mineral substrate > 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."ErosionalFeature" IS 'Dominant type of erosional features present, if any (Eroding, Slough, Slump, Fracture, Absent)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."DataCollectionBank" IS 'Side of the river data was collected from as you look downstream. Boatable protocol alternates data collection between right and left banks every two transects.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."BankAngleFlag" IS 'Indicates if there was any difficulty collecting bank angle and if measurements were estimated. Measurements may be estimated if depth rods can''t be placed at the correct angle due to vegetation on the bank.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_B"."BankStabilityCoverFlag" IS 'Indicates for each specific bank and transect if there was any difficulty collecting bank stability or cover or if stability, cover, or scour line was estimated';

-- aim_lotic.F_Bank_W
COMMENT ON TABLE "aim_lotic"."F_Bank_W" IS 'Bank stability and cover data and bank angle data for wadeable reaches. Bank stability and cover is collected at 21 main and intermediate transects for left and right banks while bank angle is a contingent indicator and only collected at left and right banks at main transects. Both bank stability and cover and bank angle may be collected on side channels when present. Bank angle is measured with a clinometer following Acher et al. 2015 methods. Acute angles are overhanging banks and obtuse are laid back banks.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."Bank" IS 'Left or right bank of the stream as one looks downstream';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankType" IS 'Erosional or depositional bank as defined by the field protocol';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."CoveredBankMIM" IS 'Percent of 42 banks with greater than 50% foliar cover provided by perennial vegetation, wood or mineral substrate greater than 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."CoveredBankOld" IS 'Percent of 42 banks with greater than 50% basal cover provided by perennial vegetation, wood or mineral substrate greater than 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankBedrockCover" IS 'Percent of bank stability plot covered with bedrock';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankCobbleCover" IS 'Percent of bank stability plot covered with cobble greater than 15 cm';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankLargeWoodCover" IS 'Percent of bank stability plot covered with large wood';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankVegCoverFoliar" IS 'Percent of bank stability plot covered with perennial vegetation as assessed by foliar cover';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankVegCoverBasal" IS 'Percent of bank stability plot covered with perennial vegetation as assessed by basal cover';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."ErosionalFeature" IS 'Dominant type of erosional features present, if any (Eroding, Slough, Slump, Fracture, Absent)';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankAngle" IS 'Bank angle in degrees - PIBO method; acute angles are overhanging banks and obtuse are laid back banks';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankAngleType" IS 'Obtuse or acute angle; used in app to determine if the input value should be subtracted from 180; acute angles are overhanging banks and obtuse are laid back banks';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankAngleFlag" IS 'Indicates if there was any difficulty collecting bank angle and if measurements were estimated. Measurements may be estimated if depth rods can''t be placed at the correct angle due to vegetation on the bank.';
COMMENT ON COLUMN "aim_lotic"."F_Bank_W"."BankStabilityCoverFlag" IS 'Indicates for each specific bank and transect if there was any difficulty collecting bank stability or cover or if stability, cover, or scour line was estimated';

-- aim_lotic.F_CanopyCover_B
COMMENT ON TABLE "aim_lotic"."F_CanopyCover_B" IS 'Canopy cover for boatable reaches as assessed by counting the number of covered intersections of a convex densiometer. Four measurements are taken on one bank at 11 main transects.';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."Transect" IS '11 main are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects. Intermediate transects are not collected using the boatable protocol. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."CanopyDown" IS 'Number of covered intersections of the densiometer when in the littoral plot looking downstream';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."CanopyUp" IS 'Number of covered intersections of the densiometer when in the littoral plot looking upstream';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."CanopyLeft" IS 'Number of covered intersections of the densiometer when in the littoral plot looking towards the left bank';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_B"."CanopyRight" IS 'Number of covered intersections of the densiometer when in the littoral plot looking towards the right bank';

-- aim_lotic.F_CanopyCover_W
COMMENT ON TABLE "aim_lotic"."F_CanopyCover_W" IS 'Canopy cover for wadeable reaches as assessed by counting the number of covered intersections of a convex densiometer. Four measurements are taken midstream channel and one measurement is taken at each bank at 11 main transects and side channels when present.';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyCenterDown" IS 'Number of covered intersections of the densiometer when in the center of the stream looking downstream';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyCenterLeft" IS 'Number of covered intersections of the densiometer when in the center of the stream looking towards the left bank';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyCenterRight" IS 'Number of covered intersections of the densiometer when in the center of the stream looking towards the right bank';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyCenterUp" IS 'Number of covered intersections of the densiometer when in the center of the stream looking upstream';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyLeft" IS 'Number of covered intersections of the densiometer when in the littoral plot looking towards the left bank';
COMMENT ON COLUMN "aim_lotic"."F_CanopyCover_W"."CanopyRight" IS 'Number of covered intersections of the densiometer when in the littoral plot looking towards the right bank';

-- aim_lotic.F_ChannelDimensions_B
COMMENT ON TABLE "aim_lotic"."F_ChannelDimensions_B" IS 'Channel widths and heights for boatable reaches taken with stick and tape at 11 main transects';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."Transect" IS '11 main are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects. Intermediate transects are not collected using the boatable protocol. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."WettedWidth" IS 'Wetted width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BarWidth" IS 'Bar width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BankfullWidth" IS 'Bankfull width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BankfullHeight" IS 'Bankfull height (m) measured from water surface unless transect dry';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BenchHeight" IS 'Bench height measured from water surface unless transect dry';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."TransectValleyConstraint" IS 'Visual estimate of whether the channel is constrained within a narrow valley, constrained by local features within a broad valley, unconstrained and free to move about within a broad floodplain, or free to move about, but within a relatively narrow valley floor';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."SeeOverBank" IS 'Whether you could or could not readily see over the bank at each transect from the boat';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."DistanceToRiparianVeg" IS 'Estimate of the distance (m) from the shore to the edge of the riparian vegetation plot';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."WettedWidthFlag" IS 'Wetted width may be estimated if dense vegetation or other obstructions prevent being able to reach the bank with a tape';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BarWidthFlag" IS 'Indicates if there was any difficulty collecting bar width and if it was estimated.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BenchHeightFlag" IS 'Indicates if there was any difficulty collecting bench height and if it was estimated. At transects where there are not clear bankfull or bench geomorphic surface indicators, crews can use upstream or downstream transects to inform an estimate.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_B"."BankfullWidthFlag" IS 'Indicates if there was difficulty collecting bankfull width. Crews may estimate widths if obstructions prevent being able to reach the bank with a tape.';

-- aim_lotic.F_ChannelDimensions_W
COMMENT ON TABLE "aim_lotic"."F_ChannelDimensions_W" IS 'Channel widths and heights for wadeable reaches taken with stick and tape at 21 main and intermediate transects and side channels when present.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."WettedWidth" IS 'Wetted width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BarWidth" IS 'Bar width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."TransectStatus" IS 'Was the transect dry, wet or not collected';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BankfullWidth" IS 'Bankfull width (m)';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BankfullHeight" IS 'Bankfull height (m) measured from water surface unless transect dry';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BenchHeight" IS 'Bench height measured from water surface unless transect dry';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."SideChannelType" IS 'Major minor, or dry side channel based on amount of flow';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."SideChannelLocation" IS 'Which side of the main channel the side channel was on as you are looking downstream';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BankfullWidthFlag" IS 'Indicates if there was difficulty collecting bankfull width. Crews may estimate widths if obstructions prevent being able to reach the bank with a tape.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BankfullHeightFlag" IS 'Indicates if there was difficulty collecting bankfull height and if it was estimated. If there are not clear bankfull geomorphic surface indicators, crews may use upstream and downstream transects to inform an estimate.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."WettedWidthFlag" IS 'Wetted width may be estimated if dense vegetation or other obstructions prevent being able to reach the bank with a tape';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BarWidthFlag" IS 'Indicates if there was any difficulty collecting bar width and if it was estimated.';
COMMENT ON COLUMN "aim_lotic"."F_ChannelDimensions_W"."BenchHeightFlag" IS 'Indicates if there was any difficulty collecting bench height and if it was estimated. At transects where there are not clear bankfull or bench geomorphic surface indicators, crews can use upstream or downstream transects to inform an estimate.';

-- aim_lotic.F_FishCover_W_B
COMMENT ON TABLE "aim_lotic"."F_FishCover_W_B" IS 'Ocular estimates of instream of concealment features (e.g., LWD, veg., undercuts, boulders) at 11 plots. Plots for wadeable extend 5 m upstream and 5 m downstream of the transect and across the entire wetted width. Separate plots for side channels may also be assessed. Plots for boatable are littoral and extend 10 m upstream and 10 m downstream of the transect and 10 m out into the river towards the thalweg from the wetted edge.';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc. Boating reaches do not collect intermediate transects.';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."FilamentousAlgaeCover" IS 'Cover class for filamentous algae  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."BoulderCover" IS 'Cover class for boulders (basketball to car-sized particles)  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."SmallWoodyDebrisCover" IS 'Cover class for Brush/small woody debris  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."LiveTreeCover" IS 'Cover class for in-channel live trees or roots  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."MacrophyteCover" IS 'Cover class for macrophytes (water loving plants including mosses) (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."OverhangingVegCover" IS 'Cover class for Overhanging Veg (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."ArtificialStructureCover" IS 'Cover class for artificial structures  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."UndercutBankCover" IS 'Cover class for undercut banks > 5 cm deep (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_FishCover_W_B"."LargeWoodyDebrisCover" IS 'Cover class for large woody debris  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';

-- aim_lotic.F_FloodproneWidth_W_B
COMMENT ON TABLE "aim_lotic"."F_FloodproneWidth_W_B" IS 'Two tape measurements of the floodplain valley width at riffles closest to the bottom and top of reach at wadeable reaches or at transects B and K at boatable reaches. Hand levels are used to ensure measurements are taken at a height of 2*bankfull depth.';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."Riffle" IS 'A sequential number indicating riffle that floodprone width was collected at. Riffle one is the downstream most riffle in the reach and riffle two is the riffle closest to the upstream most riffle.';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneBankfullHeight" IS 'Bankfull height (m) taken from the water surface at the location where floodprone width will be measured';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneMaxWaterDepth" IS 'Max water depth (thalweg depth) (m) at the location where floodprone width was taken';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneHeight" IS 'The height (m) at which floodprone width was taken (maxdepth+2 times bankfull height)';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneBankfullWidth" IS 'Bankfull width (m) at the location where floodprone with was measured';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneWidth" IS 'Width of the floodplain (m) at 2 times bankfull depth (bankfull height + max water depth at the transect)';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneLatitude" IS 'Latitude of the locations where floodprone width was collected in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_FloodproneWidth_W_B"."FloodproneLongitude" IS 'Longitude of the locations where floodprone width was collected in decimal degrees and WGS84 datum';

-- aim_lotic.F_HumanInfluence_W_B
COMMENT ON TABLE "aim_lotic"."F_HumanInfluence_W_B" IS 'Ocular est. of human activities on left and right banks at each of 11 transects including on side channels. Activities are recorded by their proximity to the stream (on the bank or instream, within the 10 X 10 m (wadeable) or 10 X 20 m (boatable) riparian vegetation plot, and present outside of the riparian vegetation plot)';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc. Boating reaches do not collect intermediate transects.';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Bank" IS 'Left or right bank of the stream as one looks downstream';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Buildings" IS 'Human influence - buildings, included powelines in 2020. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."LoggingOperations" IS 'Human influence - logging operations. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Mining" IS 'Human influence - mining activity (including gravel mining). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."ParksLawns" IS 'Human influence - parks or maintained lawns. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."PavementClearedLot" IS 'Human influence - pavement/cleared lots (e.g. paved, graveled, dirt parking lot or foundation). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Pipes" IS 'Human influence - inlet or outlet pipes. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."RoadRailroadCulvert" IS 'Human influence - roads or railroads, including culverts. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."RowCrops" IS 'Human influence - row crops. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."LandfillTrash" IS 'Human influence - landfills or trash (e.g., cans, bottles, trash heaps). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."WallDikeRipRap" IS 'Human influence - wall, dikes, or bank stabilization structures such as riprap. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."PastureHayFence" IS 'Human influence - pasture, hay fields, fences. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."LivestockHorseBurro" IS 'Human influence - presence of livestock or wild horses and burros, including feces. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."HydrologicAlterations" IS 'Human influence - hydrologic alteration (irrigation diversions, impoundments, dams). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Recreation" IS 'Human influence - recreation. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."InstreamRestoration" IS 'Human influence - instream habitat restoration (e.g. gabion rock baskets, cabled large wood, beaver dam analog structures). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."PowerlinePipeline" IS 'Human influence - utility line, powerline, pipeline corridor. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."OilGas" IS 'Human influence - oil and gas wells and associated well pads. Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';
COMMENT ON COLUMN "aim_lotic"."F_HumanInfluence_W_B"."Fire" IS 'Human influence - wildfire (natural or human caused). Available options: Absent, Present Outside Plot, Contained In Plot, Within Streambed';

-- aim_lotic.F_LargeWood_W_B
COMMENT ON TABLE "aim_lotic"."F_LargeWood_W_B" IS 'Large wood tallied by diameter and length size classes. Qualifying large wood sizes differ between wadeable and boatable. Wadeable assesses >0.1m diameter for at least 1.5 m in length over the entire reach. While boatable assess >0.3m diameter for at least 5 m in length in 11 littoral plots. Littoral plots extend 10 m upstream and 10 m downstream of the transect and 10 m out into the river towards the thalweg from the wetted edge.';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc. Boating reaches do not collect intermediate transects.';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."WoodLocation" IS 'Whether the large wood was bridging above bankfull channel or entirely or partially below the bankfull elevation';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."LargeDiamLargeLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.6-0.8 m diameter, >15 m length; Boatable - 0.8-1.0 m diameter, >30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."LargeDiamMediumLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.6-0.8 m diameter, 5-15 m length; Boatable - 0.8-1.0 m diameter, 15-30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."LargeDiamCombinedSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.6-0.8 m diameter, 1.5-5 m length; Boatable - 0.8-1.0 m diameter, 5-15 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."LargeDiamSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.6-0.8 m diameter, 3-5 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."LargeDiamXSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.6-0.8 m diameter, 1.5-3 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."MediumDiamLargeLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.3-0.6 m diameter, >15 m length; Boatable - 0.6-0.8 m diameter, >30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."MediumDiamMediumLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.3-0.6 m diameter, 5-15 m length; Boatable - 0.6-0.8 m diameter, 15-30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."MediumDiamCombinedSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.3-0.6 m diameter, 1.5-5 m length; Boatable - 0.6-0.8 m diameter, 5-15 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."MediumDiamSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.3-0.6 m diameter, 3-5 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."MediumDiamXSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.3-0.6 m diameter, 1.5-3 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."SmallDiamLargeLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.1-0.3 m diameter, >15 m length; Boatable - 0.3-0.6 m diameter, >30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."SmallDiamMediumLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.1-0.3 m diameter, 5-15 m length; Boatable - 0.3-0.6 m diameter, 15-30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."SmallDiamCombinedSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.1-0.3 m diameter, 1.5-5 m length; Boatable - 0.3-0.6 m diameter, 5-15 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."SmallDiamSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.1-0.3 m diameter, 3-5 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."SmallDiamXSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - 0.1-0.3 m diameter, 1.5-3 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."XLargeDiamLargeLen" IS 'Number of pieces of wood in the following size class: Wadeable - >0.8 m diameter, >15 m length; Boatable - >1.0 m diameter, >30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."XLargeDiamMediumLen" IS 'Number of pieces of wood in the following size class: Wadeable - >0.8 m diameter, 5-15 m length; Boatable - >1.0 m diameter, 15-30 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."XLargeDiamCombinedSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - >0.8 m diameter, 1.5-5 m length; Boatable - >1.0 m diameter, 5-15 m length';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."XLargeDiamSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - >0.8 m diameter, 3-5 m length;';
COMMENT ON COLUMN "aim_lotic"."F_LargeWood_W_B"."XLargeDiamXSmallLen" IS 'Number of pieces of wood in the following size class: Wadeable - >0.8 m diameter, 1.5-3 m length;';

-- aim_lotic.F_Littoral_B
COMMENT ON TABLE "aim_lotic"."F_Littoral_B" IS 'Streambed particle size classes and water depth of littoral plots at 11 main transects for boatable reaches. The littoral plot that extends 10 m upstream and downstream from the transects and 10 m out from the wetted edge. Littoral Streambed particle and water depths are taken at the 5 haphazard locations within the plot. Dominant and subdominant substrate size classes are also ocularly assessed along a shoreline swath 20 meters long and 1 meter back from the wetted edge.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."Transect" IS '11 main are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects. Intermediate transects are not collected using the boatable protocol. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."DominantLittoralSubstrate" IS 'The dominant substrate size class within the littoral plot that extends 10 m upstream and downstream from the transects and 10 m out from the wetted edge. Estimated from 5 haphazard locations within the plot.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."SecondaryLittoralSubstrate" IS 'The secondary substrate size class within the littoral plot that extends 10 m upstream and downstream from the transects and 10 m out from the wetted edge. Estimated from 5 haphazard locations within the plot.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."DominantShoreSubstrate" IS 'The dominant substrate size class present along a shoreline swath 20 meters long and 1 meter back from the waterline. Visual estimate from standing within the littoral plot.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."SecondaryShoreSubstrate" IS 'The secondary substrate size class present along a shoreline swath 20 meters long and 1 meter back from the waterline. Visually estimate from standing within the littoral plot.';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."LittoralDepth1" IS 'First water depth taken haphazardly in the littoral plot';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."LittoralDepth2" IS 'Second water depth taken haphazardly in the littoral plot';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."LittoralDepth3" IS 'Third water depth taken haphazardly in the littoral plot';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."LittoralDepth4" IS 'Fourth water depth taken haphazardly in the littoral plot';
COMMENT ON COLUMN "aim_lotic"."F_Littoral_B"."LittoralDepth5" IS 'Fifth water depth taken haphazardly in the littoral plot';

-- aim_lotic.F_NotSampledReaches_W_B
COMMENT ON TABLE "aim_lotic"."F_NotSampledReaches_W_B" IS 'Layer used to track the outcome of all rejected reaches. Information includes reason why it was not sampled, any notes from the crew and information about who approved the rejections.';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date. If the reach was field evaluated, FieldEvalDate is used for the EvaluationID. If the reach was rejected in the office, OfficeEvalDate is used for the EvaluationID';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."PointID" IS 'Unique location identifier; 2-3 letter prefix indicating the original stratum, followed by the stream order group, and 4-5 digit random code specifying the order the points were selected';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."EvaluationLatitude" IS 'Latitude of the rejected reach in decimal degrees and WGS84 datum. If a reach was rejected in the field, the crews coordinates are used. If the reach was rejected in the office, the design coordinates were used.';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."EvaluationLongitude" IS 'Longitude of the rejected reach in decimal degrees and WGS84 datum. If a reach was rejected in the field, the crews coordinates are used. If the reach was rejected in the office, the design coordinates were used.';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."OfficeEvalDate" IS 'Date the point or reach was office evaluated to determine if it could be sampled or not';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."FieldEvalDate" IS 'Date the point or reach was field evaluated to determine if it could be sampled or not';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."EvalStatus" IS 'All NotSampled reaches have Final Eval Statuses of Not Sampled';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."FieldEvalStatus" IS 'All NotSampled reaches have a FieldEvalStatus of Not Sampled';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."ReasonNotSampled" IS 'The reason why a particular reach was not sampled.';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."AdminState" IS 'BLM administrative state';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."EvaluationID_OLD" IS 'Unique visit identifier from NAMC. Generated at time of data collection by the iPad.';
COMMENT ON COLUMN "aim_lotic"."F_NotSampledReaches_W_B"."geom" IS 'Point feature geometry (EPSG:4269 NAD83).';


-- aim_lotic.F_PoolTailFines_W
COMMENT ON TABLE "aim_lotic"."F_PoolTailFines_W" IS 'Number of intersections with a 36 X 36 cm grid (50 possible intersections) that have fine sediment';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."PoolNum" IS 'A sequential number of a given pool in the reach';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."GridNum" IS 'Sequential number indicating which pool tail fine grid was collected. Three grids per pool are collected.';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."PoolTailFinesLessThan2mm" IS 'Number of intersections with a 14 X 14 inch grid (49 possible intersections) that have fine sediment <2 mm for a pool tail';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."PoolTailFinesLessThan6mm" IS 'Number of intersections with a 14 X 14 inch grid (49 possible intersections) that have fine sediment <6mm for a pool tail';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."PoolTailOrgMatterBoulder" IS 'Number of intersections with a 14 X 14 inch grid (49 possible intersections) that cannot be measured due to organic matter or something else obscuring the substrate for a pool tail';
COMMENT ON COLUMN "aim_lotic"."F_PoolTailFines_W"."PoolTail6To512mm" IS 'Number of intersections in the pool tail fine grid that are not fines (> 6mm) and not large wood, organic matter or boulders (<512 mm)';

-- aim_lotic.F_Pools_W
COMMENT ON TABLE "aim_lotic"."F_Pools_W" IS 'Depth and length of each qualifying pool for wadeable reaches';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolNum" IS 'A sequential number of a given pool in the reach';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolType" IS 'Pool type; full (>90% of wetted width) or partial (50-90% of wetted width)';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolLength" IS 'Pool length (along the thalweg) of each pool (m)';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolTailDepth" IS 'Pool tail depth (m). A pool tail is the shallowest downstream location in the pool from which water would spill if the flow ere reduced to a trickle.';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolMaxDepth" IS 'Pool maximum depth (m)';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolLocation" IS 'List of transects that were within the pool';
COMMENT ON COLUMN "aim_lotic"."F_Pools_W"."PoolFlag" IS 'Pool depths or widths may be estimated if pools are too deep or unsafe to sample';

-- aim_lotic.F_SampledReaches_W_B
COMMENT ON TABLE "aim_lotic"."F_SampledReaches_W_B" IS 'Wadeable and boatable reach information such as PointID, date visited, individuals involved in data collection, protocol used, and bottom and top of reach latitude and longitude. Additional reach characteristics such as beaver modifications or waterwithdrawls are included. Much of this information is included in the I_Indicator table but this table has some additional information.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."PointID" IS 'Unique location identifier; 2-3 letter prefix indicating the original stratum, followed by the stream order group, and 4-5 digit random code specifying the order the points were selected';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."FieldEvalDate" IS 'Date the point or reach was evaluated to determine if it could be sampled or not';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ProtocolType" IS 'Protocol implemented (wadeable vs. boatable).';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ProtocolVersion" IS 'Pdf archive of the protocol used for data collection';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."FieldStatus" IS 'This is a subset of FieldEvalStatus categories and is whether the reach was partially or fully sampled or had interrupted flow.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."PointCoordinatesMoved" IS 'Where the mid reach point coordinates moved up or downstream from the original design coordinate to get more sampleable transects?';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."DistanceFromDesignCoord" IS 'Calculated straight-line distance (m) between the original GRTS design coordinate and the F transect coordinate; distance between sampled targeted points and original targeted point locations may not be calculated or may be misleading and reflect changes in targeted points to be sampled';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."SampledMidLatitude" IS 'Latitude of the F transect of the sampled reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."SampledMidLongitude" IS 'The longitude of the F transect of the sampled reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AccuracyMidReachCoord" IS 'Accuracy of F transect coordinates (m)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ElevationMidReachCoord" IS 'Elevation (m) at Transect F';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."TopReachLatitude" IS 'Latitude of top of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."TopReachLongitude" IS 'Longitude of top of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AccuracyTopReachCoord" IS 'Accuracy of the top of the reach coordinates (m)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ElevationTopReachCoord" IS 'Elevation (m) at the top of the reach. For fully sampled reaches, this will correspond to Transect K.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."BottomReachLatitude" IS 'Latitude of bottom of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."BottomReachLongitude" IS 'Longitude of bottom of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AccuracyBottomReachCoord" IS 'Accuracy of the bottom of the reach coordinates (m)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ElevationBottomReachCoord" IS 'Elevation (m) at the bottom of the reach. For fully sampled reaches, this will correspond to Transect A.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."MonumentLatitude" IS 'Latitude of the feature that monuments the F transect in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."MonumentLongitude" IS 'Longitude of the feature that monuments the F transect in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AccuracyMonumentCoord" IS 'Accuracy of the coordinate of the monument feature for the F transect (m)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ProtocolReachLength" IS 'Total reach length intended to be sampled. Calculated in the app from average widths using the protocol rules';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AvgTypicalBankfullWidths" IS 'Average of 5 typical bankfull widths. This measurement is used to define reach length. Reach length is 20 times this measurement but has a minimum of 150 meters and a maximum of 4000 meters.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AvgTypicalWettedWidths" IS 'Average of 5 typical wetted widths. This measurement is used to define reach length. Reach length is 40 times this measurement but has a minimum of 150 meters and a maximum of 4000 meters. Wadeable protocol was changed from this method to use AvgTypicalBankfullWidths instead but boatable still uses this method.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."ThalwegSpacing" IS 'Distance between each thalweg depth measurement (m)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."NumThalwegsPerTransect" IS 'Number of thalweg stations supposed to be collected between each main transect; number has varied among protocols but was initially based on avg typical widths with smaller streams having more measurements than larger streams';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."BeaverFlowMod" IS 'Flow influence by beaver across the reach. Options of Major, Minor or None';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."BeaverSigns" IS 'Qualitative visual assessment of frequency of beaver signs (e.g. chewed logs) within the reach (Absent, Rare, Common)';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."WaterWithdrawals" IS 'Presence or absence of water withdrawals';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."WeatherConditions" IS 'Weather conditions while sampling the reach';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."Project" IS 'The BLM administrative unit associated with the design and points. WRSA points are associated with district or field office level projects if these points are reused at those specific levels in a design.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."DataCollectionOrganization" IS 'Organization that the field crew leader personnel belongs to; If BLM project leads accompanied a crew at a site, the crew''s organization was used rather than the project lead.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."QC_Visit" IS 'Reaches resampled within 2-3 weeks by an independent crew to quantify protocol repeatability';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."EvaluationID_OLD" IS 'Unique visit identifier from NAMC. Generated at time of data collection by the iPad.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."AdminState" IS 'BLM administrative state';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."SampledCOMID" IS 'NHD catchments identifier (COMID) used to track whether points were moved from their original DesignCOMID.';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."OfficeEvalDate" IS 'Date the point or reach was office evaluated to determine if it could be sampled or not';
COMMENT ON COLUMN "aim_lotic"."F_SampledReaches_W_B"."geom" IS 'Point feature geometry (EPSG:4269 NAD83).';

-- aim_lotic.F_SlopePoolSummary_W
COMMENT ON TABLE "aim_lotic"."F_SlopePoolSummary_W" IS 'The length of the wadeable reach sampled for slope and pools along with summary statistics for slope and pools. Slope statistics include elevation change summed across all shots and averaged across passes that were within 10% of one another. Pool statistics include the percent of the reach that was pools.';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeCollected" IS 'How much of slope was collected. Options of Fully Collected, Partially Collected, Not Collected';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeStartTransect" IS 'Transect that crews started measuring slope';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeEndTransect" IS 'Transect that crews ended measuring slope';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeReachLength" IS 'Length of the reach (m) measured for slope';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeFlag" IS 'Flag field highlighting the precision between recorded slope passes. Options of Within 10 percent, Within 20 percent, Not within 20 percent, Only one pass';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."TotalElevationChangePass1" IS 'The total elevation change of the reach from the first pass of the reach';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."TotalElevationChangePass2" IS 'The total elevation change of the reach from the second pass of the reach';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."TotalElevationChangePass3" IS 'The total elevation change of the reach from the third pass of the reach';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."SlopeMethod" IS 'Method used to collect slope. Transit levels are the main method used by most crews. Hand levels can be used for backpacking sites. Measurements taken with clinometers should not be trusted.';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."PoolsCollected" IS 'Were pools collected and if not why not. Options of Fully Collected, No Flow Not Collected, Collected No Pools Present, Not Collected or Partially Collected';
COMMENT ON COLUMN "aim_lotic"."F_SlopePoolSummary_W"."PoolReachLength" IS 'Length of reach (m) that was assessed for qualifying pools';

-- aim_lotic.F_Slope_W
COMMENT ON TABLE "aim_lotic"."F_Slope_W" IS 'Elevation change for each shot that was taken using transit and stadia rod, clinometer, or hand level. Slope is only collected at wadeable reaches.';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."Pass" IS 'Two measurements of total elevation change throughout the reach are taken and compared as a QC check in the field. Completing two measurements of total elevation change requires two passes of the reach (i.e. first walking up the reach and the second is completed walking down the reach).';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."Shot" IS 'One set of elevation measurements (start and end heights) within a pass of the reach';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."StartHeight" IS '1st Height from stadia rod used to compute elevation change';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."EndHeight" IS '2nd Height from stadia rod used to compute ElevationChange';
COMMENT ON COLUMN "aim_lotic"."F_Slope_W"."ElevationChange" IS 'Elevation change (m) for each shot taken (2 sightings of the stadia rod)';

-- aim_lotic.F_StreambedParticles_W
COMMENT ON TABLE "aim_lotic"."F_StreambedParticles_W" IS '10 Streambed particles measured at equal distances across the active channel (scour line to scour line) at 21 transects at wadeable reaches. Number of measurements and location of particles collected has changed across years.';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."ParticleNum" IS 'Sequential number indicating order particles were collected from left to right bank across the channel. Starting in 2021 and on extra particles collected in the wetted channel to meet protocol requirements are particles 11-15';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."StreambedLocation" IS 'Location of stream bed particles, whether they were taken from within the wetted channel (including midchannel bars) ("Dry Middle" or "Wet or Dry Middle") or outside the wetted channel (Dry edge). Within the wetted channel only, is the EPA method. The BLM added additional particles to be collected going up to scour or where bed meets bank.';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."ParticleSize" IS 'Size of substrate particle B axis';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."ParticleSizeClassNonMeas" IS 'Streambed particles that could not be measured were binned into categories. These categories can then be changed to quantitative measurements on QC of data where appropriate. These categories confused crews and led to large chunks of missing data so this parameter was omitted in 2017. Categories were also stored differently among years prior to this.';
COMMENT ON COLUMN "aim_lotic"."F_StreambedParticles_W"."ParticleSizeClass" IS 'Visually estimated size class of streambed particles along 5 equally spaced locations at main and intermediate transects';

-- aim_lotic.F_Thalweg_StreambedParticles_B
COMMENT ON TABLE "aim_lotic"."F_Thalweg_StreambedParticles_B" IS '100 inter-transect measurements of thalweg water depth at boatable reaches. Size class estimates are also taken while measuring thalweg based on rod contact. Size classes used are bedrock/hardpan, boulder, cobble, gravel, sand, silt/clay/muck.';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."Transect" IS '11 main are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects. Intermediate transects are not collected using the boatable protocol. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."ThalwegMeasNum" IS 'Sequential number indicating the order of thalweg depth measurements along the longitudinal profile';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."ThalwegDepth" IS 'Thalweg depth (m) taken at 100-300 equally spaced locations longitudinally along the reach length';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."ThalwegDepthMethod" IS 'Method used to collect thalweg depths. Poles (include stadia rods) are most common and units should be in meters, while sonar units may be in feet.';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."OffChannelHabitatPresent" IS 'Presence of off channel habitat at each thalweg station';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."SnagPresent" IS 'Snag presence at each thalweg station';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_StreambedParticles_B"."ParticleSizeClass" IS 'Visually estimated size class of streambed particles along 5 equally spaced locations at main and intermediate transects';

-- aim_lotic.F_Thalweg_W
COMMENT ON TABLE "aim_lotic"."F_Thalweg_W" IS '100+ inter-transect measurements of thalweg water depth at wadeable reaches. The number of measurements is dependent on the reach length and the interval between measurements can be found in the F_SampledReaches_W_B table.';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc.';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."ThalwegMeasNum" IS 'Sequential number indicating the order of thalweg depth measurements along the longitudinal profile';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."ThalwegDepth" IS 'Thalweg depth (m) taken at 100-300 equally spaced locations longitudinally along the reach length';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."FlowPresent" IS 'Was the water flowing at each thalweg station';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."SideChannelPresent" IS 'Presence of a side channel at each thalweg station';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."BackwaterPresent" IS 'Presence of a backwater at each thalweg measurement location';
COMMENT ON COLUMN "aim_lotic"."F_Thalweg_W"."BarPresent" IS 'Presence of a bar at each thalweg measurement location';

-- aim_lotic.F_VegComplexity_W_B
COMMENT ON TABLE "aim_lotic"."F_VegComplexity_W_B" IS 'Ocular cover, structure, and type estimates for left and right bank 10 X 10 m plots (wadeable) or 20 m X 10 m plots (boatable) at 11 transects';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc. Boating reaches do not collect intermediate transects.';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."Bank" IS 'Left or right bank of the stream as one looks downstream';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."CanopyVegType" IS 'Woody vegetation type of the canopy layer (> 5 m high). Options of Coniferous, Deciduous, Evergreen, Mixed or None.';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."CanopyBigTreeCover" IS 'Cover class for canopy layer (> 5 m high) for big trees (>0.3m DBH) (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."CanopySmallTreeCover" IS 'Cover class for canopy layer (> 5 m high) for small trees (<0.3 m DBH) (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."UnderstoryVegType" IS 'Vegetation type of the understory layer (0.5 m to 5 m high). Options of Coniferous, Deciduous, Evergreen, Mixed or None.';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."UnderstoryWoodyCover" IS 'Cover class for understory layer (0.5 m to 5 m high) for woody shrubs + saplings (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."UnderstoryNonWoodyCover" IS 'Cover class for understory layer (0.5 m to 5 m high) for non-woody herbs, grasses, forbs  (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."GroundNonWoodyCover" IS 'Cover class for ground layer (<0.5 m high) for non-woody herbs, grasses, forbs (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."GroundWoodyCover" IS 'Cover class for ground layer (<0.5 m high) for woody shrubs (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';
COMMENT ON COLUMN "aim_lotic"."F_VegComplexity_W_B"."BareGroundCover" IS 'Cover class for ground layer (<0.5 m high) for barren, bare dirt or duff (0-4) 0=absent: zero cover, 1=sparse:<10%, 2=moderate 10-40%, 3=heavy:41-75%,4=very heavy:>75%';

-- aim_lotic.F_VegSpecies_W_B
COMMENT ON TABLE "aim_lotic"."F_VegSpecies_W_B" IS 'Ocular assessment of absence/presence of priority woody native and woody and herbaceous noxious species in 10 m X 10 m (wadeable) or 20 m X 10 m (boatable) riparian plots at 11 transects on left and right banks.';
COMMENT ON COLUMN "aim_lotic"."F_VegSpecies_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_VegSpecies_W_B"."Transect" IS '11 main or 10 intermediate transects are established at systematic intervals throughout the reach and labeled alphabetically with A, B etc. for main transects and AB, BC for intermediate transects. Main transects may also extend to side channels if present which are designated by XA, XB etc. Boating reaches do not collect intermediate transects.';
COMMENT ON COLUMN "aim_lotic"."F_VegSpecies_W_B"."Bank" IS 'Left or right bank of the stream as one looks downstream';
COMMENT ON COLUMN "aim_lotic"."F_VegSpecies_W_B"."CommonName" IS 'Common name of priority noxious or native species. Names should be all lower case for input into Survey 123';

-- aim_lotic.F_WaterQualityMacroInvert_W_B
COMMENT ON TABLE "aim_lotic"."F_WaterQualityMacroInvert_W_B" IS 'In-situ measurements of water quality (ph, specific conductance, instant temperature, turbidity). 50 ml water samples collected in a sterile vial using gloves, preserved, and processed for total nitrogen and total phosphorus. Macroinvertebrate sample information such as area sampled and net type used. Analyzed macroinvertebrate can be found in the I_Indicator table or by contacting the USU National Aquatic Monitoring Center.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."MacroInvertCollectionDate" IS 'Actual date bug sample collected';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."MacroInvertCollectionMethod" IS 'Method used for collecting bugs (targeted riffle or reachwide)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NetType" IS 'Type of sampling device. Options of Surber net, Kick net, Mini Surber net or Hess net.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NetArea" IS 'Area of the sampling device used for macroinvertebrates';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NumMacroInvertLocationsSampled" IS 'Number of locations or transects sampled for bugs';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalAreaSampled" IS 'Total sampled area for macroinvertebrates (m2)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NumMacroInvertJars" IS 'Number of macroinvertebrate jars';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NAMC_MacroInvertSampleID" IS 'NAMC Lab SampleID to track Macroinvertebrate data';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."NAMC_WaterSampleID" IS 'NAMC Lab SampleID to track Water Quality data';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."CalibrationDate" IS 'Date the YSI or Sonde was calibrated.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."InstrumentSerialNum" IS 'Serial number of sonde or YSI';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."InstrumentModel" IS 'Model of sonde or YSI';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."SondeDateTimeCollected" IS 'Date and time that the sonde water quality measurements were taken';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."SpecificConductance" IS 'Measured specific conductance value using a Sonde. The specific conductance is conductivity standardized to 25 degrees C. (units: µS/cm, min: 0, max: 65500, n=1)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TempCorrected" IS 'Was conductivity corrected to 25C (specific conductance)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."SpecificConductanceFlag" IS 'Whether measurements outside the typical range were validated in any manner or if they were suspect or reasonable.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."pH" IS 'Measured pH value using a Sonde (units: SU, min: 0, max: 14, n=1)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."pHFlag" IS 'Whether measurements outside the typical pH range were validated in any manner or if they were suspect or reasonable.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."InstantTemp" IS 'Instantaneous temperature collected with sonde';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."InstantTempFlag" IS 'Temperature may be flagged as suspect if there is reason to suspect the YSI is not reading correctly. Values may be flagged as reasonable if values were outside the range of typical values but there is no reason to believe this value is incorrect.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TurbidityReading1" IS 'Three readings of turbidity (NTU) are taken at the reach. Any one of the three readings should be within 30% of the others. For data collected before 2021 the average of the three readings is recorded for all three readings';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TurbidityReading2" IS 'Three readings of turbidity (NTU) are taken at the reach. Any one of the three readings should be within 30% of the others. For data collected before 2021 the average of the three readings is recorded for all three readings';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TurbidityReading3" IS 'Three readings of turbidity (NTU) are taken at the reach. Any one of the three readings should be within 30% of the others. For data collected before 2021 the average of the three readings is recorded for all three readings';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TurbidityFlag" IS 'Comments about calibration issues';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TNTP_DateTimeCollected" IS 'Actual date and time water quality grab sample collected';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TNTP_PreservationStatus" IS 'Whether grab samples were preserved by freezing or acidifying and then freezing. The amount of time before freezing is also noted categorially in some field seasons but categories have changed among field seasons.';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."WaterQualityLab" IS 'Lab that processed the sample';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."Date_TNTP_Analyzed" IS 'Date NAMC sent WQ samples to lab to be analyzed. Note this is not the date the lab actually analyzed the samples';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalNitrogenOriginal" IS 'Measured total nitrogen value (units: µg/L, n=1)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalNitrogenDuplicate" IS 'Total Nitrogen (ug/L) in the duplicate sample that was collected in the field at the same time and manner as the original sample';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalNitrogenBlank" IS 'Total Nitrogen (ug/L) in the blank sample that was collected in the field by filling a vial with DI water';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalNitrogenFlag" IS 'Lab comments about processing the grab sample for total nitrogen or any concern with the values especially after reviewing the duplicate and blank samples';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalPhosphorusOriginal" IS 'Measured total phosphorous value (units: µg/L, n=1)';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalPhosphorusDuplicate" IS 'Total Phosphorus (ug/L) in the duplicate sample that was collected in the field at the same time and manner as the original sample';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalPhosphorusBlank" IS 'Total Phosphorus (ug/L) in the blank sample that was collected in the field by filling a vial with DI water';
COMMENT ON COLUMN "aim_lotic"."F_WaterQualityMacroInvert_W_B"."TotalPhosphorusFlag" IS 'Lab comments about processing the grab sample for total phosphorus or any concern with the values especially after reviewing the duplicate and blank samples';


-- aim_lotic.I_Indicators
COMMENT ON TABLE "aim_lotic"."I_Indicators" IS 'This feature class includes monitoring data collected nationally to understand the status, condition, and trend of resources on BLM lands. Data are collected in accordance with the BLM Assessment, Inventory, and Monitoring (AIM) Strategy. The AIM Strategy specifies a probabilistic sampling design, standard core indicators and methods, electronic data capture and management, and integration with remote sensing. Attributes include the BLM aquatic core indicators: pH, conductivity, temperature, pool depth, length, frequency, streambed particles sizes, bank stability and cover, floodplain connectivity, large woody debris, macroinvertebrate biological integrity, ocular estimates of vegetative type, cover, and structure and canopy cover. In addition, the contingent indicators of total nitrogen and phosphorous, turbidity, bank angle, thalweg depth profile and quantitative vegetation estimates (see the Data Structure and Attribute Information section for exact details on attributes). Data were collected and managed by BLM Field Offices, BLM Districts, and/or affiliated field crews with support from the BLM National Operations Center. Data are stored in a centralized database (BLM AIM Lotic Database) at the BLM National Operations Center.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."Project" IS 'The BLM administrative unit associated with the design and points. National AIM (WRSA) points are associated with district or field office level projects if these points are reused at those specific levels in a design.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BLM_AdminState" IS 'BLM administrative state';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."District" IS 'BLM administrative district';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."FieldOffice" IS 'BLM administrative field office';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PointID" IS 'Unique location identifier; 2-3 letter prefix indicating the original stratum, followed by the stream order group, and 4-5 digit random code specifying the order the points were selected';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."StreamName" IS 'Name of stream, default is name from NHD but project leads may edit';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."FieldEvalDate" IS 'Date the point or reach was evaluated to determine if it could be sampled or not';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."SampledMidLatitude" IS 'Latitude of the F transect of the sampled reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."SampledMidLongitude" IS 'The longitude of the F transect of the sampled reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BottomReachLatitude" IS 'Latitude of bottom of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BottomReachLongitude" IS 'Longitude of bottom of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."TopReachLatitude" IS 'Latitude of top of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."TopReachLongitude" IS 'Longitude of top of reach in decimal degrees and WGS84 datum';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ProtocolReachLength" IS 'Total reach length intended to be sampled. Calculated in the app from average widths using the protocol rules';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ProtocolType" IS 'Protocol used to sample the reach. Wadeable or Boatable.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ProtocolVersion" IS 'Pdf archive of the protocol used for data collection';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."FieldStatus" IS 'Whether the reach was partially or fully sampled or had interrupted flow. FieldStatus is used to determine required sample sizes for indicators to be reported.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PointSelectionType" IS 'Whether the point was selected from a spatially balanced random sample using Generalized Random Tessellation Stratified (RandomGRTS) algorithms, systematically random sample(RandomSystematic), or a hand selected (targeted) monitoring location';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."OriginalDesign" IS 'Original DesignID of the point';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."OriginalStratum" IS 'Original stratum of the point';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."StreamOrder" IS 'Modified Strahler Stream Order';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."EcoregionStreamSize" IS 'This field is a combination of hybrid level 3 Omernick ecoregion (lower 48 states) or level 3 Omernick ecoregion (Alaska) , stream size based on bankfull width, and protocol.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctOverheadCover" IS 'Average % overhead cover provided by stream banks, vegetation, or other objects measured mid-channel (looking 4 directions) across 11 transects (units: %, min: 0, max: 100, n= 44)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankOverheadCover" IS 'Average percent overhead cover provided by stream banks (left and right), vegetation or other objects measured at the scour line of the left and right banks across 11 transects (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."VegComplexity" IS 'Average vegetative cover provided by three different vegetative height categories: Canopy (>5m), Understory (0.5-5m), and Ground (<0.5m). Each vegetative height category is then divided into two vegetation types (e.g. woody or nonwoody). Proportional cover was binned into four classes (0.875, 0.575, 0.25, and 0.05) per vegetation type, summed across the three heights, and then averaged across the left and right banks of 11 transects. (units: none, min: 0, max: 2.6, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."VegComplexityWoody" IS 'Average woody vegetative cover provided by three different vegetative height categories: Canopy (>5m), Understory (0.5-5m), and Ground (<0.5m). Each vegetative height category is then divided into two vegetation types. Proportional cover was binned into four classes (0.875, 0.575, 0.25, and 0.05), summed across the three heights, and then averaged across the left and right banks of 11 transects. (units: none, min: 0, max: 2.6, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."VegComplexityUnderstoryGround" IS 'Average vegetative cover provided by two different vegetative height categories: Understory (0.5-5m), and Ground (<0.5m). Each vegetative height category is then divided into two vegetation types (e.g. woody or nonwoody). Proportional cover was binned into four classes (0.875, 0.575, 0.25, and 0.05) per vegetation type, summed across the two heights, and then averaged across the left and right banks of 11 transects. (units: none, min: 0, max: 2.6, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctNoxiousWoodySpecies" IS 'Percent of 22 vegetation plots with priority noxious woody vegetation present (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctNativeWoodySpecies" IS 'Percent of 22 vegetation plots with priority native woody vegetation present (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctNoxiousHerbSpecies" IS 'Percent of 22 vegetation plots with priority noxious herbaceous vegetation present (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctSedgeRushSpecies" IS 'Percent of 22 vegetation plots with sedges and rushes present (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctEquisetumSpecies" IS 'Percent of 22 vegetation plots with equisetum present (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."InvasiveInvertSpecies" IS 'List of invasive macroinvertebratespresent, if not present value is "Absent"';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ObservedInvertRichness" IS 'Observed macroinvertebrate richness standardized to model specific operational taxonomic units (OTU) (units: # of taxa)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ExpectedInvertRichness" IS 'Expected macroinvertebrate richness in the absence of anthropogenic impacts from the O/E model (units: # of taxa)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."OE_Macroinvertebrate" IS 'Biological condition was assessed using an observed/expected (O/E) index. O/E models compare the macroinvertebrate taxa observed at sites of unknown biological condition (i.e., ‘test sites’) to the assemblages expected to be found in the absence of anthropogenic stressors (see Hawkins et al. 2000 for details). (units: none, min: 0, max: 1.5)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."MMI_Macroinvertebrate" IS 'Biological condition was assessed using the MMI (MultimetricIndex) model specified in theOE_MMI_ModelUsedcolumn.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."OE_MMI_ModelUsed" IS 'The O/E or MMI model used to determine biological integrity. NAMC currently has the following models available UT, NV, CA, CO, OR, WY, regional models for areas sampled by AREMP or PIBO programs (Northwest Forest Plan or Columbia River Basin), and a West-wide model. Generally, State based models are used ifavailable,otherwise the West-wide model is used.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."OE_MMI_ModelApplicability" IS 'This field indicates whether or not the site''s environmental gradients were within the range of experience of the model. A fail indicates the model potentially had to extrapolate, rather than interpolate, to accommodate one or more of the habitat variables. O/E scores and condition ratings should be interpreted cautiously if a site failed the test of experience.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."MacroinvertebrateCount" IS 'Number of macroinvertebrates identified and resampled to a standardized fixed count (i.e. rarefaction). Samples with counts less than 200 macroinvertebrates can result from sampling and/or laboratory processing errors, but low counts can also be a signal of degraded biological condition. Additional samples should be taken to verify Major or Moderate departure from reference. (units: # of individuals, min: 0, max: 400)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."TotalNitrogen" IS 'Measured total nitrogen value (units: µg/L, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."TotalPhosphorous" IS 'Measured total phosphorous value (units: µg/L, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."SpecificConductance" IS 'Measured specific conductance value using a Sonde. The specific conductance is conductivity standardized to 25 degrees C. (units: µS/cm, min: 0, max: 65500, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."pH" IS 'Measured pH value using a Sonde (units: SU, min: 0, max: 14, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."InstantTemp" IS 'Instantaneous temperature collected with sonde';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."AugTempAvg" IS 'Site specific prediction of 19 year average August stream temperature for the period of 1993 – 2011 as derived fromNorWestmodels(Isaak et al.2016https://doi.org/10.2737/RDS-2016-0033)(units: degrees C, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."TurbidityAvg" IS 'Average water clarity as measured by the suspended solids in the water column (units: NTU, n=3)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PoolCount" IS 'The number of qualifying pools within the reach (units: # pools, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctPools" IS 'Percent of the sample reach (linear extent) classified as pool habitat as assessed using the core pool method (units: %, min: 0, max: 100, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PoolFreq" IS 'Frequency of pools in the reach as assessed using the core pool method (units: # pools/km, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ResPoolDepthAvg" IS 'Average residual pool depth as assessed using the core pool method (units: m, n= variable depending on number of pools)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodInChanCount" IS 'Number of pieces of wood within the bankfull channel of the reach (units: # pieces, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodInChanFreq" IS 'Frequency of large wood within the bankfull channel of the reach (units: # pieces/ 100 m, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodInChanVol" IS 'Volume of large wood within the bankfull channel of the reach (units: m^3/100 m, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodAboveChanCount" IS 'Number of pieces of wood bridging above the bankfull channel of the reach (units: # pieces, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodAboveChanFreq" IS 'Frequency of large wood bridging above the bankfull channel of the reach (units: # pieces/ 100 m, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."LgWoodAboveChanVol" IS 'Volume of large wood bridging above the bankfull channel of the reach (units: m^3/100 m, n=1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctFinesLessThan2mm" IS 'Average percent fine sediment (≤ 2mm) of measured particles (units: %, min: 0, max: 100, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctFinesLessThan6mm" IS 'Average percent fine sediment (≤ 6mm) of measured particles (units: %, min: 0, max: 100, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."D16" IS 'Particle size corresponding to the 16th percentile of measured particles (units: mm, min: 1, max: 4098, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."D84" IS 'Particle size corresponding to the 84th percentile of measured particles (units: mm, min: 1, max: 4098, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."D50" IS 'Particle size corresponding to the 50th percentile of measured particles (units: mm, min: 1, max: 4098, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."GeometricMeanParticleDiam" IS 'Geometric average bed particle diameter= exponentialfunction[average(log(particle diameter)]. This is a less frequently used metric of characterizing central tendency of substrate sizes, but is the main metric used by the EPA for determining relative bed stability. It is less variable than a D50 and more biologically meaningful because it is more influenced by fine sediment. (units: mm, min: 1, max: 4098, n=210)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctPoolTailFinesLessThan2mm" IS 'Average percent fine sediment (≤ 2mm) on the pool tail (units %, min: 0, max: 100, n= 3 per pool)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctPoolTailFinesLessThan6mm" IS 'Average percent fine sediment (≤ 6mm) on the pool tail (units %, min: 0, max: 100, n=3 per pool)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankCoveredMIM" IS 'Percent of 42 erosional or depositional banks with greater than 50% foliar cover provided by perennial vegetation, wood or mineral substrate > 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankStable" IS 'Percent of 42 banks lacking visible signs of active erosion (e.g., slump, slough, fracture) (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankCoveredStableMIM" IS 'Percent of 42 banks both stable (lacking visible signs of active erosions (e.g., slump, slough, fracture)) and covered (greater than 50%foliarcover provided by perennial vegetation, wood or mineral substrate > 15 cm) (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankCoveredOld" IS 'Percent of 42 erosional banks with greater than 50% basal cover provided by perennial vegetation, wood or mineral substrate > 15 cm (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBankCoveredStableOld" IS 'Percent of 42 banks both stable (lacking visible signs of active erosions (e.g., slump, slough, fracture)) and covered (greater than 50%basalcover provided by perennial vegetation, wood or mineral substrate > 15 cm) (units: %, min: 0, max: 100, n= 42)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BankfullHeightAvg" IS 'Average bankfull height measured from water surface across 11 transects (units: m, n = 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BenchHeightAvg" IS 'Average bench height measured from water surface across 11 transects (units: m, n = 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BankfullDepthAvg" IS 'Average bankfull depth measured from the thalweg across 11 transects. (bankfull height + thalweg depth) (units: m, n = 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BenchDepthAvg" IS 'Average bench depth measured from the thalweg across 11 transects (bench height + thalweg depth) (units: m, n = 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ChannelIncision" IS 'Logarithm of the difference between average bankfull height and average bench height= log(BenchHeight-BankHeight+ 0.1) (units: none, min: -1, max: 2, n=11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."FloodplainConnectivity" IS 'The ratio of average bench height to average bankfull height taken from the thalweg = (bench height + thalweg depth) / (bankfull height + thalweg depth). This is also known asRosgen''sBank Height Ratio (units: none, n= 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."InstreamHabitatComplexity" IS 'Aggregate measure of average cover provided by boulders, overhanging vegetation, live trees and roots, large wood, small wood, and stream banks for stream fishes measured at 11 plots. Proportional cover was binned into four classes (0.875, 0.575, 0.25, and 0.5), averaged across transects, and then summed across six types of cover. (units: none, min: 0, max: 2.3, n= 66)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BankAngleAvg" IS 'Average angle of the stream bank; banks with obtuse angles = >90° and undercut banks with acute angles = <90° (units: degrees, min: 0, max: 180, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctBanksUndercut" IS 'Percent of 22 banks with undercuts (angles <90°) (units: %, min: 0, max: 100, n= 22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ThalwegDepthCV" IS 'Indicator of bed heterogeneity computed as the coefficient of variation of 100-300 thalweg depth measurements (units: none, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."ThalwegDepthAvg" IS 'Average thalweg depth. Metric of how deep water was at the site. Only calculated if thalweg depth profile contingent method was collected. (units: m, min: 0, max: none, n variable depending on reach length (100 - 300))';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctDry" IS 'Percent of the reach that was dry. This is calculated as the number of dry thalweg measurements divided by the total number of thalweg measurements collected and expressed as a percentage. (units: %, min: 0, max: 100, n= variable depending on reach length (100-300))';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BankfullWidthAvg" IS 'Average bankfull width across 11 transects (units: m, n= 11)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."WettedWidthAvg" IS 'Average wetted width across 21 transects (units: m, n= 21)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."WettedWidthWithBarAvg" IS 'Average wetted width including bars across 21 transects (units: m, n= 21)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."EntrenchmentRiffle1" IS 'Entrenchment ratio at the downstream end of the reach = floodprone width measurement at riffle 1 divided by bankfull width at riffle 1. Ratios of 1-1.4 represent entrenched streams; 1.41-2.2 represent moderately entrenched streams; and ratios greater than 2.2 indicate rivers only slightly entrenched in a well-developed floodplain (Rosgen 1996). (units: none, min: 1, max: 3, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."EntrenchmentRiffle2" IS 'Entrenchment ratio at the upstream end of the reach = floodprone width measurement at riffle 2 divided by bankfull width at riffle 2. Ratios of 1-1.4 represent entrenched streams; 1.41-2.2 represent moderately entrenched streams; and ratios greater than 2.2 indicate rivers only slightly entrenched in a well-developed floodplain (Rosgen 1996). (units: none, min: 1, max: 3, n= 1)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."PctSlope" IS 'Reach slope measured from the water''s surface. In most cases, the reported value is an average of 2 independent measurements that were within 10% of one another. (units: %, min: 0, max: ~45, n= 2)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."Sinuosity" IS 'Sinuosity is calculated as protocol reach length / the Euclidean distance between top and bottom of reach coordinates';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."HumanInfluence" IS 'Metric of human influence in the reach and its proximity to the stream; visually estimated in 10 m x 10 m plots at left and right banks and all 11 main transects (units:None, min: max, n=22)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BeaverFlowMod" IS 'Flow influence by beaver across the reach';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."BeaverSigns" IS 'Qualitative visual assessment of frequency of beaver signs (e.g. chewed logs) within the reach (Absent, Rare, Common)';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."WaterWithdrawals" IS 'Presence or absence of water withdrawals';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."SideChannels" IS 'Presence of absence of side channelsacross the entire reach';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."EvaluationID_OLD" IS 'Unique visit identifier from NAMC. Generated at time of data collection by the iPad.';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."CoreSubset" IS 'Were all a subset of core indicators sampled? Y/N';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."Supplementals" IS 'List of supplemental indicators collected';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."GreenlineVegComposition" IS 'Was greenline vegetation composition collected? Y/N';
COMMENT ON COLUMN "aim_lotic"."I_Indicators"."geom" IS 'Point feature geometry (EPSG:4269 NAD83)';

-- aim_lotic.I_VegSpeciesFreqOccurrence
COMMENT ON TABLE "aim_lotic"."I_VegSpeciesFreqOccurrence" IS 'Frequency of occurrence of priority noxious and native vegetative species';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."EvaluationID" IS 'Unique visit or evaluation identifier; PointID concatenated with the evaluation/ sampled date';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."CommonName" IS 'Common name of priority noxious or native species. Names should be all lower case for input into Survey 123';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."ScientificName" IS 'The scientific name of the vegetation species present in the plot';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."PercentPlotsPresent" IS 'The percent of vegetation plots collected that contained a given species';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."Noxious" IS 'Whether or not the species is noxious or not';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."WoodyHerb" IS 'Whether the species is woody or herbaceous';
COMMENT ON COLUMN "aim_lotic"."I_VegSpeciesFreqOccurrence"."geom" IS 'Point feature geometry (EPSG:4269 NAD83)';

-- aim_lotic.LU_Ecoregion
COMMENT ON TABLE "aim_lotic"."LU_Ecoregion" IS 'Omerick level I, II, III ecoregions. These ecoregions are frequently used for lotic design and setting benchmarks.';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."fid" IS 'Internal feature number.';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."EcoregionLevel3" IS 'Omernik level 3 ecoregion';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."EcoregionLevel2" IS 'Omernik level 2 ecoregion';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."EcoregionLevel1" IS 'Omernik level 1 ecoregion';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."Ecoregion" IS 'Omernik (1987) level 3 (AK) or hybrid level 3 (Lower 48 states) ecoregions.';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."EcoregionCode" IS 'Two letter abbreviation for each ecoregion';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."Climate" IS 'Groups of Omerick ecoregions with similar climate (Mountains, Plains, Xeric)';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."ClimateCode" IS 'Abbreviations for climate type';
COMMENT ON COLUMN "aim_lotic"."LU_Ecoregion"."geom" IS 'Multipolygon feature geometry (EPSG:4269 NAD83).';

-- aim_lotic.LU_SpeciesMetadata
COMMENT ON TABLE "aim_lotic"."LU_SpeciesMetadata" IS 'A species list of priority native woody and noxious woody and herbaceous species for the western United States. Species information includes things such as common name, scientific name, and noxious status.';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."CommonName" IS 'Common name of priority noxious or native species. Names should be all lower case for input into Survey 123';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."ScientificName" IS 'The scientific name of the vegetation species present in the plot';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."name" IS 'Code friendly name CommonName with spaces replaced with underscores. This list is what Survey123 uses for select multiples.';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."label" IS 'Label field used by Survey123.';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."WoodyHerb" IS 'Whether the species is woody or herbaceous';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."GrowthHabit" IS 'Whether species is a tree, forb, shrub, graminoid';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."Duration" IS 'Whether a non-native species is woody or herbaceous';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."Image" IS 'Associated .JPG file linked into Survey123';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."AlwaysNoxious" IS 'Whether or not a species is always considered Noxious.';
COMMENT ON COLUMN "aim_lotic"."LU_SpeciesMetadata"."AlwaysNative" IS 'Whether or not a species is always considered Native.';

-- aim_lotic.LU_StateSpeciesList
COMMENT ON TABLE "aim_lotic"."LU_StateSpeciesList" IS 'A species list of priority native woody and noxious woody and herbaceous species for each state. Species information includes things such as common name, scientific name, and noxious status.';
COMMENT ON COLUMN "aim_lotic"."LU_StateSpeciesList"."SpeciesState" IS 'Coded value that generally corresponds to BLM AdminState. Exceptions include areas like OR, where there are 2 species lists: WOR, EOR';
COMMENT ON COLUMN "aim_lotic"."LU_StateSpeciesList"."CommonName" IS 'User friendly common name of priority noxious or native species.';
COMMENT ON COLUMN "aim_lotic"."LU_StateSpeciesList"."Noxious" IS 'Yes/No field used to indicate if a species is considered noxious.';
COMMENT ON COLUMN "aim_lotic"."LU_StateSpeciesList"."Year" IS '4 digit year that a list was created. Lists are still considered active until a newer "Year" is added to the list for a particular SpeciesState.';

