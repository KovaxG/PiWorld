module Constants where

import Network.Simple.TCP (HostPreference (..), ServiceName)

import GameTypes

-- "System settings" I don't expect these to really change

ticksPerDay :: Int
ticksPerDay = 86400 --24 * 60 * 60

host :: HostPreference
host = Host "127.0.0.1" --"192.168.0.136"

port :: ServiceName
port = "80"

receiveBufferSize :: Int
receiveBufferSize = 1024

gameLoopDelay :: Int
gameLoopDelay = round $ (86400.0 / fromIntegral ticksPerDay) * 1000000

-- Rates per Day

eatingRatePerDay :: Int
eatingRatePerDay = 3

hungerRecoveryRatePerDay :: Int
hungerRecoveryRatePerDay = 300

hungerRatePerDay :: Int
hungerRatePerDay = 100

healingRatePerDay :: Int
healingRatePerDay = 10

damageRatePerDay :: Int
damageRatePerDay = 20

gatherRatePerDay :: Int
gatherRatePerDay = 10

explorationRatePerDay :: Int
explorationRatePerDay = 200

-- Other

startingFood :: Double
startingFood = 16.0

capacity :: BuildingSize -> Int
capacity size = case size of
  Tiny -> 50
  Small -> 400
  Normal -> 3200
  Big -> 25600

toolFactor :: Tool -> Double
toolFactor tool = case tool of
  EmptyHanded -> 1.0
  ChippedStone -> 2.0
  StoneTool -> 3.0
  CopperTool -> 4.0
  IronTool -> 4.5
  SteelTool -> 4.9

buildingWoodCost :: BuildingSize -> Int
buildingWoodCost size = case size of
  Tiny -> 23
  Small -> 67
  Normal -> 201
  Big -> 603

buildingStoneCost :: BuildingSize -> Int
buildingStoneCost size = case size of
  Tiny -> 100
  Small -> 300
  Normal -> 900
  Big -> 2700

-- Rates per Tick. Do not change these since they are derived from daily rates!

perDayToPerTick :: Int -> Double
perDayToPerTick a = fromIntegral a / fromIntegral ticksPerDay

eatingRatePerTick :: Double
eatingRatePerTick = perDayToPerTick eatingRatePerDay

hungerRecoveryRatePerTick :: Double
hungerRecoveryRatePerTick = perDayToPerTick hungerRecoveryRatePerDay

hungerRatePerTick :: Double
hungerRatePerTick = perDayToPerTick hungerRatePerDay

healingRatePerTick :: Double
healingRatePerTick = perDayToPerTick healingRatePerDay

damageRatePerTick :: Double
damageRatePerTick = perDayToPerTick damageRatePerDay

explorationRatePerTick :: Double
explorationRatePerTick = perDayToPerTick explorationRatePerDay

gatherRatePerTick :: Double
gatherRatePerTick = perDayToPerTick gatherRatePerDay

overfilledResourceDegradationRatePerTick :: Double
overfilledResourceDegradationRatePerTick = 0.2

-- Graphics

hiddenMapTile :: String
hiddenMapTile = "favicon.ico"
