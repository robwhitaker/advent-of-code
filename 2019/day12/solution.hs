import qualified Text.ParserCombinators.ReadP as Parser
import Text.ParserCombinators.ReadP (ReadP)
import Control.Monad (void, guard)
import Data.List (sort)
import qualified Data.Set as Set

data Vec3 = Vec3
    { getX :: Int
    , getY :: Int
    , getZ :: Int
    } deriving (Show, Ord, Eq)

data Moon = Moon
    { moonLoc :: Vec3
    , moonVel :: Vec3
    } deriving (Show, Ord, Eq)

vec3Add :: Vec3 -> Vec3 -> Vec3
vec3Add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

vec3Parser :: ReadP Vec3
vec3Parser = do
    pairs <- Parser.between
        (Parser.char '<')
        (Parser.char '>')
        (Parser.sepBy parseAssignment (Parser.string ", "))
    guard (length pairs == 3)
    let [x, y, z] = map snd $ sort pairs
    return $ Vec3 x y z
  where
    parseAssignment = do
        key <- Parser.munch1 (/= '=')
        void $ Parser.char '='
        val <- Parser.munch1 (\ch -> ch /= '>' && ch /= ',')
        return (key, read val)

parse :: ReadP a -> String -> Maybe a
parse parser input =
    case Parser.readP_to_S parser input of
        [] -> Nothing
        xs -> Just $ fst (last xs)

getInput :: IO [Moon]
getInput = do
    input <- readFile "input.txt"
    case traverse (parse vec3Parser) (lines input) of
        Nothing ->
            error "Failed to parse input."

        Just moonCoords ->
            return $ map (\loc -> Moon loc (Vec3 0 0 0)) moonCoords

velocityFromGravity :: Moon -> Moon -> (Moon, Moon)
velocityFromGravity moon1@(Moon loc1 vel1) moon2@(Moon loc2 vel2) =
    let (xMod1, xMod2) = getLocationModification getX
        (yMod1, yMod2) = getLocationModification getY
        (zMod1, zMod2) = getLocationModification getZ
    in
    ( moon1 { moonVel = vec3Add vel1 $ Vec3 xMod1 yMod1 zMod1 }
    , moon2 { moonVel = vec3Add vel2 $ Vec3 xMod2 yMod2 zMod2 }
    )
  where
    getLocationModification getter =
        if getter loc1 > getter loc2 then
            (-1, 1)
        else if getter loc1 < getter loc2 then
            (1, -1)
        else
            (0, 0)

sumVelocities :: [Moon] -> Moon
sumVelocities =
    foldl1 (\moon@(Moon _ vel) (Moon _ velDelta) ->
        moon { moonVel = vec3Add vel velDelta }
    )

applyVelocity :: Moon -> Moon
applyVelocity (Moon loc vel) =
    Moon (vec3Add loc vel) vel

step :: [Moon] -> [Moon]
step moons =
    map applyVelocity $ loopUntilEnd moons []
  where
    loopUntilEnd [] acc = reverse acc
    loopUntilEnd (m:ms) acc =
        let
            newMoons =
                scanl (\(cMoon, _) otherMoon ->
                    velocityFromGravity cMoon otherMoon
                ) (m, m) ms
            cMoon = fst $ last newMoons
            rest = map snd $ drop 1 newMoons
        in
        loopUntilEnd rest (cMoon : acc)

calcEnergy :: Vec3 -> Int
calcEnergy (Vec3 x y z) =
    abs x + abs y + abs z

potE :: Moon -> Int
potE (Moon loc _) =
    calcEnergy loc

kinE :: Moon -> Int
kinE (Moon _ vel) =
    calcEnergy vel

totalE :: Moon -> Int
totalE moon =
    potE moon * kinE moon

problem1 :: IO ()
problem1 = do
    moons <- getInput
    let step1000 = iterate step moons !! 1000
        energy = foldr ((+) . totalE) 0 step1000
    mapM_ print step1000
    print energy

problem2 :: IO ()
problem2 = do
    moons <- getInput
    let steps = iterate step moons
        n = loop (0, Set.empty) steps
    print n
  where
    loop (count, moonSet) (m:ms) =
        if Set.member m moonSet
           then count
           else loop (count+1, Set.insert m moonSet) ms
