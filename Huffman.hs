import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
import           Test.HUnit

-- Assignement 2 - data type
data HTree = HLeaf Char Int
                | HNode Int HTree HTree
    deriving (Show)

type Map = Map.Map
data Bit = Zero | One
    deriving (Eq, Show)
type CodingTable = Map Char [Bit]

type Worklist = IntMap.IntMap [HTree]

{--type Assertion = IO ()
assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqual what expected real =
    if expected == real
    then putStrLn ("[ok]" ++ what)
    else fail
            (what ++ ": assertEqual failed. Expected: " ++ show expected ++ ", given: " ++ show real)--}

-- Assignement 2 tree representation
exampleTree = HNode 606
    (HNode 337 (HLeaf 'E' 158)
        (HNode 179 (HLeaf 'N' 97) (HLeaf 'I' 82))
    )
    (HNode 269
        (HNode 144 (HLeaf 'R' 77) (HLeaf 'S' 67))
        (HNode 125 (HLeaf 'T' 64) (HLeaf 'A' 61))
    )

inconsistentTree_1 = HNode 606
    (HNode 337 (HLeaf 'E' 158)
        (HNode 179 (HLeaf 'N' 97) (HLeaf 'I' 82))
    )
    (HNode 269
        (HNode 144 (HLeaf 'R' 77) (HLeaf 'S' 67))
        (HNode 130 (HLeaf 'T' 64) (HLeaf 'A' 61))
    )

inconsistentTree_2 = HNode 606
    (HNode 337 (HLeaf 'E' 158)
        (HNode 179 (HLeaf 'N' 97) (HLeaf 'I' 82))
    )
    (HNode 269
        (HNode 144 (HLeaf 'R' 77) (HLeaf 'S' 67))
        (HNode 125 (HLeaf 'T' 64) (HLeaf 'A' 63))
    )

inconsistentTree_3 = HNode 607
    (HNode 337 (HLeaf 'E' 158)
        (HNode 179 (HLeaf 'N' 97) (HLeaf 'I' 82))
    )
    (HNode 269
        (HNode 144 (HLeaf 'R' 77) (HLeaf 'S' 67))
        (HNode 125 (HLeaf 'T' 64) (HLeaf 'A' 63))
    )

inconsistentTree_4 = HNode 606
    (HNode 338 (HLeaf 'E' 158)
        (HNode 179 (HLeaf 'N' 97) (HLeaf 'I' 82))
    )
    (HNode 269
        (HNode 144 (HLeaf 'R' 77) (HLeaf 'S' 67))
        (HNode 125 (HLeaf 'T' 64) (HLeaf 'A' 63))
    )


-- Assignement 3
isConsistent :: HTree -> Bool
isConsistent (HLeaf character val) = True
isConsistent (HNode val treeLeft treeRight) =
    if (val == (getValueOfNode treeLeft + getValueOfNode treeRight)) && (isConsistent treeLeft) && (isConsistent treeRight)
        then True
        else False

getValueOfNode :: HTree -> Int
getValueOfNode (HLeaf _ val)    = val
getValueOfNode (HNode val _ _ ) = val


-- Assignement 4
toCodingTable :: HTree -> CodingTable
toCodingTable (HNode _ treeLeft treeRight) = Map.union (toCodingTableWithBitArray [Zero] treeLeft) (toCodingTableWithBitArray [One] treeRight)

toCodingTableWithBitArray :: [Bit] -> HTree -> CodingTable
toCodingTableWithBitArray array (HLeaf letter _ ) = Map.singleton letter array
toCodingTableWithBitArray array (HNode _ treeLeft treeRight) = Map.union (toCodingTableWithBitArray (array ++ [Zero]) treeLeft) (toCodingTableWithBitArray (array ++ [One]) treeRight)

codingTableExampleTree = Map.fromList [ ('E', [Zero, Zero]),
                                        ('N', [Zero, One, Zero]),
                                        ('I', [Zero, One, One]),
                                        ('R', [One, Zero, Zero]),
                                        ('S', [One, Zero, One]),
                                        ('T', [One, One, Zero]),
                                        ('A', [One, One, One])
                                        ]


-- Assignement 5
encode :: CodingTable -> String -> [Bit]
encode table [] = []
encode table (x:xs) = case (Map.lookup x table) of
                        Nothing -> error "String cannot be encoded with the provided CodingTable"
                        Just x -> x ++ (encode table xs)


-- Assignment 6
decode :: HTree -> [Bit] -> String
decode _ [] = ""
decode (HLeaf _ _ ) _ = error "Huffman tree only consist of one leaf"
decode rootTree array = reverse(decodeWithString rootTree array "")
    where
        decodeWithString :: HTree -> [Bit] -> String -> String
        decodeWithString (HLeaf letter _) array string = decodeWithString rootTree array (letter:string)
        decodeWithString (HNode _ treeLeft treeRight) (bit:array) string =
            if bit == Zero
                then decodeWithString treeLeft array string
                else decodeWithString treeRight array string
        decodeWithString _ [] string = string

-- Assignment 7
buildHTree :: String -> HTree
buildHTree [] = error "Cannot derive a Huffman tree from one letter"
buildHTree string = getTreeFromFrequencyMap (buildWorklist string)

-- Worklist
buildWorklist :: String -> Worklist
buildWorklist string = IntMap.fromListWith (++) derived
    where derived = [(frequency, [HLeaf letter frequency]) | (letter, frequency) <- Map.toList (Map.fromListWith (+) [(char, 1) | char <- string])]

-- Build Tree
getTreeFromFrequencyMap :: Worklist -> HTree
getTreeFromFrequencyMap freqMap = do
        let headTrees = snd (head (IntMap.toList freqMap))
        if (IntMap.size freqMap ==  1) && (length headTrees == 1)
            then head(headTrees)
            else combineNodes (IntMap.deleteFindMin freqMap)

-- Combine two trees into one node
combineNodes :: ((Int, [HTree]), Worklist) -> HTree
combineNodes ((key, tree1:tree2:otherTrees), treeMap) = do
        let newNodeValue = getValueOfNode tree1 + getValueOfNode tree2
        let newNode = HNode newNodeValue tree1 tree2
        let updatedMap = IntMap.insertWith (++) newNodeValue [newNode] treeMap 
        let mapWithOldKey = IntMap.insertWith (++) key otherTrees updatedMap
        getTreeFromFrequencyMap mapWithOldKey
combineNodes ((key, tree1:[]), treeMap) = combineExistingNodeWithNode (IntMap.deleteFindMin treeMap) tree1
combineNodes ((key, []), treeMap) = getTreeFromFrequencyMap treeMap

-- Helper function to be used when getting only one tree in "combineNodes"
combineExistingNodeWithNode :: ((Int, [HTree]), Worklist) -> HTree -> HTree
combineExistingNodeWithNode ((key, tree2:otherTrees), treeMap) tree1 = do
        let newNodeValue = getValueOfNode tree1 + getValueOfNode tree2
        let newNode = HNode newNodeValue tree1 tree2
        let updatedMap = IntMap.insertWith (++) newNodeValue [newNode] treeMap
        let mapWithOldKey = IntMap.insertWith (++) key otherTrees updatedMap
        getTreeFromFrequencyMap mapWithOldKey


-- Tests
testIsConsistent :: Assertion
testIsConsistent =
    do  assertEqual "consistent" True (isConsistent exampleTree)
        assertEqual "inconsistent_1" False (isConsistent inconsistentTree_1)
        assertEqual "inconsistent_2" False (isConsistent inconsistentTree_2)
        assertEqual "inconsistent_3" False (isConsistent inconsistentTree_3)
        assertEqual "inconsistent_4" False (isConsistent inconsistentTree_4)


testCodingTable :: Assertion
testCodingTable =
    do  assertEqual "correct Table 1" codingTableExampleTree (toCodingTable exampleTree)

testEncode :: Assertion
testEncode =
    do  assertEqual "correct encoding ENTE" [Zero,Zero,Zero,One,Zero,One,One,Zero,Zero,Zero] (encode codingTableExampleTree "ENTE")
        assertEqual "encode empty String" [] (encode codingTableExampleTree "")
        --assertEqual "error on invalid input" (encode codingTableExampleTree "ENTO") -- needs refactoring

testDecode :: Assertion
testDecode =
    do  assertEqual "Correct decoding ENTE" "ENTE" (decode exampleTree [Zero,Zero,Zero,One,Zero,One,One,Zero,Zero,Zero])
        assertEqual "decode empty array" "" (decode exampleTree [])

{--allTests :: Assertion
allTests =
    do testIsConsistent --}

allTests :: Test
allTests = TestList [TestCase testIsConsistent, TestCase testCodingTable, TestCase testEncode, TestCase testDecode]
