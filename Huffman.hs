import           Data.Binary
import           Data.Bits
import Data.ByteString (pack, unpack)
import qualified Data.ByteString as ByteString
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
import           System.IO
import           Test.HUnit

type ByteString = ByteString.ByteString

-- Assignement 2 - data type
data HTree = HLeaf Char Int
                | HNode Int HTree HTree
    deriving (Show, Eq, Ord)

type Map = Map.Map
data Bit = Zero | One
    deriving (Eq, Show, Ord)

type CodingTable = Map Char [Bit]

type Worklist = IntMap.IntMap [HTree]

type CodingTableAsListWithInts = [(Char, [Int])]

data FileContent = Elements {codingTable :: CodingTableAsListWithInts, encodedString :: ByteString }
    deriving (Show)

instance Binary FileContent where
    put (Elements codingTable encodedString) = do
        put codingTable
        put encodedString

    get = do
        codingTable <- get
        encodedString <- get
        return $ Elements codingTable encodedString

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
encodeString :: CodingTable -> String -> [Bit]
encodeString table [] = []
encodeString table (x:xs) = case (Map.lookup x table) of
                        Nothing -> error "String cannot be encoded with the provided CodingTable"
                        Just x -> x ++ (encodeString table xs)


-- Assignment 6
decodeBitSequence :: HTree -> [Bit] -> String
decodeBitSequence _ [] = ""
decodeBitSequence (HLeaf _ _ ) _ = error "Huffman tree only consist of one leaf"
decodeBitSequence rootTree array = reverse(decodeWithString rootTree array "")
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
buildHTree []     = error "Cannot derive a Huffman tree from one letter"
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

-- Assignement 8
toDecodeTree :: CodingTable -> HTree
toDecodeTree codingTable = buildSubTree [] (reverseCodingTable codingTable)

reverseCodingTable :: CodingTable -> Map [Bit] Char
--reverseCodingTable codingTable = Map.fromList (map (\(letter, bits) -> (bits, letter)) (Map.toList codingTable))
reverseCodingTable codingTable = Map.fromList tupleList
    where tupleList = [(bits, letter) | (letter, bits) <- Map.toList codingTable]

-- build tree and bit sequence and just look if there is a character for the current sequence available in the codingTable
buildSubTree :: [Bit] -> Map [Bit] Char -> HTree
buildSubTree bits reversedCodingTable = do
    let maybeCharacter = Map.lookup bits reversedCodingTable
    case maybeCharacter of
        Just character -> HLeaf character 0
        Nothing -> HNode 0 (buildSubTree (bits ++ [Zero]) reversedCodingTable) (buildSubTree (bits ++ [One]) reversedCodingTable)

-- Assignment 9
encodeFileCustom :: FilePath -> IO ()
encodeFileCustom pathToFile = do
        fileContents <- readFile pathToFile
        let constructedTree = buildHTree fileContents
        let constructedCodingTable = toCodingTable constructedTree
        let encodedFileContent = encodeString constructedCodingTable fileContents
        let encodedFileContentBools = bitToBool encodedFileContent
        let codingTableWithInts = changeBitToInt constructedCodingTable
        --let encodedTable = encodeList codingTableWithInts--map encodeTuple codingTableWithInts
        let contentToEncode = Elements codingTableWithInts (pack (bitsToWord8 encodedFileContent))
        encodeFile "file.comp" contentToEncode
        --BL.writeFile "encodedTable.comp" $ P.runPut encodedTable
        --print encodedFileContent

changeBitToInt :: CodingTable -> [(Char, [Int])]
changeBitToInt codingTable = do
        let tableAsList = Map.toList codingTable
        map replaceBitArrayWithIntArray tableAsList

changeIntToBit :: [(Char, [Int])] -> CodingTable
changeIntToBit tableAsIntList = do
        let tableAsList = map replaceIntArrayWithBitArray tableAsIntList
        Map.fromList tableAsList

groupBitsIn8 :: Int -> [Bit] -> [[Bit]]
groupBitsIn8 _ []    = []
groupBitsIn8 count array 
    | count > 0 = (take count array) : (groupBitsIn8 8 (drop count array))
    | otherwise = error "Negative count"

bitsToWord8 :: [Bit] -> [Word8]
bitsToWord8 bits = map createWord8 (groupBitsIn8 8 bits)
    where
    createWord8 :: [Bit] -> Word8
    createWord8 b = z (b!!0) 128 .|. z (b!!1) 64 .|. z (b!!2) 32 .|. z (b!!3) 16 .|. z (b!!4) 8 .|. z (b!!5) 4 .|. z (b!!6) 2 .|. z (b!!7) 1
        where z Zero _ = 0
              z One  n = n

word8ToBits :: [Word8] -> [Bit]
word8ToBits words = concat (map dissectWord8 words)
    where
        dissectWord8 :: Word8 -> [Bit]
        dissectWord8 word = [b 128, b 64, b 32, b 16, b 8, b 4, b 2, b 1]
            where
                b z = if (word .&. z == 0) then Zero else One

{--encodeList [] = return ()
encodeList (x:xs) = do
        let tuple = head (x:xs)
        encodeTuple tuple
        encodeList xs--}

{--encodeTuple :: (Char, [Int]) -> B.Put
encodeTuple (char, bits) = do
    P.putCharUtf8 char
    B.put bits--}

--packBoolToWord8 (b1:b2:b3:b4:b5:b6:b7:b8:remainingBits) = [BW.packWord8BE b1 b2 b3 b4 b5 b6 b7 b8] ++ packBoolToWord8 remainingBits

bitToBool :: [Bit] -> [Bool]
bitToBool [] = []
bitToBool (bit:remainingBits) =
    if bit == Zero
        then [False] ++ bitToBool remainingBits
        else [True] ++ bitToBool remainingBits


--decodeFileCustom :: FilePath -> IO ()
decodeFileCustom pathToFile = do
    readData <- decodeFile pathToFile
    let decodedFileContent = readData :: FileContent
    let codingTableFromFile = changeIntToBit (codingTable decodedFileContent)
    let encodedStringFromFile = word8ToBits (unpack (encodedString decodedFileContent))
    let usedTree = toDecodeTree codingTableFromFile
    let originalString = decodeBitSequence usedTree encodedStringFromFile
    writeFile "file.txt" originalString

replaceBitArrayWithIntArray :: (Char, [Bit]) -> (Char, [Int])
replaceBitArrayWithIntArray (character, bitArray) = (character, bitToInt bitArray)

replaceIntArrayWithBitArray :: (Char, [Int]) -> (Char, [Bit])
replaceIntArrayWithBitArray (character, intArray) = (character, intToBit intArray)

bitToInt :: [Bit] -> [Int]
bitToInt [] = []
bitToInt (bit:remainingBits) =
    if bit == Zero
        then [0] ++ bitToInt remainingBits
        else [1] ++ bitToInt remainingBits

intToBit :: [Int] -> [Bit]
intToBit [] = []
intToBit (int:remainingInts) =
    if int == 0
        then [Zero] ++ intToBit remainingInts
        else [One] ++ intToBit remainingInts


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
    do  assertEqual "correct encoding ENTE" [Zero,Zero,Zero,One,Zero,One,One,Zero,Zero,Zero] (encodeString codingTableExampleTree "ENTE")
        assertEqual "encode empty String" [] (encodeString codingTableExampleTree "")
        --assertEqual "error on invalid input" (encodeString codingTableExampleTree "ENTO") -- needs refactoring

testDecode :: Assertion
testDecode =
    do  assertEqual "Correct decoding ENTE" "ENTE" (decodeBitSequence exampleTree [Zero,Zero,Zero,One,Zero,One,One,Zero,Zero,Zero])
        assertEqual "decode empty array" "" (decodeBitSequence exampleTree [])

{--allTests :: Assertion
allTests =
    do testIsConsistent --}

allTests :: Test
allTests = TestList [TestCase testIsConsistent, TestCase testCodingTable, TestCase testEncode, TestCase testDecode]
