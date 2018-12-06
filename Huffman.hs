import Test.HUnit
import qualified Data.Map.Strict as Map

-- Assignement 2 - data type
data HTree = HLeaf Char Int 
                | HNode Int HTree HTree  
    deriving (Show)

type Map = Map.Map
data Bit = Zero | One
    deriving (Eq, Show)
type CodingTable = Map Char [Bit]

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
getValueOfNode (HLeaf _ val) = val
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

-- Assignement 6
decode :: HTree -> [Bit] -> String
decode 

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


{--allTests :: Assertion
allTests = 
    do testIsConsistent --}

allTests :: Test
allTests = TestList [TestCase testIsConsistent, TestCase testCodingTable, TestCase testEncode]