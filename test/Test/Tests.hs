module Test.Tests (testTree) where

import Control.Applicative((<$>))
import Data.Bifunctor
import Data.List (nub, sort)
import qualified Data.Set as S
import Prelude hiding (null, lookup, filter,map)
import Data.Tuple
import qualified Prelude as P
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Bimap


testTree :: TestTree
testTree = testGroup "Bimap tests"
    [ case_size_empty
    , case_null_empty
    , testProperty "arbitrary valid" (prop_valid @Int @Integer)
    , testGroup "Creation"
        [ testProperty "singleton" (prop_singleton @Int @Integer)
        , testProperty "fromList account" (prop_fromList_account @Int @Integer)
        , testProperty "fromList size" (prop_fromList_size @Int @Integer)
        , testProperty "fromAscPairList reconstitute" (prop_fromAscPairList_reconstitute @Int @Integer)
        , testProperty "fromAscPairList check" (prop_fromAscPairList_check @Int @Integer)
        ]
    , testProperty "pair member" (prop_pairMember @Int @Integer)
    , testGroup "Isos"
        [ testProperty "twist twist" (prop_twist_twist @Int @Integer)
        , testProperty "fromList toList" (prop_fromList_toList @Int @Integer)
        , testProperty "fromList fromAList" (prop_fromList_fromAList @Int)
        ]
    , testGroup "Insert"
        [ testProperty "insert member" (prop_insert_member @Int @Integer)
        , testProperty "try insert member" (prop_tryInsert_member @Int @Integer)
        , testProperty "try insert doesn't clobber" (prop_tryInsert_not_clobber @Int @Integer)
        , testProperty "clobber left" (prop_clobberL @Int @Integer)
        , testProperty "clobber right" (prop_clobberR @Int @Integer)
        ]
    , testProperty "member twin" (prop_member_twin @Int @Integer)
    , testProperty "delete" (prop_delete @Int @Integer)
    , testProperty "adjust ~ fmap" (prop_adjust_fmap @Int @Integer)
    , testProperty "adjustR ~ fmap" (prop_adjustR_fmap @Int @Integer)
    , testGroup "Minimums"
        [ testProperty "deleteMin is valid" (prop_deleteMin_is_valid @Int @Integer)
        , testProperty "deleteFindMin is valid" (prop_deleteFindMin_is_valid @Int @Integer)
        , testProperty "deleteMinR is valid" (prop_deleteMinR_is_valid @Int @Integer)
        , testProperty "deleteFindMinR is valid" (prop_deleteFindMinR_is_valid @Int @Integer)
        , testProperty "deleteFindMin ~ deleteMin" (prop_deleteMin_is_delete @Int @Integer)
        , testProperty "deleteFindMinR ~ deleteMinR" (prop_deleteMinR_is_delete @Int @Integer)
        , testProperty "deleteFindMin ~ findMin" (prop_deleteMin_is_find @Int @Integer)
        , testProperty "deleteFindMinR ~ findMinR" (prop_deleteMinR_is_find @Int @Integer)
        , testProperty "deleteFindMin deletes" (prop_deleteMin_deletes @Int @Integer)
        , testProperty "deleteFindMinR deletes" (prop_deleteMinR_deletes @Int @Integer)
        , testProperty "findMin member" (prop_findMin_member @Int @Integer)
        , testProperty "findMinR member" (prop_findMinR_member @Int @Integer)
        , testProperty "singleton is findMin" (prop_singleton_is_findMin @Int @Integer)
        , testProperty "singleton is findMinR" (prop_singleton_is_findMinR @Int @Integer)
        , testProperty "singleton deleteMin is empty" (prop_singleton_deleteMin_empty @Int @Integer)
        , testProperty "singleton deleteMinR is empty" (prop_singleton_deleteMinR_empty @Int @Integer)
        , testProperty "findMin is minimal" (prop_findMin_is_minimal @Int @Integer)
        , testProperty "findMinR is minimal" (prop_findMinR_is_minimal @Int @Integer)
        ]
    , testGroup "Maximums"
        [ testProperty "deleteMax is valid" (prop_deleteMax_is_valid @Int @Integer)
        , testProperty "deleteFindMax is valid" (prop_deleteFindMax_is_valid @Int @Integer)
        , testProperty "deleteMaxR is valid" (prop_deleteMaxR_is_valid @Int @Integer)
        , testProperty "deleteFindMaxR is valid" (prop_deleteFindMaxR_is_valid @Int @Integer)
        , testProperty "deleteFindMax ~ deleteMax" (prop_deleteMax_is_delete @Int @Integer)
        , testProperty "deleteFindMaxR ~ deleteMaxR" (prop_deleteMaxR_is_delete @Int @Integer)
        , testProperty "deleteFindMax ~ findMax" (prop_deleteMax_is_find @Int @Integer)
        , testProperty "deleteFindMaxR ~ findMaxR" (prop_deleteMaxR_is_find @Int @Integer)
        , testProperty "deleteFindMax deletes" (prop_deleteMax_deletes @Int @Integer)
        , testProperty "deleteFindMaxR deletes" (prop_deleteMaxR_deletes @Int @Integer)
        , testProperty "findMax member" (prop_findMax_member @Int @Integer)
        , testProperty "findMaxR member" (prop_findMaxR_member @Int @Integer)
        , testProperty "singleton is findMax" (prop_singleton_is_findMax @Int @Integer)
        , testProperty "singleton is findMaxR" (prop_singleton_is_findMaxR @Int @Integer)
        , testProperty "singleton deleteMax is empty" (prop_singleton_deleteMax_empty @Int @Integer)
        , testProperty "singleton deleteMaxR is empty" (prop_singleton_deleteMaxR_empty @Int @Integer)
        , testProperty "findMax is maximal" (prop_findMax_is_maximal @Int @Integer)
        , testProperty "findMaxR is maximal" (prop_findMaxR_is_maximal @Int @Integer)
        ]
    , testProperty "filter true" (prop_filter_true @Int @Integer)
    , testProperty "filter false" (prop_filter_false @Int @Integer)
    , testGroup "Preserve" 
        [ testProperty "preserve keys" (prop_map_preserve_keys @Double @Integer)
        , testProperty "preserve keysR" (prop_map_preserve_keysR @Int @Double)
        , testProperty "preserve lookup" (prop_map_preserve_lookup @Int @Integer (1 -))
        , testProperty "preserve lookupR" (prop_map_preserve_lookupR @Int @Integer (1 -))
        , testProperty "preserve monotonic keys" (prop_mapMonotonic_preserve_keys @Int @Integer (2 *))
        , testProperty "preserve monotonic lookup" (prop_mapMonotonic_preserve_lookup @Int @Integer (3 *))
        , testProperty "preserve monotonic right keys" (prop_mapMonotonic_preserve_right_keys @Int @Integer (^ 2))
        , testProperty "preserve monotonic lookupR" (prop_mapMonotonic_preserve_lookupR @Int @Integer (^ 3))
        ]
    , testGroup "Partition"
        [ testProperty "agree" (prop_partition_agree @Int @Integer)
        , testProperty "disjunction" (prop_partition_disjoint @Int @Integer)
        , testProperty "union" (prop_partition_union @Int @Integer)
        , testProperty "filter" (prop_partition_filter @Int @Integer)
        , testProperty "filter valid" (prop_partition_filter_valid @Int @Integer)
        ]
    ]


(.:) = (.).(.)

instance (Ord a, Arbitrary a, Ord b, Arbitrary b) => Arbitrary (Bimap a b) where
    arbitrary = fromList <$> arbitrary

instance (Ord a, CoArbitrary a, Ord b, CoArbitrary b) => CoArbitrary (Bimap a b)

-- generator for filter/partition classification functions
data FilterFunc a b = FilterFunc String (a -> b -> Bool)
instance Show (FilterFunc a b) where
    show (FilterFunc desc _) = desc
instance (Integral a, Arbitrary a, Integral b, Arbitrary b)
    => Arbitrary (FilterFunc a b) where
    arbitrary = do
        pivot <- (arbitrary :: Gen Integer)
        return $ FilterFunc
            ("(\\x y -> x - y < " ++ show pivot ++ ")")
            (\x y -> fromIntegral x - fromIntegral y < pivot)
instance (Integral a, CoArbitrary a, Integral b, CoArbitrary b)
    => CoArbitrary (FilterFunc a b) where
    coarbitrary _ gen = do
        x <- arbitrary
        coarbitrary (x :: Int) gen


case_size_empty = testCase "empty bimap has zero size" $
    size empty @?= 0

case_null_empty = testCase "empty bimap is null" $
    null empty @? "null empty"

-- when converting from a list and back, each pair in the latter
-- list was originally in the former list
-- (heh, this is probably made redundant by polymorphism)
prop_fromList_toList :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => [(a, b)] -> Property
prop_fromList_toList xs = do
    let xs' = toList . fromList $ xs
    conjoin $ (`elem` xs) <$> xs'

-- when converting a list to a bimap, each list element either
-- ends up in the bimap, or could conceivably have been clobbered
prop_fromList_account :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => [(a, b)] -> Property
prop_fromList_account xs = conjoin $ (\x -> isMember x .||. notUnique x) <$> xs
    where
    bi = fromList xs
    isMember x = x `pairMember` bi
    notUnique (x, y) =
        ((>1) . length . P.filter (== x) . P.map fst $ xs) ||
        ((>1) . length . P.filter (== y) . P.map snd $ xs)

-- a bimap created from a list is no larger than the list
prop_fromList_size :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => [(a, b)] -> Property
prop_fromList_size xs = do
    let bm = fromList xs
    classify (length xs > 1) "non-trivial" $
        classify (size bm < length xs) "shrunk" $
            size (fromList xs) <= length xs

-- a monotone bimap can be reconstituted via fromAscPairList
prop_fromAscPairList_reconstitute :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => [(a, b)] -> Property
prop_fromAscPairList_reconstitute xs = valid bi' .&&. (bi == bi')
    where
    xs' = zip (sort $ P.map fst xs) (sort $ P.map snd xs)
    bi = fromList xs'
    bi' = fromAscPairList . toAscList $ bi

-- fromAscPairList will never produce an invalid bimap
prop_fromAscPairList_check :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => [(a, b)] -> Bool
prop_fromAscPairList_check xs = valid bi
    where
    xs' = zip (nub $ sort $ P.map fst xs) (nub $ sort $ P.map snd xs)
    bi = fromAscPairList xs'

-- if a pair is a member of the bimap, then both elements are present
-- and associated with each other
prop_pairMember :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> a -> b -> Property
prop_pairMember bi k v = do
    conjoin $ (\(k, v) ->
        k `member`  bi .&&. v `memberR` bi .&&.
        lookup  k bi === Just v .&&. lookupR v bi === Just k)
        <$> toList bi

-- an inserted pair ends up in the bimap
prop_insert_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> a -> b -> Property
prop_insert_member bi k v = do
    let new = (k, v)
    let bi' = insert k v bi
    classify (size bi' > size bi) "added new element" $
        new `pairMember` bi'

-- if we insert a pair with an existing value, the old value's twin
-- is no longer in the bimap
prop_clobberL :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> b -> Property
prop_clobberL bi b' =
    (not . null $ bi) && (b' `notMemberR` bi) ==> do
        let (a, b) = head . toList $ bi
        (a, b) `pairNotMember` insert a b' bi

prop_clobberR :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> a -> Property
prop_clobberR bi a' =
    (not . null $ bi) && (a' `notMember` bi) ==> do
        let (a, b) = head . toList $ bi
        (a, b) `pairNotMember` insert a' b bi

-- if we politely insert two members, neither of which is present,
-- then the two are successfully associated
prop_tryInsert_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> a -> b -> Property
prop_tryInsert_member bi k v =
    (k, v) `neitherMember` bi ==>
        pairMember (k, v) (tryInsert k v bi)
    where
    neitherMember (k, v) bi = k `notMember` bi && v `notMemberR` bi

-- polite insertion will never remove existing associations
prop_tryInsert_not_clobber :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> a -> b -> Property
prop_tryInsert_not_clobber bi k v = do
    let bi' = tryInsert k v bi
    conjoin $ flip pairMember bi' <$> toList bi

-- an arbitrary bimap is valid
prop_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> Bool
prop_valid = valid

-- if x maps to y, then y maps to x
prop_member_twin :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> Property
prop_member_twin bi =
    conjoin $ (\(x, y) ->
        ((bi !  x) `memberR` bi) .&&.
        ((bi !> y) `member`  bi))
    <$> toList bi

-- deleting an element removes it and its twin from the map
prop_delete :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> Property
prop_delete bi =
    conjoin $ (\(x, y) ->
        (x `notMember`  delete  x bi) .&&.
        (y `notMemberR` deleteR y bi) .&&.
        ((bi !  x) `notMemberR` delete  x bi) .&&.
        ((bi !> y) `notMember`  deleteR y bi))
    <$> toList bi

-- adjust and fmap are similar
prop_adjust_fmap :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show b) => Fun b b -> Bimap a b -> a -> Property
prop_adjust_fmap (Fn f) bi a = do
    let adjust' = lookup @_ @_ @Maybe a $ adjust f a bi
    let fmap' = f <$> lookup @_ @_ @Maybe a bi
    adjust' === fmap'

prop_adjustR_fmap :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a) => Fun a a -> Bimap a b -> b -> Property
prop_adjustR_fmap (Fn f) bi b = do
    let adjust' = lookupR @_ @_ @Maybe b $ adjustR f b bi
    let fmap' = f <$> lookupR @_ @_ @Maybe b bi
    adjust' === fmap'

-- a singleton bimap is valid, has one association, and the two
-- given values map to each other
prop_singleton :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => a -> b -> Property
prop_singleton x y = do
    let bi = singleton x y
    conjoin
        [ valid bi
        , (x, y) `pairMember` bi
        , (bi !  x) == y
        , (bi !> y) == x
        , size bi == 1
        ]

-- an always-true filter makes no changes
prop_filter_true :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_filter_true bi = bi === filter (curry $ const True) bi

-- an always-false filter gives an empty result
prop_filter_false :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Bool
prop_filter_false = null . filter (curry $ const False)

-- all elements of the projection satisfy the predicate, and all
-- elements of the rejection do not
prop_partition_agree :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> Fun (a, b) Bool -> Property
prop_partition_agree bi (Fn2 ff) = do
    let (projection, rejection) = partition ff bi
    conjoin (uncurry ff <$> toList projection)
        .&&. conjoin (not . uncurry ff <$> toList rejection)


-- the two halves of a partition are disjoint
prop_partition_disjoint :: (Ord a, Ord b, Arbitrary a, Arbitrary b) => Bimap a b -> Fun (a, b) Bool -> Property
prop_partition_disjoint bi (Fn2 ff) = do
    let (projection, rejection) = bimap asSet asSet $ partition ff bi
    property . S.null $ S.intersection projection rejection
    where
    asSet = S.fromList . toList

-- the two halves of a partition contain the elements of the original
-- bimap
prop_partition_union :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Fun (a, b) Bool -> Property
prop_partition_union bi (Fn2 ff) = do
    let (projection, rejection) = bimap asSet asSet $ partition ff bi
    asSet bi === S.union projection rejection
    where
    asSet = S.fromList . toList

-- the two halves of a partition agree with individual filters
prop_partition_filter :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Fun (a, b) Bool -> Property
prop_partition_filter bi (Fn2 ff) = do
    let (projection, rejection) = partition ff bi
    projection === filter ff bi .&&.
        rejection === filter (not .: ff) bi

-- partition and filter produce valid results
prop_partition_filter_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Fun (a, b) Bool -> Property
prop_partition_filter_valid bi (Fn2 ff) = do
    let (projection, rejection) = partition ff bi
    conjoin $ valid <$>
        [ projection
        , rejection
        , filter ff bi
        , filter (not .: ff) bi
        ]

-- twist is its own inverse
prop_twist_twist :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_twist_twist bi = bi === (twist . twist $ bi)

-- the property (fromList == fromAList . reverse) only holds
-- if either the left or right values are all distinct
prop_fromList_fromAList :: (Ord a, Arbitrary a, Show a) => [a] -> Property
prop_fromList_fromAList xs = do
    let ys = xs `zip` [1..]
    let rys = reverse ys
    fromList  ys === fromAList rys .&&.
        fromList rys === fromAList  ys

-- deleteFindMin and deleteMin agree
prop_deleteMin_is_delete :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMin_is_delete bi = do
    not (null bi) ==>
        snd (deleteFindMin bi) === deleteMin bi

-- deleteFindMin and findMin agree
prop_deleteMin_is_find :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMin_is_find bi = do
    not (null bi) ==>
        fst (deleteFindMin bi) == findMin bi

-- elements removed by deleteFindMin are no longer in the bimap
prop_deleteMin_deletes :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMin_deletes bi =
    not (null bi) ==> do
        let (deleted, bi') = deleteFindMin bi
        deleted `pairNotMember` bi'

-- findMin finds a member of the map
prop_findMin_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMin_member bi = do
    not (null bi) ==>
        findMin bi `pairMember` bi

-- the minimum of a singleton bimap is its contents
prop_singleton_is_findMin :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_is_findMin x y = do
    let bi = singleton x y
    findMin bi === (x, y)

-- deleting the minimum of a singleton leaves it empty
prop_singleton_deleteMin_empty :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_deleteMin_empty x y = do
    let bi = singleton x y
    property . null . deleteMin $ bi

-- the minimum of a bimap is <= all other elements
prop_findMin_is_minimal :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMin_is_minimal bi = do
    let m = fst . findMin $ bi
    conjoin $ (>= m) . fst <$> toList bi

-- deleteFindMinR and deleteMinR agree
prop_deleteMinR_is_delete :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMinR_is_delete bi = do
    not (null bi) ==>
        snd (deleteFindMinR bi) === deleteMinR bi

-- deleteFindMinR and findMinR agree
prop_deleteMinR_is_find :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMinR_is_find bi = do
    not (null bi) ==>
        fst (deleteFindMinR bi) == findMinR bi

-- elements removed by deleteFindMinR are no longer in the bimap
prop_deleteMinR_deletes :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMinR_deletes bi =
    not (null bi) ==> do
        let (deleted, bi') = deleteFindMinR bi
        swap deleted `pairNotMember` bi'

-- findMinR finds a member of the map
prop_findMinR_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMinR_member bi = do
    not (null bi) ==>
        swap (findMinR bi) `pairMember` bi

-- the minimum of a singleton bimap is its contents
prop_singleton_is_findMinR :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_is_findMinR x y = do
    let bi = singleton x y
    findMinR bi === (y, x)

-- deleting the minimum of a singleton leaves it empty
prop_singleton_deleteMinR_empty :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_deleteMinR_empty x y = do
    let bi = singleton x y
    property . null . deleteMinR $ bi

-- the minimum of a bimap is <= all other elements
prop_findMinR_is_minimal :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMinR_is_minimal bi = do
    let m = fst . findMinR $ bi
    conjoin $ (>= m) . snd <$> toList bi

----------
-- deleteFindMax and deleteMax agree
prop_deleteMax_is_delete :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMax_is_delete bi = do
    not (null bi) ==>
        snd (deleteFindMax bi) === deleteMax bi

-- deleteFindMax and findMax agree
prop_deleteMax_is_find :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMax_is_find bi = do
    not (null bi) ==>
        fst (deleteFindMax bi) == findMax bi

-- elements removed by deleteFindMax are no longer in the bimap
prop_deleteMax_deletes :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMax_deletes bi =
    not (null bi) ==> do
        let (deleted, bi') = deleteFindMax bi
        deleted `pairNotMember` bi'

-- findMax finds a member of the map
prop_findMax_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMax_member bi = do
    not (null bi) ==>
        findMax bi `pairMember` bi

prop_singleton_is_findMax :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_is_findMax x y = do
    let bi = singleton x y
    findMax bi === (x, y)

-- deleting the maximum of a singleton leaves it empty
prop_singleton_deleteMax_empty :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_deleteMax_empty x y = do
    let bi = singleton x y
    property . null . deleteMax $ bi

-- the maximum of a bimap is >= all other elements
prop_findMax_is_maximal :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMax_is_maximal bi = do
    let m = fst . findMax $ bi
    conjoin $ (<= m) . fst <$> toList bi

-- deleteFindMaxR and deleteMaxR agree
prop_deleteMaxR_is_delete :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMaxR_is_delete bi = do
    not (null bi) ==>
        snd (deleteFindMaxR bi) === deleteMaxR bi

-- deleteFindMaxR and findMaxR agree
prop_deleteMaxR_is_find :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMaxR_is_find bi = do
    not (null bi) ==>
        fst (deleteFindMaxR bi) == findMaxR bi

-- elements removed by deleteFindMaxR are no longer in the bimap
prop_deleteMaxR_deletes :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMaxR_deletes bi =
    not (null bi) ==> do
        let (deleted, bi') = deleteFindMaxR bi
        swap deleted `pairNotMember` bi'

-- findMaxR finds a member of the map
prop_findMaxR_member :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMaxR_member bi = do
    not (null bi) ==>
        swap (findMaxR bi) `pairMember` bi

-- the Maximum of a singleton bimap is its contents
prop_singleton_is_findMaxR :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_is_findMaxR x y = do
    let bi = singleton x y
    findMaxR bi === (y, x)

-- deleting the maximum of a singleton leaves it empty
prop_singleton_deleteMaxR_empty :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => a -> b -> Property
prop_singleton_deleteMaxR_empty x y = do
    let bi = singleton x y
    property . null . deleteMaxR $ bi

-- the maximum of a bimap is <= all other elements
prop_findMaxR_is_maximal :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_findMaxR_is_maximal bi = do
    let m = fst . findMaxR $ bi
    conjoin $ (<= m) . snd <$> toList bi

prop_deleteMin_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMin_is_valid bi =
    not (null bi) ==>
        valid (deleteMin bi)

prop_deleteFindMin_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteFindMin_is_valid bi =
    not (null bi) ==>
        valid . snd $ deleteFindMin bi

prop_deleteMinR_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMinR_is_valid bi =
    not (null bi) ==>
        valid (deleteMin bi)

prop_deleteFindMinR_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteFindMinR_is_valid bi =
    not (null bi) ==>
        valid . snd $ deleteFindMin bi

prop_deleteMax_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMax_is_valid bi =
    not (null bi) ==>
        valid (deleteMax bi)

prop_deleteFindMax_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteFindMax_is_valid bi =
    not (null bi) ==>
        valid . snd $ deleteFindMax bi

prop_deleteMaxR_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteMaxR_is_valid bi =
    not (null bi) ==>
        valid (deleteMaxR bi)

prop_deleteFindMaxR_is_valid :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Bimap a b -> Property
prop_deleteFindMaxR_is_valid bi =
    not (null bi) ==>
        valid . snd $ deleteFindMaxR bi

-- prop_strict_monotonic :: (Ord a, Arbitrary a, Show a) => Fun a a -> a -> a -> Property
-- prop_strict_monotonic (Fn f) x y = (x < y .&&. f x < f y)

prop_map_preserve_keys :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Fun a a -> Bimap a b -> Property
prop_map_preserve_keys (Fn f) bi =
    (Data.List.sort . P.map f . keys  $ bi) === (keys . map f $ bi)

prop_map_preserve_lookup :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => (a -> a) -> Bimap a b -> a -> Property
prop_map_preserve_lookup f bi v =
    (lookup (f v) . map f $ bi) === lookup @_ @_ @Maybe v bi

prop_map_preserve_keysR :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => Fun b b -> Bimap a b -> Property
prop_map_preserve_keysR (Fn f) bi =
    (Data.List.sort . P.map f . keysR  $ bi) === (keysR . mapR f $ bi)

prop_map_preserve_lookupR :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => (b -> b) -> Bimap a b -> b -> Property
prop_map_preserve_lookupR f bi v =
    (lookupR (f v) . mapR f $ bi) === lookupR @_ @_ @Maybe v bi

prop_mapMonotonic_preserve_keys :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => (a -> a) -> Bimap a b -> Property
prop_mapMonotonic_preserve_keys f bi =
    (P.map f $ keys bi) === (keys $ mapMonotonic f bi)

prop_mapMonotonic_preserve_lookup :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b, Eq b) => (a -> a) -> Bimap a b -> a -> Property
prop_mapMonotonic_preserve_lookup f bi v =
    (lookup @_ @_ @Maybe (f v) $ mapMonotonic f bi) === (lookup v bi)

prop_mapMonotonic_preserve_right_keys :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b) => (b -> b) -> Bimap a b -> Property
prop_mapMonotonic_preserve_right_keys f bi =
    (P.map f $ keysR bi) === (keysR $ mapMonotonicR f bi)

prop_mapMonotonic_preserve_lookupR :: (Ord a, Ord b, Arbitrary a, Arbitrary b, Show a, Show b, Eq b) => (b -> b) -> Bimap a b -> a -> Property
prop_mapMonotonic_preserve_lookupR f bi v =
    (lookup @_ @_ @Maybe v $ mapMonotonicR f bi) === (f <$> lookup v bi)
