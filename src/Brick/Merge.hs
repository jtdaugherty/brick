module Brick.Merge
  ( maintainSel
  )
where

import Data.Algorithm.Diff

-- Assuming `xs` is an existing list that we want to update to match the state
-- of `ys`. Given a selected index in `xs`, the goal is to compute the
-- corresponding index in `ys`.
maintainSel :: Eq e => [e] -> [e] -> Int -> Int
maintainSel xs ys sel = let hunks = getDiff xs ys
                        in merge 0 sel hunks

merge :: Eq e => Int -> Int -> [Diff e] -> Int
merge _   sel []                 = sel
merge idx sel (h:hs) | idx > sel = sel
                     | otherwise = case h of
    Both _ _ -> merge sel (idx+1) hs

    -- element removed in new list
    First _  -> let newSel = if idx < sel
                             then sel - 1
                             else sel
                in merge newSel idx hs

    -- element added in new list
    Second _ -> let newSel = if idx <= sel
                             then sel + 1
                             else sel
                in merge newSel (idx+1) hs
