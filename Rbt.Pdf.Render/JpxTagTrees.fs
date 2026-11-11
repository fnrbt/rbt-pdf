// Copyright 2012 Mozilla Foundation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
//
// This file is a faithful F# port of TagTree and InclusionTree from
// pdf.js v3.11.174 src/core/jpx.js (lines 1598-1741).
namespace Rbt.Pdf.Render

open System

/// A single level in a JPEG 2000 tag tree.
type JpxTagTreeLevel internal (width: int, height: int, items: int array) =
    member _.Width = width
    member _.Height = height
    member _.Items = items
    member val Index = 0 with get, set

/// JPEG 2000 tag tree, with lazily materialized node values.
type TagTree(width: int, height: int) =
    do
        if width <= 0 || height <= 0 then invalidArg "width" "Tag tree dimensions must be positive."
        if int64 width * int64 height > 64000000L then invalidArg "width" "Tag tree dimensions exceed the supported allocation size."
    let levels = ResizeArray<JpxTagTreeLevel>()
    do
        let mutable w, h, levelCount = width, height, 1
        let mutable largest = max width height
        while largest > 1 do
            largest <- largest / 2 + largest % 2
            levelCount <- levelCount + 1
        let mutable totalCount = 0L
        let mutable sizeW, sizeH = width, height
        for _ = 1 to levelCount do
            totalCount <- totalCount + int64 sizeW * int64 sizeH
            sizeW <- sizeW / 2 + sizeW % 2
            sizeH <- sizeH / 2 + sizeH % 2
        if totalCount > 64000000L then
            invalidArg "width" "Tag tree dimensions exceed the supported allocation size."
        for _ = 1 to levelCount do
            let count = int (int64 w * int64 h)
            levels.Add(JpxTagTreeLevel(w, h, Array.create count -1))
            w <- w / 2 + w % 2
            h <- h / 2 + h % 2
    let mutable currentLevel = 0
    let mutable resultValue: int option = None
    member _.Levels = levels :> seq<JpxTagTreeLevel>
    member _.Value = resultValue
    member _.Reset(i: int, j: int) =
        if i < 0 || j < 0 || i >= width || j >= height then invalidArg "i" "Tag tree coordinate is out of bounds."
        let mutable x, y, levelIndex, value, found = i, j, 0, 0, false
        while levelIndex < levels.Count && not found do
            let level = levels[levelIndex]
            let index = x + y * level.Width
            if level.Items[index] >= 0 then
                value <- level.Items[index]
                found <- true
            else
                level.Index <- index
                x <- x >>> 1
                y <- y >>> 1
                levelIndex <- levelIndex + 1
        levelIndex <- levelIndex - 1
        let level = levels[levelIndex]
        level.Items[level.Index] <- value
        currentLevel <- levelIndex
        resultValue <- None
    member _.IncrementValue() =
        let level = levels[currentLevel]
        level.Items[level.Index] <- level.Items[level.Index] + 1
    member _.NextLevel() =
        let level = levels[currentLevel]
        let value = level.Items[level.Index]
        currentLevel <- currentLevel - 1
        if currentLevel < 0 then
            resultValue <- Some value
            false
        else
            let parent = levels[currentLevel]
            parent.Items[parent.Index] <- value
            true

/// JPEG 2000 inclusion tree, storing values in unsigned-byte form.
type JpxInclusionTreeLevel internal (width: int, height: int, items: byte array) =
    member _.Width = width
    member _.Height = height
    member _.Items = items
    member val Index = 0 with get, set

type InclusionTree(width: int, height: int, defaultValue: int) =
    do
        if width <= 0 || height <= 0 then invalidArg "width" "Inclusion tree dimensions must be positive."
        if defaultValue < 0 || defaultValue > 255 then invalidArg "defaultValue" "Default value must fit in a byte."
        if int64 width * int64 height > 64000000L then invalidArg "width" "Inclusion tree dimensions exceed the supported allocation size."
    let levels = ResizeArray<JpxInclusionTreeLevel>()
    do
        let mutable w, h, levelCount = width, height, 1
        let mutable largest = max width height
        while largest > 1 do
            largest <- largest / 2 + largest % 2
            levelCount <- levelCount + 1
        let mutable totalCount = 0L
        let mutable sizeW, sizeH = width, height
        for _ = 1 to levelCount do
            totalCount <- totalCount + int64 sizeW * int64 sizeH
            sizeW <- sizeW / 2 + sizeW % 2
            sizeH <- sizeH / 2 + sizeH % 2
        if totalCount > 64000000L then
            invalidArg "width" "Inclusion tree dimensions exceed the supported allocation size."
        let mutable w, h = width, height
        for _ = 1 to levelCount do
            let count = int (int64 w * int64 h)
            let items = Array.create count (byte defaultValue)
            levels.Add(JpxInclusionTreeLevel(w, h, items))
            w <- w / 2 + w % 2
            h <- h / 2 + h % 2
    let mutable currentLevel = 0
    member _.Levels = levels :> seq<JpxInclusionTreeLevel>
    member _.Reset(i: int, j: int, stopValue: int) =
        if i < 0 || j < 0 || i >= width || j >= height then invalidArg "i" "Inclusion tree coordinate is out of bounds."
        let mutable x, y, levelIndex, doneLoop, result = i, j, 0, false, true
        while levelIndex < levels.Count && not doneLoop do
            let level = levels[levelIndex]
            let index = x + y * level.Width
            level.Index <- index
            let value = int level.Items[index]
            if value = 255 then doneLoop <- true
            elif value > stopValue then
                currentLevel <- levelIndex
                let currentValue = level.Items[level.Index]
                for k = levelIndex - 1 downto 0 do
                    let parent = levels[k]
                    parent.Items[parent.Index] <- currentValue
                result <- false
                doneLoop <- true
            else
                x <- x >>> 1
                y <- y >>> 1
                levelIndex <- levelIndex + 1
        if result then currentLevel <- levelIndex - 1
        result
    member _.IncrementValue(stopValue: int) =
        if stopValue < 0 || stopValue >= 255 then invalidArg "stopValue" "Stop value must be between 0 and 254."
        let level = levels[currentLevel]
        level.Items[level.Index] <- byte (stopValue + 1)
        let value = level.Items[level.Index]
        for k = currentLevel - 1 downto 0 do
            let parent = levels[k]
            parent.Items[parent.Index] <- value
    member _.NextLevel() =
        let level = levels[currentLevel]
        let value = level.Items[level.Index]
        level.Items[level.Index] <- 255uy
        currentLevel <- currentLevel - 1
        if currentLevel < 0 then false
        else
            let parent = levels[currentLevel]
            parent.Items[parent.Index] <- value
            true
