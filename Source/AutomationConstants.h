// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2024
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#pragma once


static const int kActionsCount  = 40;
static const int kWipeCount     = 8;
static const int kWipeOddCount  = 4;
static const int kWipeOddWCount = 6;
static const int kWipeOddHCount = 6;
static const int kRevealCount   = 6;

static const std::wstring kAutomationActions[kActionsCount] = {
	L"Mirror", L"Flip", L"Invert",
	L"Scroll left", L"Scroll right", L"Scroll up", L"Scroll down",
	L"Rotate Left", L"Rotate Right",
	L"Wipe (Vertical)", L"Wipe (Vertical) Clear",
	L"Wipe (Horizontal)", L"Wipe (Horizontal) Clear",
	L"Jiggle Left", L"Jiggle Right",
	L"Jiggle Up", L"Jiggle Down",
	L"Bounce left/right", L"Bounce up/down",
	L"Brush #1 every frame", L"Brush #1 first frame",
	L"Brush #2 every frame", L"Brush #2 first frame",
	L"Scroll left/right split", L"Scroll right/left split",
	L"Scroll up/downt split", L"Scroll downt/up split",
	L"Colour cycling (linear)", L"Colour cycling (bounce)",
	L"Alternate up/down scroll",
	L"Reveal left/right", L"Reveal right/left", L"Reveal top/bottom", L"Reveal bottom/top", L"Reveal centre in", L"Reveal centre out",
	L"Wipe left", L"Wipe right", L"Wipe up", L"Wipe down"
	};

static const int kActionsWipe[kWipeCount] = { 9, 10, 11, 12, 36, 37, 38, 39 };
static const int kActionsWipeOdd[kWipeOddCount] = { 36, 37, 38, 39 };
static const int kActionsWipeOddW[kWipeOddWCount] = { 11, 12, 36, 37, 38, 39 };
static const int kActionsWipeOddH[kWipeOddHCount] = { 9, 10, 36, 37, 38, 39 };

static const int kActionsReveal[kRevealCount] = { 30, 31, 32, 33, 34, 35 };
