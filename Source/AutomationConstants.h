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

static const int kAutomationMirror = 0;
static const int kAutomationFlip = 1;
static const int kAutomationInvert = 2;

static const int kAutomationScrollLeft = 3;
static const int kAutomationScrollRight = 4;
static const int kAutomationScrollUp = 5;
static const int kAutomationScrollDown = 6;

static const int kAutomationRotateLeft = 7;
static const int kAutomationRotateRight = 8;

static const int kAutomationWipeVertical = 9;
static const int kAutomationWipeVerticalClear = 10;
static const int kAutomationWipeHorizontal = 11;
static const int kAutomationWipeHorizontalClear = 12;

static const int kAutomationJiggleLeft = 13;
static const int kAutomationJiggleRight = 14;
static const int kAutomationJiggleUp = 15;
static const int kAutomationJiggleDown = 16;

static const int kAutomationBounceLeftRight = 17;
static const int kAutomationBounceUpDown = 18;

static const int kAutomationBrush1EveryFrame = 19;
static const int kAutomationBrush1FirstFrame = 20;
static const int kAutomationBrush2EveryFrame = 21;
static const int kAutomationBrush2FirstFrame = 22;

static const int kAutomationScrollLeftRightSplit = 23;
static const int kAutomationScrollRightLeftSplit = 24;
static const int kAutomationScrollUpDownSplit = 25;
static const int kAutomationScrollDownUpSplit = 26;

static const int kAutomationColourCyclingLinear = 27;
static const int kAutomationColourCyclingBounce = 28;

static const int kAutomationAlternateUpDownScroll = 29;

static const int kAutomationRevealLeftRight = 30;
static const int kAutomationRevealRightLeft = 31;
static const int kAutomationRevealTopBottom = 32;
static const int kAutomationRevealBottomTop = 33;
static const int kAutomationRevealCentreIn = 34;
static const int kAutomationRevealCentreOut = 35;

static const int kAutomationWipeLeft = 36;
static const int kAutomationWipeRight = 37;
static const int kAutomationWipeUp = 38;
static const int kAutomationWipeDown = 39;

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
	L"Scroll up/down split", L"Scroll down/up split",
	L"Colour cycling (linear)", L"Colour cycling (bounce)",
	L"Alternate up/down scroll",
	L"Reveal left/right", L"Reveal right/left", L"Reveal top/bottom", L"Reveal bottom/top", L"Reveal centre in", L"Reveal centre out",
	L"Wipe left", L"Wipe right", L"Wipe up", L"Wipe down"
	};

static const int kActionsWipe[kWipeCount] 			= { 9, 10, 11, 12, 36, 37, 38, 39 };
static const int kActionsWipeOdd[kWipeOddCount] 	= { 36, 37, 38, 39 };
static const int kActionsWipeOddW[kWipeOddWCount] 	= { 11, 12, 36, 37, 38, 39 };
static const int kActionsWipeOddH[kWipeOddHCount]	= { 9, 10, 36, 37, 38, 39 };

static const int kActionsReveal[kRevealCount] 		= { 30, 31, 32, 33, 34, 35 };
