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

#include <vector>

#include "ActionObjectBrush.h"


enum class AutomateSource { kFirstFrame = 0, kEachFrame, kEachFrameInc };
enum class CyclingDirection { kForwards = 0, kBackwards };

static const int actionTypeRevealLeftRight = 30;
static const int actionTypeRevealRightLeft = 31;
static const int actionTypeRevealTopBottom = 32;
static const int actionTypeRevealBottomTop = 33;
static const int actionTypeRevealCentreOut = 34;
static const int actionTypeRevealCentreIn  = 35;


class ActionObject
{

public:

	std::wstring LastFileName = L"";

	int ProcesingStage = 0;

	int FrameStart = 0;
	int FrameEnd = 0;

	int Layer = 0;

	AutomateSource Source = AutomateSource::kFirstFrame;

	bool EraseBehind = false;

	std::vector<int> ActionList;
	std::vector<int> PostProcessList;

	int Parameter1 = 0;
	int Parameter2 = 0;
	int ParameterReveal = 0;
	int ParameterRevealColour = 0;

	std::vector<int> SourceColours;
	std::vector<int> TargetColours;
	int TargetSkip = 0;
	int TargetSkipIndex = 0;

	int CCSourceIndex = 0;
	int CCTargetIndex = 0;
	CyclingDirection CCDirection;

	ActionObjectBrush Brushes[2];

	ActionObject()
	{
	}

	void Clear()
	{
		Parameter1 = 0;
		Parameter2 = 0;
    }

	void SetParameterReveal(int Width, int Height)
	{
		for (int t = 0; t < ActionList.size(); t++)
		{
			int source = ActionList[t];

			switch (source)
			{
			case actionTypeRevealLeftRight:
				ParameterReveal = 0;
				break;
			case actionTypeRevealRightLeft:
				ParameterReveal = Width;
				break;
			case actionTypeRevealTopBottom:
				ParameterReveal = 0;
				break;
			case actionTypeRevealBottomTop:
				ParameterReveal = Height;
				break;
			case actionTypeRevealCentreOut:
				ParameterReveal = 0; // to do
				break;
			case actionTypeRevealCentreIn:
				ParameterReveal = 0;
				break;
			}
		}
	}
};
