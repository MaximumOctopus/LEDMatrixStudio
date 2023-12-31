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

#include "CalcUtility.h"


double CalcUtility::DegToRadians(double degrees)
{
	return degrees * (3.14159265358979323846 / 180);
}


void CalcUtility::IncWithRollOver(int &value, const int max)
{
	if (max == value)
	{
		value = 0;

		return;
	}

	value++;
}
