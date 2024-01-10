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

#include <string>

#include "MatrixConstants.h"


static const int _DataOutDataMax = 32;


struct InternalArray
{
	unsigned __int64 Data[_DataOutDataMax];

	void Clear()
	{
		for (int t = 0; t < _DataOutDataMax; t++)
		{
			Data[t] = -1;
		}
	}
};


 struct DataOut
 {
	int Count = 0;
	std::wstring Data[_DataOutDataMax];

	void Clear()
	{
		for (int t = 0; t < _DataOutDataMax; t++)
		{
			Data[t] = L"";
        }
	}
};


struct DataOutDisplay
{
	std::wstring Text = L"";
	std::wstring ColumnData[__MaxHeight];
	std::wstring RowData[__MaxHeight];
};
