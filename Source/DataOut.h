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

#include "Convert.h"
#include "MatrixConstants.h"


static const int _DataOutDataMax = 32;
static const int _BitCountDirectionDown = 0;
static const int _BitCountDirectionUp = 1;


struct BitCounting
{
	int highbit = 7;                           // highest bit, count between highbit and 0
	int resetvalue = 7;                        // when overflowing, set bitcounter to this
	int direction = _BitCountDirectionDown;    // count up or down

	int bitcounter = 7;                        // current bit
	unsigned int databyte = 0;                 // current value

	int outputcount = 0;                       // values added

	BitCounting()
	{
	}

	void SetDirection(int d, int bitmax)
	{
		direction = d;

		if (d == _BitCountDirectionDown)
		{
			resetvalue = bitmax;

			bitcounter = bitmax;
		}
		else
		{
			resetvalue = 0;

            bitcounter = 0;
		}

        highbit = bitmax;
    }

	bool Next()
	{
		if (direction == _BitCountDirectionDown)
		{
			bitcounter--;

			if (bitcounter < 0)
			{
				outputcount++;

				return true;
			}
		}
		else
		{
			bitcounter++;

			if (bitcounter > highbit)
			{
                outputcount++;

				return true;
			}
		}

		return false;
	}

	bool IsStartingPosition()
	{
		return bitcounter == resetvalue;
    }

	void Reset()
	{
		bitcounter = resetvalue;

		databyte = 0;
    }
};


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

	void SwapData(int index, NumberSize size)
	{
        std::wstring s = L"";

		switch (size)
		{
			case NumberSize::k8bitSwap:		// swap nybbles
			{
				std::wstring b = L"XX";

				s = IntToHex(Data[index], 2);

				b[0] = s[1];
				b[1] = s[0];

				Data[index] = Convert::HexToInt(b);
				break;
			}
			case NumberSize::k16bitSwap:	// swap bytes
			{
				std::wstring b = L"XXXX";

				s = IntToHex(Data[index], 4);

				b[0] = s[2];
				b[1] = s[3];
				b[2] = s[0];
				b[3] = s[1];

				Data[index] = Convert::HexToInt(b);
				break;
			}
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

	void SetColumn(int column, int data, int padmode, bool hexformat)
	{
		if (hexformat)
		{
			ColumnData[column] = IntToHex(data, padmode);
		}
		else
		{
			ColumnData[column] = std::to_wstring(data);
		}
	}

	void SetRow(int row, int data, int padmode, bool hexformat)
	{
		if (hexformat)
		{
			RowData[row] = IntToHex(data, padmode);
		}
		else
		{
			RowData[row] = std::to_wstring(data);
		}
    }
};
