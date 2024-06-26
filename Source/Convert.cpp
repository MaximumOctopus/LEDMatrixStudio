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

#include <Vcl.Dialogs.hpp>

#include "Convert.h"
#include "MatrixConstants.h"


namespace Convert
{
	unsigned __int64 BinToInt(const std::wstring s)
	{
		unsigned __int64 value = 0;

		int i = 0;

		for (int t = s.length() - 1; t >= 0; t--)
		{
			if (s[t] == L'1')
			{
				value += powers[i];
			}

			i++;
		}

        return value;
	}


	__int64 HexToInt(const std::wstring s)
	{
		int total = 0;
		int digit = 0;

		for (int i = 0; i < s.length(); i++)
		{
			if (isdigit(s[i]))
			{
				digit = s[i] - 48;
			}
			else if (s[i] >= 65 && s[i] <= 70)   // A..F
			{
				digit = s[i] - 55;
			}

			total += digit * powers16[s.length() - i - 1];
		}

		return total;
	}


	char HexToByte(const std::wstring s)
	{
		int total = 0;
		int digit = 0;
		int multiplier = 1;

		for (int i = s.length() - 1; i >= 0; i--)
		{
			int c = (unsigned char)s[i];

			if (isdigit(s[i]))
			{
				digit = c - 48;
			}
			else if (c >= 65 && c <= 70)   // A..F
			{
				digit = c - 55;
			}

			total += digit * multiplier;

			multiplier *= 16;
		}

		return total & 0xff;
	}


	std::wstring IntegerToBinary(int ns, __int64 number)
	{
		std::wstring s = L"";

		for (int i = 0; i <= ns; i++)
		{
			s += L'0';
		}

		for (int i = 0; i <= ns; i++)
		{
			if ((number & powers[i]) == powers[i])
			{
				s[ns - i] = L'1';
			}
		}

        return s;
	}
}