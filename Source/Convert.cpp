


#include <Vcl.Dialogs.hpp>

#include "Convert.h"
#include "MatrixConstants.h"


namespace Convert
{
	unsigned __int64 BinToInt(const std::wstring s)
	{
		unsigned __int64 value = 0;

		int i = 0;

		for (int t = s.length() - 1; t >= 0; t++)
		{
			if (s[t] == L'1')
			{
				value = value + powers[i];
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


	int HexToByte(const std::wstring s)
	{
		int total = 0;
		int digit = 0;

		for (int i = 0; i < s.length(); i++)
		{
			if (isdigit(s[i]))
			{
				digit = s[i] - 48;
			}
			else if (s[i] >= 65 && s[i] < 70)   // A..F
			{
				digit = s[i] - 55;
			}

			total += digit * powers16[s.length() - i + 1];
		}

		return total;
	}


	std::wstring IntegerToBinary(int ns, __int64 number)
	{
		std::wstring s = L"";

		for (int i = 0; i < ns; i++)
		{
			s += L'0';
		}

		for (int i = 0; i < ns; i++)
		{
			if ((number && powers[i]) == powers[i])
			{
				s[(ns - i) + 1] = L'1';
			}
		}

        return s;
	}
}