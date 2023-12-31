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

#include <string>
#include <windows.h>

#include "Formatting.h"


namespace Formatting
{
	std::wstring AddLeadingSpace(std::wstring input, int length)
	{
		if (input.length() == length)
		{
			return input;
		}
		else if (input.length() < length)
		{
			for (auto t = input.length(); t < length; t++)
			{
				input.insert(input.begin(), L' ');
			}

			return input;
		}

		return input;
	}


	std::wstring AddTrailingSpace(std::wstring input, int length)
	{
		if (input.length() == length)
		{
			return input;
		}
		else if (input.length() < length)
		{
			for (auto t = input.length(); t < length; t++)
			{
				input += L' ';
			}

			return input;
		}

		return input;
	}


	std::wstring PadString(wchar_t character, int count)
	{
		std::wstring s = L"";

		for (int i = 0; i < count; i++)
		{
			s += character;
		}

        return s;
	}


	std::wstring PadToLength(const std::wstring input, int count)
	{
		std::wstring s = input;

		while (input.length() < count)
		{
			s += L' ';
		}

		return s;
	}


	std::wstring PadZeroes(const std::wstring input, int count)
	{
		std::wstring s = input;

		while (s.length() < count)
		{
			s = L'0' + s;
		}

		return s;
	}


	std::wstring VectorToString(const std::vector<std::wstring> &input)
	{
		if (input.size() != 0)
		{
            std::wstring s = L"";

			for (int i = 0; i < input.size(); i++)
			{
				s += input[i] + L"\x0d\x0a";        // adds a linefeed that TMemo recognises to add a new line per each line in our vector
			}

			return s;
		}

		return L"";
	}


    // utf8 output magic
    std::string to_utf8(const std::wstring& str)
    {
        return to_utf8(str.c_str(), (int)str.size());
    }


    std::string to_utf8(const wchar_t* buffer, int len)
    {
        int nChars = ::WideCharToMultiByte(
            CP_UTF8,
            0,
            buffer,
            len,
            NULL,
            0,
            NULL,
            NULL);
        if (nChars == 0) return "";

        std::string newbuffer;

        newbuffer.resize(nChars);
        ::WideCharToMultiByte(
            CP_UTF8,
            0,
            buffer,
            len,
            const_cast<char*>(newbuffer.c_str()),
            nChars,
            NULL,
            NULL);

        return newbuffer;
    }
}