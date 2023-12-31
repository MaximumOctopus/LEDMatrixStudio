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

#include <fstream>

#include "FileConstants.h"
#include "Formatting.h"
#include "Gradient.h"


bool MatrixGradient::Load(const std::wstring file_name)
{
	std::wifstream file(file_name);

	if (file)
	{
		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					if (s[0] == kGradientColour[0])
					{
						std::wstring v = s.substr(2);

						int idx     = 0;
						std::wstring colour = L"";

						for (int t = 0; t < v.length(); t++)
						{
							if (v[t] == L' ')
							{
								if (Option == GradientOption::kVertical)
								{
									IY[idx] = stoi(colour);
								}
								else
								{
									IX[idx] = stoi(colour);
								}

								colour = L"";

								idx++;
							}
							else
							{
								colour += v[t];
							}
						}
					}
				}
			}
		}

		file.close();

		return true;
	}

	return false;
}


bool MatrixGradient::Save(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kGradientFileHeader + L"\n");

		std::wstring g = L"";

		for (int t = 0; t < Height; t++)
		{
			g += std::to_wstring(IY[t]) + L" ";
		}

		file << Formatting::to_utf8(kGradientColour + L":" + g + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		file.close();

		return true;
	}

    return false;
}


void MatrixGradient::Clear(int colour)
{
	for (int t = 0; t < Width; t++)
	{
		IX[t] = colour;
	}

	for (int t = 0; t < Height; t++)
	{
		IY[t] = colour;
	}
}
