// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
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

		// ===================================================================

	   /*	for (int slot = 1; slot < MatrixLayers[0].Frames.size(); slot++)     TO DO, why does it draw to all frames?! move to thematrix, add option for frame/layer/etc.
		{
			for (int x = 0; x < Matrix.Width; x++)
			{
				for (int y = 0; y < Matrix.Height; y++)
				{
					if (MatrixLayers[0].Frames[slot].Grid[y * Details.Width + x] <> FRGBBackground)
					{
						MatrixLayers[0].Frames[slot].Grid[y * Matrix.Width + x] = Render.Gradient.IY[y];
					}
				}
			}
		}*/

		// ===================================================================

		//PaintBox->Invalidate();

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
