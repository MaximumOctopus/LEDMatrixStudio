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

#include "Formatting.h"
#include "MatrixIgnored.h"


MatrixIgnored::MatrixIgnored(int width, int height)
{
	Grid = new int[width * height];

	Width = width;
	Height = height;
}


MatrixIgnored::~MatrixIgnored()
{
	delete[] Grid;
}


MatrixIgnored::DataParameter MatrixIgnored::LoadDataParameterType(const std::wstring data, bool IgnorePixelMode)
{
	if (data.find(L"deadpixel") != std::wstring::npos)  // DO NOT EDIT THIS, COMPATIBILITY REASONS!!!
	{
		return DataParameter::kIgnoredPixelBegin;
	}
	else if (data[0] == kDataBlockEnd)
	{
		return DataParameter::kIgnoredPixelEnd;
	}
	else
	{
		if (IgnorePixelMode)
		{
			if (data[0] == L'p')
			{
				return DataParameter::kRowData;
			}
		}
    }

	return DataParameter::kUnknown;
}


bool MatrixIgnored::Load(const std::wstring file_name)
{
	bool IgnorePixelMode = false;
	int Row = 0;

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
					std::wstring v = s.substr(2);   // make lowercase

					switch (LoadDataParameterType(v, IgnorePixelMode))
					{
					case DataParameter::kIgnoredPixelBegin:
						IgnorePixelMode = true;
						Row = 0;
						break;
					case DataParameter::kIgnoredPixelEnd:
						IgnorePixelMode = false;
						break;
					case DataParameter::kRowData:
					{
						int column = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								if (pixel == L"0")
								{
									Grid[Row * Width + column] = PixelAlive;
								}
								else
								{
									Grid[Row * Width + column] = PixelIgnored;
								}

								column++;

								pixel.clear();
							}
							else
							{
								pixel += v[i];
							}

							Row++;
						}

						break;
					}
					}
				}
			}
		}

		return true;
	}

	return false;
}


bool MatrixIgnored::Save(const std::wstring file_name, int width, int height)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderIgnoredPixel + L"\n");

		for (int y = 0; y < height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < width; x++)
			{
				if (Grid[y * width + x] == PixelAlive)
				{
					s += L"0";
				}
				else
				{
					s += L"1";
				}
			}

			file << Formatting::to_utf8(kAnimIgnoredPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		file.close();

		return true;
	}

	return false;
}


void MatrixIgnored::SetAllPixels(int NewStatus)
{
	for (int z = 0; z < Width * Height; z++)
	{
		Grid[z] = NewStatus;
	}
}


void MatrixIgnored::SimpleLine(int x1, int y1, int x2, int y2)
{
	int column = x1;

	while (column <= x2)
	{
		Grid[y1 * Width + column] = PixelAlive;

		column++;
	}
}


void MatrixIgnored::SetFromCustomShape(int width, int height, CustomShape shape, int parameter)
{
	switch (shape)
	{
	case CustomShape::kNone:
		break;
	case CustomShape::kCircle:
	{
		SetAllPixels(PixelIgnored);

		int x1 = std::floor(width / 2);
		int y1 = std::floor(height / 2);

		// c^2 = a^2 + b^2
		int tc = x1; // radius of circle

		// midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm

		int a = 0;
		int b = 1 - tc;

		while (tc >= a)
		{
			SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1);
			SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1);
			SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1);
			SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1);

			a++;
		}

		if (b < 0)
		{
			b = b + 2 * a + 1;
		}
		else
		{
			tc--;
			b = b + 2 * (a - tc + 1);
		}

		break;
	}
	case CustomShape::kBorders:
	{
		parameter++;

		for (int x = 0 + parameter; x <= width - (1 + parameter); x++)
		{
			for (int y = 0 + parameter; y <= height - (1 + parameter); y++)
			{
				if (x > 0 && x < width &&
					y > 0 && y < height)
				{
					Grid[y * Width + x] = PixelIgnored;
				}
			}
		}
		break;
	}
	case CustomShape::kTriangle:
	{
		SetAllPixels(PixelIgnored);

		int b = 1;
		int x = std::floor(width / 2) - 1;
		int y = 0;

		while (y < height)
		{
			for (int a = 1; a <= b; a++)
			{
				if ((x + a) > 0 && (x + a) < width &&
					 y > 0 && y < height)
				{
					Grid[y * Width + x + a] = PixelAlive;

					x--;
					b += 2;
					y++;
				}
			}
		}
		break;
	}
	}
}
