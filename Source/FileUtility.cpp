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
#include "FileUtility.h"


MatrixMode FileUtility::GetMatrixModeFromFileChunk(const wchar_t c)
{
	switch (c)
	{
	case L'2':
		return MatrixMode::kBiSequential;
	case L'3':
		return MatrixMode::kBiBitplanes;
	case L'4':
		return MatrixMode::kRGB;
	case L'5':
		return MatrixMode::kRGB3BPP;
	}

	return MatrixMode::kMono;
}


bool FileUtility::SaveVector(const std::wstring file_name, const std::vector<std::wstring> &v)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int t = 0; t < v.size(); t++)
		{
			file << Formatting::to_utf8(v[t] + L"\n");
		}

		file.close();

		return true;
	}

    return false;
}
