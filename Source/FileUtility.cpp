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

#include "Formatting.h"
#include "FileUtility.h"


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
