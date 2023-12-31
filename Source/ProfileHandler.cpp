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
#include "ProfileHandler.h"
#include "Utility.h"

ProfileHandler *GProfileHandler;


ProfileHandler::ProfileHandler(const std::wstring path)
{
	Path = path;

	UpdateAll();
}


ExportOptions ProfileHandler::Load(const std::wstring file_name)
{
	std::wifstream file(file_name);

	ExportOptions eeo;

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
					std::wstring v = L"";

					if (s.length() >= 3)
					{
						v = s.substr(2);
                    }

					switch (GetParameterType(s))
					{
					case LoadProfile::kDataBegin:
						if (s.find(L"RGB") != std::wstring::npos)
						{
							eeo.Code.RGBEnabled = true;
						}
						else
						{
							eeo.Code.RGBEnabled = false;
						}
                        break;
					case LoadProfile::kDataEnd:
						break;
					case LoadProfile::kSource:
						eeo.SourceFromInt(stoi(v));
						break;
					case LoadProfile::kOrientation:
						eeo.OrientationFromInt(stoi(v));
						break;
					case LoadProfile::kLSB:
						eeo.LSBFromInt(stoi(v));
						break;
					case LoadProfile::kLanguage:
						eeo.LanguageFromInt(stoi(v));
						break;
					case LoadProfile::kNumberFormat:
						eeo.NumberFormatFromInt(stoi(v));
						break;
					case LoadProfile::kNumberSize:
						eeo.NumberSizeFromInt(stoi(v));
						break;
					case LoadProfile::kScanDirection:
						eeo.ScanDirectionFromInt(eeo.Code.Source, stoi(v));
						break;
					case LoadProfile::kLineContent:
						eeo.LineContentFromInt(stoi(v));
						break;
					case LoadProfile::kLineCount:
						eeo.Code.LineCount = stoi(v);
						break;

					case LoadProfile::kRGBMode:
						eeo.RGBModeFromInt(stoi(v));
						break;
					case LoadProfile::kRGBChangePixels:
						eeo.Code.RGBChangePixels = stoi(v);
						break;
					case LoadProfile::kRGBChangeColour:
						eeo.Code.RGBChangeColour = stoi(v);
						break;
					case LoadProfile::kRGBBrightness:
						eeo.Code.RGBBrightness = stoi(v);
						break;

					case LoadProfile::kMinWidth:
						eeo.MinWidth = stoi(v);
						break;
					case LoadProfile::kMaxWidth:
						eeo.MaxWidth = stoi(v);
						break;
					case LoadProfile::kMinHeight:
						eeo.MinHeight = stoi(v);
						break;
					case LoadProfile::kMaxHeight:
						eeo.MaxHeight = stoi(v);
						break;

					case LoadProfile::kInformation:
						eeo.Information = v;
						break;

					case LoadProfile::kBinarySource:
						eeo.BinarySourceFromInt(stoi(v));
						break;
					case LoadProfile::kBinaryOrientation:
						eeo.BinaryOrientationFromInt(stoi(v));
						break;
					case LoadProfile::kBinaryLSB:
						eeo.BinaryLSBFromInt(stoi(v));
						break;
					case LoadProfile::kBinaryScanDirection:
						eeo.BinaryScanDirectionFromInt(eeo.Binary.Source, stoi(v));
						break;

					case LoadProfile::kBinaryRGBMode:
						eeo.BinaryRGBFormatFromInt(stoi(v));
						break;
					case LoadProfile::kBinaryRGBChangePixels:
						eeo.Binary.RGBChangePixels = stoi(v);
						break;
					case LoadProfile::kBinaryRGBChangeColour:
						eeo.Binary.RGBChangeColour = stoi(v);
						break;
					case LoadProfile::kBinaryRGBBrightness:
						eeo.Binary.RGBBrightness = stoi(v);
						break;
					case LoadProfile::kBinaryFileContents:
						eeo.BinaryFileContentsFromInt(stoi(v));
						break;
					}
                }
			}
		}

        eeo.Valid = true;

		file.close();
	}

	return eeo;
}


bool ProfileHandler::Save(const std::wstring file_name, bool IsRGB, ExportOptions &eeo)
{
	std::ofstream file(file_name);

	if (file)
	{
		if (IsRGB)
		{
			file << Formatting::to_utf8(L"{RGB\n");
		}
		else
		{
			file << Formatting::to_utf8(kDataBlockStartS + L"\n");
		}

		file << Formatting::to_utf8(kExportSource +          L":" + std::to_wstring(eeo.SourceToInt()) + L"\n");
		file << Formatting::to_utf8(kExportOrientation +     L":" + std::to_wstring(eeo.OrientationToInt()) + L"\n");
		file << Formatting::to_utf8(kExportLSB +             L":" + std::to_wstring(eeo.LSBToInt()) + L"\n");
		file << Formatting::to_utf8(kExportLanguage +        L":" + std::to_wstring(eeo.LanguageToInt()) + L"\n");
		file << Formatting::to_utf8(kExportNumberFormat +    L":" + std::to_wstring(eeo.NumberFormatToInt()) + L"\n");
		file << Formatting::to_utf8(kExportNumberSize +      L":" + std::to_wstring(eeo.NumberSizeToInt()) + L"\n");
		file << Formatting::to_utf8(kExportScanDirection +   L":" + std::to_wstring(eeo.ScanDirectionToInt()) + L"\n");
		file << Formatting::to_utf8(kExportLineContent +     L":" + std::to_wstring(eeo.ContentToInt()) + L"\n");
		file << Formatting::to_utf8(kExportLineCount +       L":" + std::to_wstring(eeo.Code.LineCount) + L"\n");
		file << Formatting::to_utf8(kExportRGBMode +         L":" + std::to_wstring(eeo.RGBFormatToInt()) + L"\n");
		file << Formatting::to_utf8(kExportRGBChangePixels + L":" + std::to_wstring(eeo.Code.RGBChangePixels) + L"\n");
		file << Formatting::to_utf8(kExportRGBChangeColour + L":" + std::to_wstring(eeo.Code.RGBChangeColour) + L"\n");
		file << Formatting::to_utf8(kExportRGBBrightness +   L":" + std::to_wstring(eeo.Code.RGBBrightness) + L"\n");

		// binary export options

		file << Formatting::to_utf8(kExportBinarySource +          L":" + std::to_wstring(eeo.BinarySourceToInt()) + L"\n");
		file << Formatting::to_utf8(kExportBinaryOrientation   +   L":" + std::to_wstring(eeo.BinaryOrientationToInt()) + L"\n");
		file << Formatting::to_utf8(kExportBinaryLSB +             L":" + std::to_wstring(eeo.BinaryLSBToInt()) + L"\n");
		file << Formatting::to_utf8(kExportBinaryScanDirection +   L":" + std::to_wstring(eeo.BinaryScanDirectionToInt()) + L"\n");
		file << Formatting::to_utf8(kExportBinaryRGBMode +         L":" + std::to_wstring(eeo.BinaryRGBFormatToInt()) + L"\n");
		file << Formatting::to_utf8(kExportBinaryRGBChangePixels + L":" + std::to_wstring(eeo.Binary.RGBChangePixels) + L"\n");
		file << Formatting::to_utf8(kExportBinaryRGBChangeColour + L":" + std::to_wstring(eeo.Binary.RGBChangeColour) + L"\n");
		file << Formatting::to_utf8(kExportBinaryRGBBrightness +   L":" + std::to_wstring(eeo.Binary.RGBBrightness) + L"\n");
		file << Formatting::to_utf8(kExportBinaryFileContents +    L":" + std::to_wstring(eeo.BinaryFileContentsToInt()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		file.close();

		return true;
	}

	return false;
}


bool ProfileHandler::DeleteExportProfile(const std::wstring file_name)
{
	return DeleteFile(file_name.c_str());
}


bool ProfileHandler::PopulateList(std::vector<std::wstring> &list, const std::wstring path)
{
	list.clear();

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(path.c_str(), &file);

	if (search_handle != INVALID_HANDLE_VALUE)
	{
		do
		{
			if (file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			{
			}
			else
			{
				std::wstring s = file.cFileName;

				list.push_back(Utility::RemoveExtension(s));
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}

    return true;
}


ProfileHandler::LoadProfile ProfileHandler::GetParameterType(const std::wstring s)
{
	if (s[0] == kDataBlockStart)
		return LoadProfile::kDataBegin;
	else if (s[0] == kDataBlockEnd)
		return LoadProfile::kDataEnd;
	else if (s[0] == kExportSource[0])
		return LoadProfile::kSource;
	else if (s[0] == kExportOrientation[0])
		return LoadProfile::kOrientation;
	else if (s[0] == kExportLSB[0])
		return LoadProfile::kLSB;
	else if (s[0] == kExportLanguage[0])
		return LoadProfile::kLanguage;
	else if (s[0] == kExportNumberFormat[0])
		return LoadProfile::kNumberFormat;
	else if (s[0] == kExportNumberSize[0])
		return LoadProfile::kNumberSize;
	else if (s[0] == kExportScanDirection[0])
		return LoadProfile::kScanDirection;
	else if (s[0] == kExportLineContent[0])
		return LoadProfile::kLineContent;
	else if (s[0] == kExportLineCount[0])
		return LoadProfile::kLineCount;
	else if (s[0] == kExportRGBMode[0])
		return LoadProfile::kRGBMode;
	else if (s[0] == kExportRGBChangePixels[0])
		return LoadProfile::kRGBChangePixels;
	else if (s[0] == kExportRGBChangeColour[0])
		return LoadProfile::kRGBChangeColour;
	else if (s[0] == kExportRGBBrightness[0])
		return LoadProfile::kRGBBrightness;
	else if (s[0] == kExportMinWidth[0])
		return LoadProfile::kMinWidth;
	else if (s[0] == kExportMaxWidth[0])
		return LoadProfile::kMaxWidth;
	else if (s[0] == kExportMinHeight[0])
		return LoadProfile::kMinHeight;
	else if (s[0] == kExportMaxHeight[0])
		return LoadProfile::kMaxHeight;
	else if (s[0] == kExportInformation[0])
		return LoadProfile::kInformation;

	else if (s[0] == kExportBinarySource[0])
		return LoadProfile::kBinarySource;
	else if (s[0] == kExportBinaryOrientation[0])
		return LoadProfile::kBinaryOrientation;
	else if (s[0] == kExportBinaryLSB[0])
		return LoadProfile::kBinaryLSB;
	else if (s[0] == kExportBinaryScanDirection[0])
		return LoadProfile::kBinaryScanDirection;
	else if (s[0] == kExportBinaryRGBMode[0])
		return LoadProfile::kBinaryRGBMode;
	else if (s[0] == kExportBinaryRGBChangePixels[0])
		return LoadProfile::kBinaryRGBChangePixels;
	else if (s[0] == kExportBinaryRGBChangeColour[0])
		return LoadProfile::kBinaryRGBChangeColour;
	else if (s[0] == kExportBinaryRGBBrightness[0])
		return LoadProfile::kBinaryRGBBrightness;
	else if (s[0] == kExportBinaryFileContents[0])
		return LoadProfile::kBinaryFileContents;

	return LoadProfile::kUnknown;
}


void ProfileHandler::UpdateAll()
{
	PopulateList(Profiles, Path + L"ledsexport");
	PopulateList(ProfilesRGB, Path + L"ledsexportrgb");
	PopulateList(ProfilesRGB3BPP, Path + L"ledsexportrgb3bpp");
}
