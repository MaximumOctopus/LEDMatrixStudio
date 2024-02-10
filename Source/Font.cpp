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

#include "Convert.h"
#include "FileConstants.h"
#include "Font.h"


Font::Font()
{
	Data = new int[96 * 8 * 8]; // 96 characters * 8 pixels wide * 8 pixels high
}


Font::~Font()
{
	delete[] Data;
}


void Font::Clear()
{
	for (int i = 0; i < 96 * 8 * 8; i++)
	{
		Data[i] = -1;
	}

	for (int i = 0; i < 96; i++)
	{
		Start[i] = 0;
		End[i] = 0;
	}

	Name = L"";
}


bool Font::Load(const std::wstring file_name, const std::wstring name)
{
	Clear();

	// =======================================================================

	std::wifstream file(file_name);

	if (file)
	{
	    Name = name;

		std::wstring s = L"";

		std::getline(file, s);

		if (s.find(kFileHeaderFontRGB) != std::wstring::npos)
		{
			int frame     = 0;
			int colid      = 0;
			int height = 0;
			int FirstData = -1;
			int LastData  = -1;
			bool headermode = true;

			Mode = MatrixMode::kRGB;

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
						switch (s[0])
						{
						case kDataBlockStart:
						{
							if (headermode)
							{
								headermode = false;
							}
							else
							{
								if (s.find(kFontPrefixChar) != std::wstring::npos)
								{
									frame++;

									colid = 0;

									FirstData = -1;
									LastData = -1;
								}
							}
							break;
						}
						case kDataBlockEnd:
						{
							if (!headermode)
							{
								if (FirstData != -1)
								{
									Start[frame] = FirstData;
								}
								else
								{
									Start[frame] = 0;
								}

								if (LastData != -1)
								{
									End[frame] = LastData;
								}
								else
								{
									End[frame] = Start[frame];
								}
							}
							break;
						}
						case kRGBFontData:
						{
							std::wstring Input = L"";
							int rowid   = height - 1;
							bool haddata = false;

							for (int t = 2; t < s.length(); t++)
							{
								if (s[t] == L' ')
								{
									if (Input == L"-1")
									{
										Data[(frame * 64) + (rowid * 8) + colid] = -1;
									}
									else
									{
										Data[(frame * 64) + (rowid * 8) + colid] = Convert::HexToInt(Input);

										haddata = true;
									}

									rowid--;

									Input = L"";
								}
								else
								{
									Input += s[t];
								}
							}

							if (haddata)
							{
								if (FirstData == -1)
								{
									FirstData = colid;
								}
								else
								{
									LastData = colid;
								}
                            }

							colid++;
							break;
						}
						case kRGBFontHeight:
							if (headermode)
							{
								height = stoi(s.substr(2));
							}
                            break;
						}
					}
				}
			}
		}
		else	// could do with seeking to beginning of file
		{
			Mode = MatrixMode::kMono;
			int frame = 0;

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
						std::wstring Input = L"";
						int t = 0;
						int byte = 0;
						int colid  = 0;

						while (s[t] != L'/' && t <= s.length())
						{
							if (s[t] == 32)
							{
								if (!Input.empty())
								{
									byte = stoi(Input);

									for (int p = 0; p < 8; p++)
									{
										if ((byte & powers[p]) == powers[p])
										{
											Data[(frame * 64) + (p * 8) + colid] = 1;
										}
									}

									Input = L"";
									colid++;
								}
							}
							else if (isdigit(s[t]))
							{
								Input += s[t];
							}

							t++;
						}

						Start[frame] = 0;
						End[frame] = colid - 1;

						frame++;
					}
				}
			}
		}

		file.close();

		return true;
	}

	return false;
}
