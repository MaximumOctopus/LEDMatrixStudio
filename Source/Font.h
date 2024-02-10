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

#pragma once

#include "MatrixConstants.h"


class Font
{

public:

	MatrixMode Mode = MatrixMode::kNone;

	std::wstring Name = L"";

	int *Data = nullptr;
	int Start[96];
	int End[96];

	Font();
	~Font();

	void Clear();

	bool Load(const std::wstring, const std::wstring);
};
