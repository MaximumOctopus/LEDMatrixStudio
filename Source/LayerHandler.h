// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
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

#include <vector>

#include "Layer.h"

// coming soon ;)


class LayerHandler
{
public:

	int Width = 0;
    int Height = 0;
    //MatrixDrawMode DrawMode = MatrixDrawMode::kGrid;

	std::vector<Layer*> Layers;

	LayerHandler();
	~LayerHandler();

    bool AddLayerSilent(const std::wstring);
};