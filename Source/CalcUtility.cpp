
#include "CalcUtility.h"


double CalcUtility::DegToRadians(double degrees)
{
	return degrees * (3.14159265358979323846 / 180);
}


void CalcUtility::IncWithRollOver(int &value, const int max)
{
	if (max == value)
	{
		value = 0;
		return;
	}

	value++;
}
