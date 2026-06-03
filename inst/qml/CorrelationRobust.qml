//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	info: qsTr("Robust correlation computes percentage bend or Winsorized correlations (WRS2 package). These methods are resistant to outliers that can distort Pearson's r.\n") +
	"## " + qsTr("Assumptions") + "\n" + "- Continuous variables.\n" + "- Independence of observations.\n" +
	"- Unlike Pearson's r, robust correlations do not assume bivariate normality."

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList  { name: "variables"; title: qsTr("Variables"); info: qsTr("Two or more continuous variables. All pairwise robust correlations are reported."); allowedColumns: ["scale"]; minNumericLevels: 2 }
	}

	Group
	{
		title: qsTr("Options")

		DropDown
		{
			name: "correlationMethod"
			label: qsTr("Method")
			info: qsTr("Percentage bend correlation downweights values in the tails. Winsorized correlation replaces extreme values with less extreme ones before computing the correlation.")
			values:
			[
				{ label: qsTr("Percentage bend"),	value: "percentageBend",	recommended: true	},
				{ label: qsTr("Winsorized"),		value: "winsorized"							}
			]
		}

		DoubleField
		{
			name:			"trimProportion"
			label:			qsTr("Trimming proportion")
			defaultValue:	0.2
			min:			0
			max:			0.5
			info:			qsTr("Bending constant β for percentage bend correlation, or Winsorization proportion for the Winsorized correlation. Also used for the descriptives table. Default is 0.2.")
		}

		CheckBox { name: "sampleSize";			label: qsTr("Sample size");						info: qsTr("Display the number of complete pairwise observations used in each correlation."); checked: false }
		CheckBox { name: "significanceFlagged";	label: qsTr("Flag significant correlations");	info: qsTr("Append asterisks to correlations whose p-value is below 0.05 (*), 0.01 (**), or 0.001 (***)."); checked: false }
		CheckBox { name: "descriptivesTable";	label: qsTr("Descriptives");					info: qsTr("Per-variable sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation."); checked: false }

		RadioButtonGroup
		{
			name: "alternative"
			title: qsTr("Alternative hypothesis")
			info: qsTr("Specify the directional hypothesis used to compute the p-value of each correlation.")
			RadioButton { value: "twoSided";	label: qsTr("Correlated (two-sided)");	info: qsTr("Test whether the correlation differs from zero in either direction."); checked: true	}
			RadioButton { value: "greater";		label: qsTr("Positively correlated");	info: qsTr("Test whether the correlation is greater than zero.")					}
			RadioButton { value: "less";		label: qsTr("Negatively correlated");	info: qsTr("Test whether the correlation is less than zero.")					}
		}
	}

	Group
	{
		title: qsTr("Plots")

		CheckBox { name: "scatterPlot"; label: qsTr("Scatter plots"); info: qsTr("Displays scatter plots with marginal density plots and a linear regression line for each pair of variables.") }
	}
}
