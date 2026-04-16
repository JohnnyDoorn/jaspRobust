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
		AssignedVariablesList  { name: "variables"; title: qsTr("Variables"); allowedColumns: ["scale"]; minNumericLevels: 2 }
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

		CheckBox { name: "sampleSize";			label: qsTr("Sample size");						checked: false }
		CheckBox { name: "significanceFlagged";	label: qsTr("Flag significant correlations");	checked: false }

		RadioButtonGroup
		{
			name: "alternative"
			title: qsTr("Alternative hypothesis")
			RadioButton { value: "twoSided";	label: qsTr("Correlated (two-sided)");	checked: true	}
			RadioButton { value: "greater";		label: qsTr("Positively correlated")					}
			RadioButton { value: "less";		label: qsTr("Negatively correlated")					}
		}
	}

	Group
	{
		title: qsTr("Plots")

		CheckBox { name: "scatterPlot"; label: qsTr("Scatter plots"); info: qsTr("Displays scatter plots with marginal density plots and a linear regression line for each pair of variables.") }
	}
}
