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
	info: qsTr("Robust linear regression uses M-estimation (MASS::rlm) to fit a linear model that is resistant to outliers. Unlike OLS, it iteratively downweights observations with large residuals.\n") +
	"## " + qsTr("Assumptions") + "\n" + "- Continuous response variable.\n" + "- Linearity: The response variable is linearly related to all predictors.\n" +
	"- Independence of observations.\n" + "- Unlike OLS, robust regression does not assume normality or homoscedasticity of residuals."

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleVariable: true		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]; minNumericLevels: 2		}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal"]; minLevels: 2			}
	}

	DropDown
	{
		name: "estimationMethod"
		label: qsTr("Estimation method")
		info: qsTr("Huber downweights outliers gradually. Bisquare (Tukey) can fully reject extreme outliers. MM-estimation combines high breakdown point with high efficiency.")
		values:
		[
			{ label: qsTr("M-estimation (Huber)"),		value: "huber",		recommended: true	},
			{ label: qsTr("M-estimation (Bisquare)"),	value: "bisquare"						},
			{ label: qsTr("MM-estimation"),				value: "mm"								}
		]
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "coefficientEstimate"; label: qsTr("Estimates"); checked: true }
			CheckBox
			{
				name: "coefficientCi"; label: qsTr("Confidence intervals")
				childrenOnSameRow: true
				CIField { name: "coefficientCiLevel" }
			}
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Residual Plots")
			CheckBox { name: "residualVsFittedPlot";	label: qsTr("Residuals vs. fitted")					}
			CheckBox { name: "residualQqPlot";			label: qsTr("Q-Q plot of residuals")				}
		}

		Group
		{
			title: qsTr("Other Plots")
			CheckBox { name: "weightsPlot"; label: qsTr("Robust weights"); info: qsTr("Shows the weight assigned to each observation during estimation. Downweighted observations (weight < 1) indicate potential outliers.") }
		}
	}
}
