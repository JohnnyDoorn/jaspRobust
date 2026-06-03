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
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	info: qsTr("The continuous outcome variable to be regressed on the predictors."); allowedColumns: ["scale"]; singleVariable: true		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			info: qsTr("Continuous predictors entered as numeric covariates."); allowedColumns: ["scale"]; minNumericLevels: 2		}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				info: qsTr("Categorical predictors. Each factor is dummy coded with the first level as the reference."); allowedColumns: ["nominal"]; minLevels: 2			}
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
			CheckBox { name: "coefficientEstimate"; label: qsTr("Estimates"); info: qsTr("Display the estimated regression coefficients with standard errors, Wald t-statistics, and p-values."); checked: true }
			CheckBox
			{
				name: "coefficientCi"; label: qsTr("Confidence intervals")
				info: qsTr("Display Wald-type confidence intervals for the regression coefficients at the chosen confidence level.")
				childrenOnSameRow: true
				CIField { name: "coefficientCiLevel" }
			}
		}

		Group
		{
			title: qsTr("Descriptives")
			CheckBox
			{
				name:	"descriptivesTable"
				label:	qsTr("Descriptives")
				info:	qsTr("Per-variable sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation for the dependent variable and each covariate. The trimming proportion is used only for these descriptives; the robust regression fit itself uses the chosen estimation method.")
				DoubleField
				{
					name:			"descriptivesTrimProportion"
					label:			qsTr("Trimming proportion")
					defaultValue:	0.2
					min:			0
					max:			0.5
					info:			qsTr("Trimming proportion used for the trimmed mean and Winsorized SD in the descriptives table. Default is 0.2.")
				}
			}
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Residual Plots")
			CheckBox { name: "residualVsFittedPlot";	label: qsTr("Residuals vs. fitted");	info: qsTr("Plot residuals against fitted values with a loess smoother to inspect non-linearity or heteroscedasticity that survives the robust fit.")	}
			CheckBox { name: "residualQqPlot";			label: qsTr("Q-Q plot of residuals");	info: qsTr("Normal quantile-quantile plot of the standardised residuals to assess the residual distribution.")	}
		}

		Group
		{
			title: qsTr("Other Plots")
			CheckBox { name: "weightsPlot"; label: qsTr("Robust weights"); info: qsTr("Shows the weight assigned to each observation during estimation. Downweighted observations (weight < 1) indicate potential outliers.") }
		}
	}
}
