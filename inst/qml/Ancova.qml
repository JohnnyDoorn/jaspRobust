//
// Copyright (C) 2013-2018 University of Amsterdam
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
import JASP
import JASP.Controls
import "./common" as Common
import "./common/classical" as Classical

Form
{
	info: qsTr("Robust ANCOVA compares trimmed means of two groups while adjusting for a covariate, using a nonparametric running interval smoother. No parametric assumptions about homogeneity of regression slopes or equal variances are required.") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The independent variable is categorical with exactly two levels.") + "\n" +
	"- " + qsTr("The dependent variable and covariate are continuous.") + "\n" +
	"- " + qsTr("The groups are independent.")

	id: form
	property int analysis:	Common.Type.Analysis.ANCOVA
	property int framework:	Common.Type.Framework.Classical

	VariablesForm
	{
		preferredHeight:	400 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable");	info: qsTr("The continuous outcome variable.");	allowedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "fixedFactors";	title: qsTr("Fixed Factor");			info: qsTr("A categorical grouping variable with exactly two levels.");	allowedColumns: ["nominal"];	singleVariable: true;	minLevels: 2	}
		AssignedVariablesList	{ name: "covariates";		title: qsTr("Covariate");			info: qsTr("A continuous covariate to adjust for.");	allowedColumns: ["scale"];	singleVariable: true;	minNumericLevels: 2	}
	}

	Common.RobustOptions
	{
		enableMedians: false
	}

	Classical.DescriptivePlots
	{
		source: ["fixedFactors", "covariates"]
	}

	Common.RainCloudPlots
	{
		source: ["fixedFactors"]
	}
}