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
import JASP
import JASP.Controls
import "./common" as Common
import "./common/classical" as Classical

Form
{
	info: qsTr("Robust ANOVA analyzes differences between group means using robust methods from the WRS2 package. These methods do not assume normality or homogeneity of variances.") + "\n" +
	"## " + qsTr("Assumptions") + "\n" +
	"- " + qsTr("The independent variables are categorical, the dependent variable is continuous.") + "\n" +
	"- " + qsTr("The groups are independent.")

	id: form
	property int analysis:	Common.Type.Analysis.ANOVA
	property int framework:	Common.Type.Framework.Classical

	VariablesForm
	{
		preferredHeight: 350 * preferencesModel.uiScale
		AvailableVariablesList	{	name:	"allVariablesList" }
		AssignedVariablesList	{	name:	"dependent";		title: qsTr("Dependent Variable");	info: qsTr("The continuous outcome variable.");	allowedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{	name:	"fixedFactors";		title: qsTr("Fixed Factors");		info: qsTr("The categorical grouping variables.");	allowedColumns: ["nominal"];	minLevels: 2	}
	}

	Common.RobustOptions
	{
		enableMedians: true
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptivesTable";	label: qsTr("Descriptives");		info: qsTr("Per-cell sample size, trimmed (or untrimmed) mean, median, Winsorized standard deviation, and median absolute deviation.") }
		CheckBox { name: "effectSizeTable";		label: qsTr("Robust effect sizes");	info: qsTr("Algina-Keselman-Penfield robust standardised mean differences (ξ) for each pair of factor levels. Computed via WRS2::akp.effect on a one-way design.") }
	}

	Classical.PostHoc
	{
		source: "fixedFactors"
	}

	Classical.DescriptivePlots
	{
		source: ["fixedFactors"]
	}

	Common.RainCloudPlots
	{
		source: ["fixedFactors"]
	}
}