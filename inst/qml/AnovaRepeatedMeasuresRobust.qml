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
	info: qsTr("Robust Repeated Measures ANOVA analyzes differences between dependent group means using robust methods from the WRS2 package. No assumptions of normality, homogeneity, or sphericity are required.") + "\n" +
"## " + qsTr("Assumptions") + "\n" +
"- " + qsTr("The dependent variable is continuous.") + "\n" +
"- " + qsTr("The observations within each subject are dependent (repeated measures).")

	id: form
	property int analysis:	Common.Type.Analysis.RMANOVA
	property int framework:	Common.Type.Framework.Classical

	VariablesForm
	{
		preferredHeight: 520 * preferencesModel.uiScale
		AvailableVariablesList			{ name: "allVariablesList" }
		FactorLevelList					{ name: "repeatedMeasuresFactors";	title: qsTr("Repeated Measures Factors"); info: qsTr("The within-subjects (repeated measures) variables. Here the repeated measures factors of interest and the different levels that belong to each factor can be labelled.")	; height: 180 * preferencesModel.uiScale;	factorName: qsTr("RM Factor")	}
		AssignedRepeatedMeasuresCells	{ name: "repeatedMeasuresCells";	title: qsTr("Repeated Measures Cells"); info: qsTr("The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s).")	;	source: "repeatedMeasuresFactors"										}
		AssignedVariablesList			{ name: "betweenSubjectFactors";	title: qsTr("Between Subject Factors");	info: qsTr("Optional between-subjects grouping variable.")	; allowedColumns: ["nominal"]; minLevels: 2;	itemType: "fixedFactors"	}
	}

	Common.RobustOptions
	{
		enableMedians: false
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptivesTable";	label: qsTr("Descriptives");	info: qsTr("Per-cell sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation across the within-subjects (and optional between-subjects) factor levels.") }
	}

	Classical.PostHoc
	{
		source: [{ name: "repeatedMeasuresFactors" }, { name: "betweenSubjectFactors" }]
	}

	Classical.DescriptivePlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
	}

	Common.RainCloudPlots
	{
		source: ["repeatedMeasuresFactors", "betweenSubjectFactors"]
	}
}