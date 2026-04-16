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

Group
{
	property bool enableMedians: true

	info: qsTr("Specify the robust estimation method and its parameters. All methods are based on the WRS2 package.") + "\n" +
		"- " + qsTr("Trimmed Means: Uses a heteroscedastic ANOVA based on trimmed means (Welch-type generalization).") + "\n" +
		"- " + qsTr("Trimmed Means + Bootstrap: A percentile t-bootstrap version of the trimmed means test.") + "\n" +
		"- " + qsTr("Medians: A heteroscedastic ANOVA based on medians using the Harrell-Davis estimator.")

	DropDown
	{
		name:	"robustMethod"
		label:	qsTr("Method")
		id:		robustMethod
		info:	qsTr("Select the robust estimation method to use for the analysis.")
		values:
		[
			{ label: qsTr("Trimmed Means"),					value: "trimmedMeans"			},
			{ label: qsTr("Trimmed Means + Bootstrap"),		value: "trimmedMeansBootstrap"	},
			{ label: qsTr("Medians"),						value: "medians",				enabled: enableMedians }
		]
	}

	DoubleField
	{
		name:			"trimProportion"
		label:			qsTr("Trimming proportion")
		defaultValue:	0.2
		min:			0
		max:			0.5
		info:			qsTr("The proportion of observations to trim from each tail. Default is 0.2 (20%% trimming).")
		visible:		robustMethod.value !== "medians"
	}

	IntegerField
	{
		name:			"bootstrapSamples"
		label:			qsTr("Bootstrap samples")
		defaultValue:	599
		min:			100
		info:			qsTr("The number of bootstrap resamples to use. Default is 599.")
		visible:		robustMethod.value === "trimmedMeansBootstrap"
	}
}
