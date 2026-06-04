//
// Copyright (C) 2013-2022 University of Amsterdam
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
import "./" as Classical


Section
{
	title: qsTr("Post Hoc Tests"); info: qsTr("Perform pairwise comparisons between group levels using robust methods. The specific post hoc function is determined by the selected robust method (e.g., lincon for trimmed means, mcppb20 for bootstrap).")
	property alias source: availableTerms.source

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList	{	name: "postHocAvailableTerms";	id: availableTerms }
		AssignedVariablesList	{	name: "postHocTerms" }
	}

	Group
	{
		title: qsTr("Correction"); info: qsTr("To correct for multiple comparisons, various methods are available for adjusting the p-value. Note: corrections are applied when supported by the chosen robust post hoc method.")
		CheckBox { name: "postHocCorrectionHochberg";		label: qsTr("Hochberg");	info: qsTr("Hochberg's (1988) sharper Bonferroni procedure. This is the default correction used by WRS2's lincon function.") ;	checked: true	}
		CheckBox { name: "postHocCorrectionBonferroni";		label: qsTr("Bonferroni");	info: qsTr("This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.")			}
		CheckBox { name: "postHocCorrectionHolm";			label: qsTr("Holm");		info: qsTr("Also called sequential Bonferroni, and considered less conservative than the Bonferroni method.")				}
	}

	// Classical.PostHocDisplay{}
}
