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

Form
{
	id: form

	Formula
	{
		lhs: "dependent"
		rhs: "fixedFactors"
		userMustSpecify: "fixedFactors"
	}

	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		AvailableVariablesList	{	name:	"allVariablesList" }
		AssignedVariablesList	{	name:	"dependent";		title: qsTr("Dependent Variable");	allowedColumns: ["scale"];				singleVariable: true		}
		AssignedVariablesList	{	name:	"fixedFactors";		title: qsTr("Fixed Factors");		allowedColumns: ["nominal"]; minLevels: 2 }
	}

	Section
	{
		property alias	source:				availableTerms.source
		property bool	enableHorizontal:	true
		property bool	enableYAxisLabel:	false
		property var	allowed:			(enableYAxisLabel) ? [] : ["nominal"]
		
		title: qsTr("Raincloud Plots")

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "rainCloudAvailableFactors";		title: qsTr("Factors"); id: availableTerms }
			AssignedVariablesList	{ name: "rainCloudHorizontalAxis";	title: qsTr("Horizontal Axis"); singleVariable: true; allowedColumns: allowed }
			AssignedVariablesList	{ name: "rainCloudSeparatePlots";	title: qsTr("Separate Plots");	singleVariable: true; allowedColumns: allowed }
		}

		CheckBox
		{
			name: "rainCloudHorizontalDisplay";
			label: qsTr("Horizontal display");
			visible: enableHorizontal 
		}
		
	}

}
