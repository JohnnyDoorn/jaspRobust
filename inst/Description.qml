import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Robust Statistics")
	icon		: "analysis-classical-anova.svg"
	description	: qsTr("Robust versions of classical statistical analyses")
	hasWrappers	: true
	preloadData : true

	GroupTitle
	{
		title:  qsTr("Robust Classical Analyses");
		icon:	"analysis-classical-anova.svg"
	}

	Analysis { title: qsTr("Robust ANOVA");					func:	"AnovaRobust"				}
	Analysis { title: qsTr("Robust Repeated Measures ANOVA");	func:	"AnovaRepeatedMeasuresRobust"	}
	Analysis { title: qsTr("Robust ANCOVA");					func:	"AncovaRobust"				}
	Analysis { title: qsTr("Robust Linear Regression");		func:	"RegressionLinearRobust"			}
	Analysis { title: qsTr("Robust Correlation");			func:	"CorrelationRobust"				}
}