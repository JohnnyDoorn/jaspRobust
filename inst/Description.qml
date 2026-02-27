import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspRobust"
	title		: qsTr("Jasp Robust")
	description	: qsTr("This module offers robust analyses.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-robust-statistics.svg"


	Analysis { title: qsTr("Robust ANOVA");					func:	"AnovaRobust"		}
	Analysis { title: qsTr("Robust ANCOVA");				func:	"AncovaRobust"		}
}


