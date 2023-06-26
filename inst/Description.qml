import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspLearnScience"
	title		: qsTr("Learn Science")
	description	: qsTr("This module offers analyses.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	Analysis
	{
		title:	qsTr("Galileo")
		func:	"Galileo"
	}
}
