// Copyright (C) 2013-2023 University of Amsterdam
//
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

//	Formula
//	{
//		lhs: "dependent"
//		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
//	}

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");		suggestedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "covariate";	title: qsTr("Predictor");				suggestedColumns: ["scale"];	singleVariable: true	}
	}

	BayesFactorType {}


	Group
	{
		title: qsTr("Plots")

	}

	// should be superseded by Model section
	IntegerField { name: "maxDegree"; min: 1; max: 10; defaultValue: 3; label: qsTr("Maximum degree") }

	Section
	{
		title: qsTr("Model")

	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "inclusionProbabilitiesPlot";	label: qsTr("Inclusion probabilities")			}
			CheckBox { name: "marginalPosteriorPlot";		label: qsTr("Marginal posterior distributions")	}
		}

		Group
		{
			title: qsTr("Predictive distributions")
			CheckBox { name: "priorPredictivesPlot";		label: qsTr("Prior predictive distribution")		}
			CheckBox { name: "posteriorPredictivesPlot";	label: qsTr("Posterior predictive distribution")	}
		}

		Group
		{
			title: qsTr("Models")
			CheckBox { name: "logPosteriorOddsPlot";	label: qsTr("Log posterior odds")				}
			CheckBox { name: "modelComplexityPlot";		label: qsTr("Log(P(data|M)) vs. model size")	}
			CheckBox { name: "modelProbabilitiesPlot";	label: qsTr("Model probabilities")				}
		}

		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsVsFittedPlot";	label: qsTr("Residuals vs. fitted")					}
			CheckBox { name: "qqPlot";				label: qsTr("Q-Q plot of model averaged residuals")	}
		}
	}

	Section
	{
		title: qsTr("Advanced Options")

		RadioButtonGroup
		{
			name: "priorRegressionCoefficients"
			title: qsTr("Prior")

			GridLayout
			{
				rowSpacing: jaspTheme.rowGroupSpacing
				columnSpacing: 0
				Group
				{
					RadioButton { value: "gPrior";			label: qsTr("g-prior");				id: gprior			}
				}
				DoubleField
				{
					name: "gPriorAlpha"
					label: qsTr("alpha")
					enabled: gprior.checked || hyperg.checked || hyperglaplace.checked || hypergn.checked
					defaultValue: 3.0
					min: 2
					max: 4
					inclusive: JASP.None
				}
				RadioButton { value: "jzs"; label: qsTr("JZS"); checked: true; id: jzs }
				DoubleField
				{
					name: "jzsRScale"
					label: qsTr("r scale")
					enabled: jzs.checked
					fieldWidth: 50
					defaultValue: 0.354
					max: 100000
					inclusive: JASP.MaxOnly
				}
			}
		}

		ColumnLayout
		{

			RadioButtonGroup
			{
				name: "samplingMethod"
				title: qsTr("Sampling Method")
				RadioButton
				{
					value: "bas"; label: qsTr("BAS"); checked: true
					childrenOnSameRow: true
					IntegerField { name: "numberOfModels"; label: qsTr("No. models"); defaultValue: 0; max: 100000000 }
				}
				RadioButton
				{
					value: "mcmc"; label: qsTr("MCMC")
					childrenOnSameRow: true
					IntegerField { name: "samples"; label: qsTr("No. samples"); defaultValue: 0; max: 100000000 }
				}
			}

			Group
			{
				title: qsTr("Numerical Accuracy")
				IntegerField
				{
					name: "numericalAccuracy"
					label: qsTr("No. samples for credible interval")
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					max: 1000000
				}
			}

			SetSeed{}

		}
	}

}
