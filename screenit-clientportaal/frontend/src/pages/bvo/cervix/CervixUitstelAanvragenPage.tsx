/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import React, {useEffect} from "react"
import {useSelectedBvo} from "../../../utils/Hooks"
import {BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import ActieBasePage from "../../ActieBasePage"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import styles from "./CervixUitstelAanvragenPage.module.scss"
import {getString} from "../../../utils/TekstPropertyUtil"
import {CervixUitstelType} from "../../../datatypes/cervix/CervixUitstelType"
import {useThunkDispatch} from "../../../index"
import * as Yup from "yup"
import {Field, Formik} from "formik"
import {FormControl, FormControlLabel, Radio} from "@material-ui/core"
import {getHuidigeCervixUitstel, getHuidigeCervixUitstelStatus, getUitstellenTotDatum, saveCervixUitstel} from "../../../api/CervixUitstelThunkAction"
import SubmitForm from "../../../components/form/SubmitForm"
import {CervixUitstelFormulier} from "../../../datatypes/cervix/CervixUitstelDto"
import {RadioGroup} from "formik-material-ui"
import ScreenitDatePicker from "../../../components/input/ScreenitDatePicker"
import properties from "./CervixUitstelAanvragenPage.json"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {
	formatDate,
	formatDateText,
	getAantalDagenTussenDatums,
	getBovengrensUitLijst,
	minDagen,
	parseIsoDatumNederlandseIso,
	plusDagen,
	plusMaanden,
	vandaag,
} from "../../../utils/DateUtil"
import {useNavigate} from "react-router-dom"
import {showToast} from "../../../utils/ToastUtil"

const CervixUitstelAanvragenPage = () => {
	const selectedBvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const cervixUitstel = useSelector((state: State) => state.client.cervixDossier.uitstel)
	const uitstelStatus = useSelector((state: State) => state.client.cervixDossier.uitstelStatus)
	const geboortedatumDisplay = useSelector((state: State) => state.client.persoon.geboortedatumDisplay)

	useEffect(() => {
		dispatch(getHuidigeCervixUitstelStatus())
		dispatch(getHuidigeCervixUitstel())
	}, [dispatch])

	const geboortedatum = parseIsoDatumNederlandseIso(geboortedatumDisplay)
	const datumVanDertigsteVerjaardag = new Date(geboortedatum.setFullYear(geboortedatum.getFullYear() + 30))
	const duurZwangerschap = 9
	const periodeTussenVandaagEnDertigsteVerjaardag = getAantalDagenTussenDatums(datumVanDertigsteVerjaardag, vandaag())

	const minPeriodeTussenZwangerEnOnderzoek = uitstelStatus ? uitstelStatus.uitstelBijZwangerschap : 42
	const periodeTussenZwangerschapEnDertigsteVerjaardag = periodeTussenVandaagEnDertigsteVerjaardag > 0 ? minPeriodeTussenZwangerEnOnderzoek - periodeTussenVandaagEnDertigsteVerjaardag : minPeriodeTussenZwangerEnOnderzoek

	const minDatumZwangerschap = minDagen(vandaag(), periodeTussenZwangerschapEnDertigsteVerjaardag)
	const maxDatumZwangerschap = plusMaanden(vandaag(), duurZwangerschap)
	const minDatumAnders = getBovengrensUitLijst([plusDagen(datumVanDertigsteVerjaardag, 1), plusDagen(vandaag(), 1)])
	const maxDatumAnders = uitstelStatus.datumVolgendeRonde != null ? uitstelStatus.datumVolgendeRonde : plusMaanden(vandaag(), 60)

	function getInitialValueUitstellenTotDatum() {
		if (cervixUitstel.uitstelType === CervixUitstelType.ZWANGERSCHAP && cervixUitstel.uitstellenTotDatum) {
			return minDagen(cervixUitstel.uitstellenTotDatum, minPeriodeTussenZwangerEnOnderzoek)
		} else {
			return cervixUitstel.uitstellenTotDatum || null
		}
	}

	const initialValues: CervixUitstelFormulier = {
		uitstelType: cervixUitstel.uitstelType,
		uitgerekendeDatum: getInitialValueUitstellenTotDatum(),
		uitstellenTotDatum: getInitialValueUitstellenTotDatum(),
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		uitstelType: Yup.string().required(getString(properties.form.error.verplicht.radiobutton)),
		uitgerekendeDatum: Yup.date().when("uitstelType", {
			is: CervixUitstelType.ZWANGERSCHAP,
			then: Yup.date().required(getString(properties.form.error.verplicht.datepicker))
				.nullable()
				.typeError(getString(properties.form.error.datum.ongeldig))
				.min(minDatumZwangerschap, getString(properties.form.error.datum.minimum, [formatDate(minDatumZwangerschap)]))
				.max(maxDatumZwangerschap, getString(properties.form.error.datum.maximum, [formatDate(maxDatumZwangerschap)])),
			otherwise: Yup.date().nullable(),
		}),
		uitstellenTotDatum: Yup.date().when("uitstelType", {
			is: CervixUitstelType.ANDERS,
			then: Yup.date().required(getString(properties.form.error.verplicht.datepicker))
				.nullable()
				.typeError(getString(properties.form.error.datum.ongeldig))
				.min(minDatumAnders, getString(properties.form.error.datum.minimum, [formatDate(minDatumAnders)]))
				.max(maxDatumAnders, getString(properties.form.error.datum.maximum, [formatDate(maxDatumAnders)])),
			otherwise: Yup.date().nullable(),
		}),
	})

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.page.title)}
			description={getString(properties.page.description)}>

			{cervixUitstel.isInSync && getZasAangevraagdError()}

			{cervixUitstel.isInSync &&
				<Formik<CervixUitstelFormulier> initialValues={initialValues}
												validationSchema={validatieSchema}
												onSubmit={(values) => {
													dispatch(saveCervixUitstel(values, minPeriodeTussenZwangerEnOnderzoek)).then(() => {
														showToast(getString(properties.toast.title, [formatDateText(getUitstellenTotDatum(
															values, minPeriodeTussenZwangerEnOnderzoek))]), getString(properties.toast.description))
														navigate("/cervix")
													})
												}}>
					{formikProps => (
						<SubmitForm<CervixUitstelFormulier> title={getString(properties.form.title)}
															formikProps={formikProps}
															buttonLabel={getString(properties.form.button)}>
							<FormControl
								required
								component="fieldset">
								<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{formikProps.errors.uitstelType}</p>
								<Field
									name="uitstelType"
									component={RadioGroup}
									value={formikProps.values.uitstelType || ""}>
									<ul>
										<li className={styles.radioGroup}>
											<FormControlLabel
												value={CervixUitstelType.ZWANGERSCHAP}
												data-testid={"radio_zwangerschap"}
												control={<Radio/>}
												label={getString(properties.form.radiobutton.zwanger)}/>
											<FormControlLabel
												value={CervixUitstelType.ANDERS}
												data-testid={"radio_anders"}
												control={<Radio/>}
												label={getString(properties.form.radiobutton.anders)}/>
										</li>
									</ul>
								</Field>
							</FormControl>

							{formikProps.values.uitstelType === CervixUitstelType.ZWANGERSCHAP &&
								<ScreenitDatePicker className={styles.datepicker}
													propertyName={"uitgerekendeDatum"}
													label={getString(properties.form.placeholder.zwanger)}
													title={getString(properties.form.label.zwanger)}
													value={formikProps.values.uitgerekendeDatum}
													errorLabel={formikProps.errors.uitgerekendeDatum}
													onChange={value => {
														formikProps.setFieldValue("uitgerekendeDatum", value)
													}}/>}

							{formikProps.values.uitstelType === CervixUitstelType.ANDERS &&
								<ScreenitDatePicker className={styles.datepicker}
													propertyName={"uitstellenTotDatum"}
													label={getString(properties.form.placeholder.anders)}
													title={getString(properties.form.label.anders)}
													value={formikProps.values.uitstellenTotDatum}
													errorLabel={formikProps.errors.uitstellenTotDatum}
													onChange={value => {
														formikProps.setFieldValue("uitstellenTotDatum", value)
													}}/>}
						</SubmitForm>)}
				</Formik>}
		</ActieBasePage>
	)

	function getZasAangevraagdError() {
		return (uitstelStatus && uitstelStatus.zasAanvraagInBehandeling) &&
			<FormErrorComponent text={getString(properties.error.zas)}/>
	}
}

export default CervixUitstelAanvragenPage
