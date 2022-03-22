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
import React, {useEffect, useState} from "react"
import ActieBasePage from "../../ActieBasePage"
import styles from "./AfmeldenPage.module.scss"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import {useThunkDispatch} from "../../../index"
import {saveNieuwAfmeldMoment} from "../../../api/AfmeldenThunkAction"
import {AfmeldType} from "../../../datatypes/afmelden/AfmeldType"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import {getString} from "../../../utils/TekstPropertyUtil"
import {splitEnumString} from "../../../utils/EnumUtil"
import {useSelectedBvo} from "../../../utils/Hooks"
import {Field, Formik, FormikValues} from "formik"
import {RadioGroup} from "formik-material-ui"
import {FormControl, FormControlLabel, Radio} from "@material-ui/core"
import SubmitForm from "../../../components/form/SubmitForm"
import * as Yup from "yup"
import {AfmeldOptiesDto, geenAfmeldOpties} from "../../../datatypes/afmelden/AfmeldOptiesDto"
import ScreenitBackend from "../../../utils/Backend"
import LadenComponent from "../../../components/laden/LadenComponent"
import {AfmeldingDto} from "../../../datatypes/afmelden/AfmeldingDto"
import {CervixAfmeldingReden} from "../../../datatypes/afmelden/CervixAfmeldingReden"
import {getBvoBaseUrl} from "../../../utils/UrlUtil"
import {useNavigate} from "react-router-dom"
import {showToast} from "../../../utils/ToastUtil"

const AfmeldenPage = () => {
	const selectedBvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const properties = require("./AfmeldenPage.json")
	const [afmeldOpties, setAfmeldOpties] = useState<AfmeldOptiesDto>(geenAfmeldOpties)

	useEffect(() => {
		ScreenitBackend.get(`/afmelden/${selectedBvo}`)
			.then((response) => {
				setAfmeldOpties(response.data)
			})
	}, [setAfmeldOpties, selectedBvo])

	const magEenmaligAfmelden = afmeldOpties.afmeldOpties.includes(AfmeldType.EENMALIG)
	const magDefinitiefAfmelden = afmeldOpties.afmeldOpties.includes(AfmeldType.DEFINITIEF)

	const initialValues = {
		afmeldType: magDefinitiefAfmelden && !magEenmaligAfmelden ? AfmeldType.DEFINITIEF : undefined,
		afmeldReden: undefined,
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		afmeldType: Yup.string().required(getString(properties.form.error)),
		afmeldReden: selectedBvo === Bevolkingsonderzoek.CERVIX ? Yup.string().when("afmeldType", {
			is: AfmeldType.DEFINITIEF,
			then: Yup.string().required(getString(properties.form.error)),
		}) : Yup.string().required(getString(properties.form.error)),
	})

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.title.afmelden, [BevolkingsonderzoekNaam[selectedBvo]])}
			description={getString(properties.infoText)}
			hintEinde={getString(properties.hintText)}>

			{!magEenmaligAfmelden && !magDefinitiefAfmelden && <LadenComponent/>}

			<div className={styles.uitlegDiv}>
				<ul>
					{magEenmaligAfmelden && <div>
						<li>{getString(properties.afmeldType.eenmalig.text)}</li>
						<p>{getString(properties.afmeldType.eenmalig.info)}</p>
					</div>}
					{magDefinitiefAfmelden && <div>
						<li>{getString(properties.afmeldType.definitief.text)}</li>
						<p>{getString(properties.afmeldType.definitief.info)}</p>
					</div>}
				</ul>
			</div>

			{afmeldOpties?.heeftOpenColonIntakeAfspraak && <FormErrorComponent text={getString(properties.errorIntakeAfspraak)}/>}

			{(magEenmaligAfmelden || magDefinitiefAfmelden) &&
				<Formik initialValues={initialValues}
						enableReinitialize={true}
						validationSchema={validatieSchema}
						onSubmit={(values) => {
							dispatch(saveNieuwAfmeldMoment(selectedBvo,
								getAfmeldingDto(values),
							)).then(() => {
								showToast(undefined,
									values.afmeldType === AfmeldType.EENMALIG ?
										getString(properties.toast.eenmalig, [BevolkingsonderzoekNaam[selectedBvo]]) :
										getString(properties.toast.definitief, [BevolkingsonderzoekNaam[selectedBvo]]))
								navigate(getBvoBaseUrl(selectedBvo))
							})
						}}>
					{formikProps => (
						<SubmitForm title={magEenmaligAfmelden ? getString(properties.form.title.afmeldType.algemeen) : getString(properties.form.title.afmeldType.definitief)}
									formikProps={formikProps}
									buttonLabel={getString(properties.form.submit)}>

							<FormControl
								required
								component="fieldset">

								<p data-testid={"error_geen_keuze_afmeldtype"} className={styles.errorLabel}>{formikProps.errors.afmeldType}</p>

								<Field
									className={styles.radiobuttons}
									name="afmeldType"
									component={RadioGroup}
									value={formikProps.values.afmeldType || ""}
									onClick={() => {
										formikProps.setFieldValue("afmeldReden", "")
									}}>
									<ul>
										{magEenmaligAfmelden && <li><FormControlLabel
											value={AfmeldType.EENMALIG}
											data-testid={"radio_eenmalig"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.eenmalig)}/></li>}
										<li><FormControlLabel
											value={AfmeldType.DEFINITIEF}
											data-testid={"radio_definitief"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.definitief)}/></li>
									</ul>
								</Field>

								{(formikProps.values.afmeldType !== undefined || !magEenmaligAfmelden) && afmeldTypeHeeftAfmeldingRedenen(formikProps.values.afmeldType) &&
									<div>
										<h3 className={styles.label}>{getString(properties.form.title.afmeldReden)}</h3>
										<p data-testid={"error_geen_keuze_afmeldreden"} className={styles.errorLabel}>{formikProps.errors.afmeldReden}</p>
										<Field
											className={styles.radiobuttons}
											name="afmeldReden"
											value={formikProps.values.afmeldReden || ""}
											component={RadioGroup}>
											<ul>
												{getAfmeldRedenen(formikProps.values.afmeldType)
													.map((reden: any, index) => {
														return <li key={index}>
															<FormControlLabel key={index}
																			  data-testid={"radio_" + reden}
																			  control={<Radio/>}
																			  className={styles.afmeldRedenRadioButton}
																			  value={splitEnumString(reden) ?? ""}
																			  label={getString(properties[reden]) ?? ""}/>
														</li>
													})}
											</ul>
										</Field>
									</div>}
							</FormControl>
						</SubmitForm>)}
				</Formik>}

		</ActieBasePage>
	)

	function getAfmeldingDto(values: FormikValues): AfmeldingDto {
		if (selectedBvo === Bevolkingsonderzoek.CERVIX && values.afmeldType === AfmeldType.EENMALIG) {
			return {
				afmeldType: values.afmeldType!,
				afmeldReden: CervixAfmeldingReden.ANDERS,
			}
		} else {
			return {
				afmeldType: values.afmeldType!,
				afmeldReden: values.afmeldReden!,
			}
		}
	}

	function afmeldTypeHeeftAfmeldingRedenen(afmeldType?: AfmeldType) {
		return !(selectedBvo === Bevolkingsonderzoek.CERVIX && afmeldType === AfmeldType.EENMALIG)
	}

	function getAfmeldRedenen(afmeldType?: AfmeldType) {
		return AfmeldType.EENMALIG === afmeldType ? afmeldOpties.afmeldRedenenEenmalig : afmeldOpties.afmeldRedenenDefinitief
	}
}

export default AfmeldenPage
