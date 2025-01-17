/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import * as Yup from "yup"
import {getString} from "../../../utils/TekstPropertyUtil"
import properties from "../../../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage.json"
import {useSelector} from "react-redux"
import {State} from "../../../datatypes/State"
import {AfspraakZoekFilter} from "../../../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage"
import {Formik, FormikProps} from "formik"
import classNames from "classnames"
import bvoStyle from "../../BvoStyle.module.scss"
import ScreenitDatePicker from "../../input/ScreenitDatePicker"
import {ArrowType} from "../../vectors/ArrowIconComponent"
import Button from "../../input/Button"
import React, {useEffect, useRef, useState} from "react"
import ScreenitDropdown, {DropdownOption} from "../../input/ScreenitDropdown"
import styles from "./MammaAfspraakMakenForm.module.scss"
import AdvancedSearchLinkComponent from "../AdvancedSearchLinkComponent"
import ScreenitBackend from "../../../utils/Backend"
import {CircularProgress} from "@mui/material"
import SearchForm from "../SearchForm"
import {AxiosResponse} from "axios"
import {lijstBevatMeegegevenDatum} from "../../../utils/DateUtil"
import {isValid} from "date-fns"

export type MammaAfspraakMakenFormProps = {
	zoekFilter: AfspraakZoekFilter
	laatsteStandplaatsZoekFilter: string[]
	onSubmitSucces: (value: AfspraakZoekFilter, beschikbareDagen: Date[]) => void
	setDagenBeschikbaar: (value: boolean) => void
	onChange: () => void
	onZoekenClick?: () => void;
}

const standplaatsOpties = (laatsteStandplaatsPlaatsZoekFilter: string[]) => {
	const standplaatsOpties: Array<DropdownOption> = []
	for (const standplaats of laatsteStandplaatsPlaatsZoekFilter) {
		standplaatsOpties.push({value: standplaats, label: standplaats})
	}
	return standplaatsOpties
}

const afstandOpties = () => {
	const afstanden = ["5", "10", "15", "20", "25", "30", "35", "40", "45"]
	const afstandOpties: Array<DropdownOption> = []
	for (const afstand of afstanden) {
		afstandOpties.push({value: afstand, label: afstand + " km"})
	}
	return afstandOpties
}

const MammaAfspraakMakenForm = (props: MammaAfspraakMakenFormProps) => {
	const huidigeStandplaats = useSelector((state: State) => state.client.mammaDossier.laatsteStandplaatsPlaats)
	const [isAdvancedSearch, setAdvancedSearch] = useState<boolean>(false)
	const [beschikbareDagen, setBeschikbareDagen] = useState<Date[]>([])
	const [beschikbaarheidOpgehaald, setBeschikbaarheidOpgehaald] = useState<boolean>(false)
	const [dossierVerverst, setDossierVerverst] = useState<boolean>(false)
	const [gekozenPlaats, setGekozenPlaats] = useState<string | undefined>(huidigeStandplaats ? huidigeStandplaats : undefined)
	const [gekozenAfstand, setGekozenAfstand] = useState<string | undefined>(undefined)
	const formikRef = useRef<FormikProps<MammaAfspraakFormValues>>(null)

	useEffect(() => {
		beschikbaarheidOpgehaald && props.setDagenBeschikbaar(beschikbareDagen.length !== 0)
	}, [beschikbaarheidOpgehaald, beschikbareDagen, props])

	useEffect(() => {
		if (beschikbareDagen.length !== 0 && beschikbaarheidOpgehaald && formikRef.current) {
			formikRef.current.initialValues.vanaf = formikRef.current.values.vanaf = beschikbareDagen[0]
			formikRef.current.handleSubmit()
		}
	}, [beschikbaarheidOpgehaald, beschikbareDagen])

	useEffect(() => {
		if (huidigeStandplaats != null) {
			setGekozenPlaats(huidigeStandplaats)
			setDossierVerverst(true)
		}
	}, [huidigeStandplaats])

	useEffect(() => {
		if (dossierVerverst) {
			setBeschikbaarheidOpgehaald(false)
			const url = "/mamma/afspraak/beschikbaarheid" + (gekozenPlaats ? `/plaats` : `/afstand/${gekozenAfstand}`)

			ScreenitBackend.request({
				url: url,
				method: gekozenPlaats ? "POST" : "GET",
				data: gekozenPlaats && {plaats: gekozenPlaats},
			}).then((response: AxiosResponse<Date[]>) => {
				setBeschikbareDagen(response.data)
				setBeschikbaarheidOpgehaald(true)
			})
		}
	}, [dossierVerverst, gekozenPlaats, gekozenAfstand])

	type MammaAfspraakFormValues = {
		vanaf: Date,
		plaats?: string,
		afstand?: string,
		meerOpties: boolean
	}

	const initialValues: MammaAfspraakFormValues = {
		vanaf: props.zoekFilter.vanaf && gekozenPlaats === props.zoekFilter.plaats ? props.zoekFilter.vanaf : beschikbareDagen[0],
		plaats: getInitialPlaats(),
		afstand: gekozenAfstand ? gekozenAfstand : undefined,
		meerOpties: false,
	}

	function getInitialPlaats(): string | undefined {
		if (gekozenPlaats) {
			return gekozenPlaats
		} else if (gekozenAfstand) {
			return undefined
		} else {
			return huidigeStandplaats
		}
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object({
		vanaf: Yup.date().required(getString(properties.form.error.verplicht))
			.nullable()
			.typeError(getString(properties.form.error.type))
			.test("vanaf", getString(properties.form.error.beschikbaarheid), ((value) => {
				return !!value && lijstBevatMeegegevenDatum(beschikbareDagen, value)
			})),
	})

	return <Formik innerRef={formikRef}
				   enableReinitialize
				   initialValues={initialValues}
				   validationSchema={validatieSchema}
				   onSubmit={(values) => props.onSubmitSucces(values, beschikbareDagen)}>

		{({errors, values, initialValues, setFieldValue, handleSubmit}) => (
			<SearchForm className={styles.style} title={getString(properties.form.titel)}>
				<ScreenitDropdown propertyName={"plaats"}
								  invalidMessage={errors.plaats}
								  value={values.plaats}
								  initialValue={initialValues.plaats}
								  options={standplaatsOpties(props.laatsteStandplaatsZoekFilter)}
								  placeholder={getString(properties.form.placeholder.plaats)}
								  onChange={(event) => {
									  props.onChange()
									  setFieldValue("afstand", undefined)
									  setFieldValue("plaats", event.target.value)
									  setGekozenPlaats(event.target.value as string)
									  setGekozenAfstand(undefined)
								  }}/>

				{beschikbaarheidOpgehaald && <ScreenitDatePicker className={styles.datepicker}
																 propertyName={"vanaf"}
																 label={getString(properties.form.placeholder.datum)}
																 value={values.vanaf}
																 beschikbareDagen={beschikbareDagen}
																 errorLabel={errors.vanaf}
																 onChange={value => {
																	 setFieldValue("vanaf", value)
																	 value && isValid(value) && handleSubmit()
																 }}/>}
				{!beschikbaarheidOpgehaald && <CircularProgress/>}

				<div className={classNames(styles.advancedSearchButton)}
					 onClick={() => {
						 setFieldValue("afstand", undefined)
						 setAdvancedSearch(!isAdvancedSearch)
					 }}>
					<AdvancedSearchLinkComponent advancedSearch={isAdvancedSearch}/>
				</div>

				{isAdvancedSearch && <ScreenitDropdown propertyName={"afstand"}
													   invalidMessage={errors.afstand}
													   value={values.afstand}
													   initialValue={initialValues.afstand}
													   options={afstandOpties()}
													   placeholder={getString(properties.form.placeholder.afstand)}
													   onChange={(event) => {
														   props.onChange()
														   setFieldValue("afstand", event.target.value)
														   setFieldValue("plaats", undefined)
														   setGekozenAfstand(event.target.value as string)
														   setGekozenPlaats(undefined)
													   }}/>}

				<Button className={bvoStyle.darkBackgroundColor}
						label={getString(properties.form.button)}
						displayArrow={ArrowType.ARROW_RIGHT}
						onClick={() => {
							if (beschikbaarheidOpgehaald) {
								handleSubmit()
								props.onZoekenClick && props.onZoekenClick()
							}
						}}/>
			</SearchForm>
		)}
	</Formik>
}

export default MammaAfspraakMakenForm
