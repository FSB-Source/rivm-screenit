/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React, {useCallback, useState} from "react"
import {BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import {Col, Row} from "react-bootstrap"
import styles from "./MammaAfspraakUitstellenPage.module.scss"
import {geenStandplaatsPeriodeZoekResultaten, KandidaatStandplaatsPeriode} from "../../../../datatypes/mamma/KandidaatStandplaatsPeriode"
import ScreenitBackend from "../../../../utils/Backend"
import {useThunkDispatch} from "../../../../index"
import BasePage from "../../../BasePage"
import SearchResultAfspraken from "../../../../components/search_results/SearchResultAfspraken"
import {formatDate, formatDateText, formatDateWithDayName, isWerkdag, plusDagen, plusWerkdagen, vandaag} from "../../../../utils/DateUtil"
import {FormErrorComponent} from "../../../../components/form_error/FormErrorComponent"
import BeforeSearching from "../../../../components/search_results/BeforeSearching"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {maakUitstel} from "../../../../api/MammaUitstelThunkAction"
import {concatWithSpace} from "../../../../utils/StringUtil"
import properties from "./MammaAfspraakUitstellenPage.json"
import {getContactUrl} from "../../../../utils/UrlUtil"
import {useRegio} from "../../../../utils/Hooks"
import BigUrlButton from "../../../../components/bigUrlButton/BigUrlButton"
import * as Yup from "yup"
import {Formik} from "formik"
import classNames from "classnames"
import bvoStyle from "../../../../components/BvoStyle.module.scss"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import Button from "../../../../components/input/Button"
import SearchForm from "../../../../components/form/SearchForm"
import {useNavigate} from "react-router-dom"
import ScreenitDatePicker from "../../../../components/input/ScreenitDatePicker"

export type UitstelZoekFilter = {
	vanaf: Date | null,
	meerOpties?: boolean
}

const MammaAfspraakUitstellenPage = () => {
	const dispatch = useThunkDispatch()
	const regio = useRegio()
	const navigate = useNavigate()
	const [gezocht, setGezocht] = useState<boolean>(false)
	const [geenResultaten, setGeenResultaten] = useState<boolean>(false)
	const [zoekResultaten, setZoekResultaten] = useState<KandidaatStandplaatsPeriode[]>(geenStandplaatsPeriodeZoekResultaten)
	const [zoekFilter, setZoekFilter] = useState<UitstelZoekFilter>({
		vanaf: null,
		meerOpties: false,
	})

	const zoekStandplaatsPeriodes = useCallback((filter: UitstelZoekFilter) => {
		return ScreenitBackend.post(`mamma/uitstel/zoeken`, filter)
			.then(response => {
				setGeenResultaten(response.data.length === 0)
				setZoekResultaten(response.data)
			})
	}, [setZoekResultaten])

	const zoek = useCallback((filter: UitstelZoekFilter) => {
		const nieuwZoekFilter = {
			...filter,
			meerOpties: false,
		}
		setZoekFilter(nieuwZoekFilter)
		setGezocht(true)
		zoekStandplaatsPeriodes(filter)
	}, [zoekStandplaatsPeriodes])

	const initialValues = {
		vanaf: plusWerkdagen(vandaag(), 1),
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		vanaf: Yup.date().required(getString(properties.form.error.verplicht))
			.nullable()
			.typeError(getString(properties.form.error.ongeldig))
			.min(plusDagen(vandaag(), 1), getString(properties.form.error.minimum, [formatDate(vandaag())]))
			.test("vanaf", getString(properties.form.error.werkdag), ((value) => {
				return !!value && isWerkdag(value)
			})),
	})

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.MAMMA}
				  title={getString(properties.page.title)}
				  description={getString(properties.page.description)}
				  toonBlob={false}>
			<Row>
				<Col md={5}>
					<Formik initialValues={initialValues}
							validationSchema={validatieSchema}
							onSubmit={zoek}>
						{formikProps => (<SearchForm title={getString(properties.form.title)}>
							<div>{getString(properties.form.toelichting)}</div>
							<ScreenitDatePicker className={styles.datepicker}
												propertyName={"vanaf"}
												label={getString(properties.form.placeholder)}
												alleenWerkdagen={true}
												errorLabel={formikProps.errors.vanaf}
												value={formikProps.values.vanaf}
												onChange={value => {
													formikProps.setFieldValue("vanaf", value)
												}}/>
							<Button className={classNames(bvoStyle.darkBackgroundColor, styles.button)}
									label={getString(properties.form.button)}
									displayArrow={ArrowType.ARROW_RIGHT}
									onClick={formikProps.handleSubmit}/>

						</SearchForm>)}
					</Formik>
				</Col>
				<Col md={7} className={styles.results}>
					{!gezocht &&
						<BeforeSearching text={getString(properties.beforesearching_text)}/>}
					{gezocht && zoekResultaten.map((kandidaatStandplaatsPeriode, index) =>
						<SearchResultAfspraken
							key={index}
							className={styles.result}

							col1={[getString(properties.searchresult.title.locatie), kandidaatStandplaatsPeriode.adres, concatWithSpace(kandidaatStandplaatsPeriode.postcode, kandidaatStandplaatsPeriode.plaats)]}
							enlargeText={false}
							col3={[getString(properties.searchresult.title.periode), getString(properties.searchresult.periode_text.vanaf, [formatDateText(kandidaatStandplaatsPeriode.startPeriode)]), getString(properties.searchresult.periode_text.tot, [formatDateText(kandidaatStandplaatsPeriode.eindPeriode)])]}

							onHoverText={getString(properties.searchresult.hovertext)}
							onClickAction={() => {
								kandidaatStandplaatsPeriode.filter = zoekFilter
								dispatch(maakUitstel(kandidaatStandplaatsPeriode)).then(() => navigate("/mamma"))
							}}/>,
					)}
					{geenResultaten && <FormErrorComponent text={getString(properties.searchresult.no_results, [formatDateWithDayName(zoekFilter.vanaf)])}/>}
					<BigUrlButton title={getString(properties.navigation.afspraak.header)} text={getString(properties.navigation.afspraak.text)}
								  link={"/mamma/afspraak"}/>

					{(gezocht && geenResultaten) &&
						<BigUrlButton title={getString(properties.navigation.contact.header)}
									  text={getString(properties.navigation.contact.text)}
									  link={getContactUrl(regio)}/>}
				</Col>
			</Row>
		</BasePage>

	)
}

export default MammaAfspraakUitstellenPage
