/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React, {useCallback, useEffect, useRef, useState} from "react"
import {BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import styles from "./ColonAfspraakMakenPage.module.scss"
import ScreenitDropdown, {DropdownOption} from "../../../../components/input/ScreenitDropdown"
import BasePage from "../../../BasePage"
import SearchResultAfspraken from "../../../../components/search_results/SearchResultAfspraken"
import * as Yup from "yup"
import {FormErrorComponent} from "../../../../components/form_error/FormErrorComponent"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import BeforeSearching from "../../../../components/search_results/BeforeSearching"
import ColonAfspraakMakenBevestigingsPopup from "./ColonAfspraakMakenBevestigingsPopup"
import BigUrlButton from "../../../../components/bigUrlButton/BigUrlButton"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {getHuidigeIntakeAfspraak} from "../../../../api/ColonAfspraakAfzeggenThunkAction"
import {useSelector} from "react-redux"
import {State} from "../../../../datatypes/State"
import {AxiosResponse} from "axios"
import {Formik} from "formik"
import bvoStyle from "../../../../components/BvoStyle.module.scss"
import ScreenitBackend from "../../../../utils/Backend"
import {useThunkDispatch} from "../../../../index"
import {formatDate, formatDateWithDayName, formatTime, plusDagen, plusMaanden, plusWerkdagen, vandaag} from "../../../../utils/DateUtil"
import properties from "./ColonAfspraakMakenPage.json"
import ScreenitTextfield from "../../../../components/input/ScreenitTextfield"
import {createShowToastAction} from "../../../../actions/ToastAction"
import {ToastMessageType} from "../../../../datatypes/toast/ToastMessage"
import {Col, Row} from "react-bootstrap"
import {placeNonBreakingSpaceInDate, splitAdresString} from "../../../../utils/StringUtil"
import {getContactUrl} from "../../../../utils/UrlUtil"
import {useRegio, useWindowDimensions} from "../../../../utils/Hooks"
import AdvancedSearchLinkComponent from "../../../../components/form/AdvancedSearchLinkComponent"
import SearchForm from "../../../../components/form/SearchForm"
import ScreenitDatePicker from "../../../../components/input/ScreenitDatePicker"
import {useLocation} from "react-router-dom"

export type VrijSlotZonderKamerFilter = {
	ziekenhuisnaam?: string,
	vanaf?: Date,
	totEnMet?: Date,
	plaats?: string,
	pagecount: number
	afstand?: number
	maxResultsPerSearchInteration?: number
}
export type VrijSlotZonderKamer = {
	startTijd: Date,
	eindTijd: Date,
	intakelocatieid: number,
	plaats: string,
	afstand: string,
	adres: string,
	postcode: string,
	ziekenhuis: string,
}

const afstandOpties = () => {
	const afstanden = ["5", "10", "15", "20", "25", "30", "35", "40", "45"]
	const afstandOpties: Array<DropdownOption> = []
	for (const afstand of afstanden) {
		afstandOpties.push({value: afstand, label: afstand + " km"})
	}
	return afstandOpties
}

const ColonAfspraakMakenPage = () => {
	const [isNieuweAfspraak, setIsNieuweAfspraak] = useState<boolean>(false)
	const [vrijeSloten, setVrijeSloten] = useState<VrijSlotZonderKamer[]>([])
	const regio = useRegio()
	const dispatch = useThunkDispatch()
	const huidigeIntakeAfspraak = useSelector((state: State) => state.client.colonDossier.intakeAfspraak)
	const [gezocht, setGezocht] = useState<boolean>(false)
	const [gekozenAfspraak, setGekozenAfspraak] = useState<VrijSlotZonderKamer | undefined>(undefined)
	const [isAdvancedSearch, setAdvancedSearch] = useState<boolean>(false)
	const [zoekFilter, setZoekFilter] = useState<VrijSlotZonderKamerFilter>({
		pagecount: 1,
	})
	const minWeekDaysBeforeDateSelection = 3
	const maxResultsPerSearchInteration = 10
	const [visibleZoekMeerButton, setVisibleZoekMeerButton] = useState<boolean>(true)
	const {width} = useWindowDimensions()
	const gevondenAfsprakenDiv = useRef<HTMLDivElement | null>(null)
	const afspraakNaHeraanmelding = useLocation().pathname.includes("heraanmelding")

	useEffect(() => {
		dispatch(getHuidigeIntakeAfspraak())
	}, [dispatch])

	useEffect(() => {
		setVisibleZoekMeerButton(vrijeSloten.length === zoekFilter.pagecount * maxResultsPerSearchInteration)
	}, [setVisibleZoekMeerButton, vrijeSloten, zoekFilter.pagecount])

	useEffect(() => {
		if (huidigeIntakeAfspraak && (huidigeIntakeAfspraak.afspraakAfgezegd || huidigeIntakeAfspraak.andereIntakelocatieOpVerzoekClient)) {
			setIsNieuweAfspraak(huidigeIntakeAfspraak.afspraakAfgezegd || huidigeIntakeAfspraak.andereIntakelocatieOpVerzoekClient)
		}
	}, [huidigeIntakeAfspraak])

	function getMinimumSearchDate() {
		return plusWerkdagen(vandaag(), minWeekDaysBeforeDateSelection)
	}

	const initialValues = {
		ziekenhuisnaam: undefined,
		vanaf: getMinimumSearchDate(),
		totEnMet: plusMaanden(getMinimumSearchDate(), 1),
		plaats: undefined,
		pagecount: 1,
		afstand: undefined,
		maxResultsPerSearchInteration: maxResultsPerSearchInteration,
	}

	const validationSchema: Yup.AnyObjectSchema = Yup.object()
		.shape({
			vanaf: Yup.date().required(getString(properties.searchitems.datefrom.error.verplicht))
				.nullable()
				.typeError(getString(properties.searchitems.datefrom.error.type))
				.min(getMinimumSearchDate(), getString(properties.searchitems.datefrom.error.date_too_early, [formatDate(getMinimumSearchDate())])),
			totEnMet: Yup.date().required(getString(properties.searchitems.datefrom.error.verplicht))
				.nullable()
				.typeError(getString(properties.searchitems.datefrom.error.type))
				.min(plusDagen(getMinimumSearchDate(), 1), getString(properties.searchitems.datetil.error_date_too_early, [formatDate(plusDagen(getMinimumSearchDate(), 1))])),
		})

	const zoekAfspraken = useCallback((filter: VrijSlotZonderKamerFilter) => {
		setZoekFilter(filter)
		return ScreenitBackend.post("/colon/afspraak/zoeken", filter)
			.then(
				(response: AxiosResponse<VrijSlotZonderKamer[]>) => {
					setVrijeSloten(response.data)
				},
			)
			.catch(() => {
				dispatch(createShowToastAction({
					title: getString(properties.toast.errors.zoeken.title),
					description: getString(properties.toast.errors.zoeken.message),
					type: ToastMessageType.ERROR,
					alGetoond: false,
				}))
			})
			.finally(() => {
				setGezocht(true)
			})
	}, [dispatch])

	const geenResultaten = vrijeSloten.length === 0 && gezocht

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.COLON}
				  toonBlob={!isNieuweAfspraak}
				  title={getString(!isNieuweAfspraak ? properties.page.title.verzetten : properties.page.title.maken)}
				  description={getString(!isNieuweAfspraak ? properties.page.description.verzetten :
					  afspraakNaHeraanmelding ? properties.page.description.heraangemeld + properties.page.description.maken :
						  properties.page.description.maken)}
				  blobTitle={!isNieuweAfspraak ? getString(properties.blob.title) : ""}
				  blobText={!isNieuweAfspraak && huidigeIntakeAfspraak ? getString(properties.blob.afspraak.moment, [huidigeIntakeAfspraak.weergaveAfspraakmoment]) : ""}
				  blobAdresLocatie={!isNieuweAfspraak && huidigeIntakeAfspraak ? getString(properties.blob.afspraak.locatie, [huidigeIntakeAfspraak.naamInstelling, splitAdresString(huidigeIntakeAfspraak.adresString)]) : ""}>

			<Row className={styles.style}>
				<Col md={5}>
					<Formik initialValues={initialValues}
							validationSchema={validationSchema}
							onSubmit={(values) => {
								zoekAfspraken(values as VrijSlotZonderKamerFilter)
								if (width <= 768 && gevondenAfsprakenDiv.current) {
									window.scrollTo(0, gevondenAfsprakenDiv.current.offsetTop - 100)
								}
							}}>
						{formikProps => (
							<SearchForm title={getString(properties.search.title)}>
								<ScreenitTextfield onChange={value => {
									formikProps.setFieldValue("plaats", value)
									formikProps.setFieldValue("afstand", "")
								}}
												   value={formikProps.values.plaats}
												   invalidMessage={formikProps.errors.plaats}
												   name={"plaats"}
												   placeholder={getString(properties.searchitems.plaats.placeholder)}/>

								<ScreenitTextfield onChange={value => {
									formikProps.setFieldValue("ziekenhuisnaam", value)
									formikProps.setFieldValue("afstand", "")
								}}
												   value={formikProps.values.ziekenhuisnaam}
												   invalidMessage={formikProps.errors.ziekenhuisnaam}
												   name={"ziekenhuisnaam"}
												   placeholder={getString(properties.searchitems.ziekenhuis.placeholder)}/>

								<ScreenitDatePicker className={styles.datepicker}
													propertyName={"vanaf"}
													label={getString(properties.searchitems.datefrom.placeholder)}
													value={formikProps.values.vanaf}
													errorLabel={formikProps.errors.vanaf}
													onChange={value => {
														formikProps.setFieldValue("vanaf", value)
													}}/>
								<ScreenitDatePicker className={styles.datepicker}
													propertyName={"totEnMet"}
													label={getString(properties.searchitems.datetil.placeholder)}
													value={formikProps.values.totEnMet}
													errorLabel={formikProps.errors.totEnMet}
													onChange={value => {
														formikProps.setFieldValue("totEnMet", value)
													}}/>
								<div>
									<div className={styles.advancedSearchButton}
										 onClick={() => {
											 formikProps.setFieldValue("afstand", "")
											 setAdvancedSearch(!isAdvancedSearch)
										 }}>
										<AdvancedSearchLinkComponent advancedSearch={isAdvancedSearch}/>
									</div>
								</div>
								{isAdvancedSearch &&
									<ScreenitDropdown propertyName={"afstand"}
													  invalidMessage={formikProps.errors.afstand}
													  value={formikProps.values.afstand}
													  options={afstandOpties()}
													  placeholder={getString(properties.searchitems.afstand.placeholder)}
													  onChange={(event) => {
														  formikProps.setFieldValue("afstand", event.target.value)
														  formikProps.setFieldValue("plaats", "")
														  formikProps.setFieldValue("ziekenhuisnaam", "")
													  }}/>}
								<Button className={bvoStyle.darkBackgroundColor}
										label={getString(properties.search.do_search)}
										displayArrow={ArrowType.ARROW_RIGHT}
										onClick={formikProps.handleSubmit}/>
							</SearchForm>)}
					</Formik>
				</Col>
				<Col md={7} className={styles.results} ref={gevondenAfsprakenDiv}>
					{gezocht && vrijeSloten.map((kiesbareLocatie, index) =>
						<SearchResultAfspraken
							key={index}
							className={styles.result}
							enlargeText

							col1={["", placeNonBreakingSpaceInDate(formatDateWithDayName(kiesbareLocatie.startTijd)), getString(properties.search.result.tijdstip, [formatTime(kiesbareLocatie.startTijd)])]}
							col2={["Locatie", kiesbareLocatie.ziekenhuis]}
							col3={["Adres", getString(properties.search.result.locatie, [kiesbareLocatie.adres, kiesbareLocatie.postcode, kiesbareLocatie.plaats])]}

							onHoverText={getString(properties.search.hovertext)}
							onClickAction={() => {
								setZoekFilter(zoekFilter)
								setGekozenAfspraak(kiesbareLocatie)
							}}
						/>,
					)}
					{geenResultaten && <FormErrorComponent text={getString(properties.search.search_no_results)}/>}
					{geenResultaten &&
						<BigUrlButton title={getString(properties.search.search_no_results_contact_header)}
									  text={getString(properties.search.search_no_results_contact_text)}
									  link={getContactUrl(regio)}/>}
					<div className={styles.navigationButtons}>
						{gezocht && !geenResultaten && visibleZoekMeerButton &&
							<div className={styles.showMoreResultsButtonArea}>
								<Button
									label={getString(properties.search.navigation_more_results)}
									displayArrow={ArrowType.ARROW_DOWN}
									onClick={() => {
										const nieuwZoekFilter = {
											...zoekFilter,
										}
										nieuwZoekFilter.pagecount = nieuwZoekFilter.pagecount + 1
										setGezocht(true)
										zoekAfspraken(nieuwZoekFilter)
									}}/>
							</div>}
					</div>
					{!gezocht &&
						<BeforeSearching text={getString(properties.search.before_search)}/>}
				</Col>
			</Row>
			{gekozenAfspraak &&
				<ColonAfspraakMakenBevestigingsPopup afspraak={gekozenAfspraak}
													 heraanmelding={isNieuweAfspraak}
													 onClose={() => {
														 setGekozenAfspraak(undefined)
														 zoekAfspraken(zoekFilter)
													 }}/>}
		</BasePage>
	)
}

export default ColonAfspraakMakenPage
