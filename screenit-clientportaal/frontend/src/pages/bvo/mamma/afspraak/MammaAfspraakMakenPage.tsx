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
import React, {useEffect, useRef} from "react"
import {BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import {Col, Row} from "react-bootstrap"
import styles from "./MammaAfspraakMakenPage.module.scss"
import {AfspraakZoekResultaten, geenAfspraakZoekResultaten, KandidaatAfspraak} from "../../../../datatypes/mamma/KandidaatAfspraak"
import {useThunkDispatch} from "../../../../index"
import {useRegio, useSelectedBvo, useWindowDimensions} from "../../../../utils/Hooks"
import {getHuidigeAfspraak} from "../../../../api/MammaAfspraakMakenThunkAction"
import {useSelector} from "react-redux"
import {State} from "../../../../datatypes/State"
import BasePage from "../../../BasePage"
import SearchResultAfspraken from "../../../../components/search_results/SearchResultAfspraken"
import {formatDateWithDayName, formatTime, zoekIndex} from "../../../../utils/DateUtil"
import {FormErrorComponent} from "../../../../components/form_error/FormErrorComponent"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import MammaAfspraakMakenPopup from "./MammaAfspraakMakenPopup"
import BigUrlButton from "../../../../components/bigUrlButton/BigUrlButton"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {getContactUrl} from "../../../../utils/UrlUtil"
import properties from "./MammaAfspraakMakenPage.json"
import NavigationDaysComponent from "../../../../components/navigation/NavigationDaysComponent"
import ScreenitBackend from "../../../../utils/Backend"
import {AxiosResponse} from "axios"
import MammaAfspraakMakenForm from "../../../../components/form/mamma/MammaAfspraakMakenForm"
import {placeNonBreakingSpaceInDate} from "../../../../utils/StringUtil"
import {compareAsc} from "date-fns"

export type AfspraakZoekFilter = {
	vanaf?: Date,
	plaats?: string,
	afstand?: string,
	meerOpties: boolean
}

const MammaAfspraakMakenPage = () => {
	const dispatch = useThunkDispatch()
	const regio = useRegio()
	const selectedBvo = useSelectedBvo()!
	const {width} = useWindowDimensions()
	const gevondenAfsprakenDiv = useRef<HTMLDivElement | null>(null)
	const [dagenBeschikbaar, setDagenBeschikbaar] = React.useState<boolean>(true)
	const [gekozenAfspraak, setGekozenAfspraak] = React.useState<KandidaatAfspraak | undefined>(undefined)
	const [afspraakMakenNietGelukt, setAfspraakMakenNietGelukt] = React.useState<boolean>(false)
	const [zoekFilter, setZoekFilter] = React.useState<AfspraakZoekFilter>({
		vanaf: undefined,
		plaats: undefined,
		afstand: undefined,
		meerOpties: false,
	})
	const [beschikbareDagen, setBeschikbareDagen] = React.useState<Date[]>([])

	useEffect(() => {
		ScreenitBackend.get(`/mamma/afspraak/standplaatsPlaatsen`)
			.then((response) => {
				setLaatsteStandplaatsZoekFilter(response.data)
			})
		dispatch(getHuidigeAfspraak())
	}, [dispatch, selectedBvo])

	function zoekMetFormulier(zoekFilter: AfspraakZoekFilter) {
		setZoekFilter(zoekFilter)
		zoekAfspraken(zoekFilter)
	}

	function zoekAfspraken(zoekFilter: AfspraakZoekFilter) {
		ScreenitBackend.post(`/mamma/afspraak/zoeken`, zoekFilter)
			.then((response: AxiosResponse<AfspraakZoekResultaten>) => {
				setLaatsteAfspraakZoekResultaten(response.data)
			})
	}

	const huidigeAfspraak = useSelector((state: State) => state.client.mammaDossier.huidigeAfspraak)

	const [laatsteStandplaatsZoekFilter, setLaatsteStandplaatsZoekFilter] = React.useState<string[]>([])
	const [laatsteAfspraakZoekResultaten, setLaatsteAfspraakZoekResultaten] = React.useState<AfspraakZoekResultaten>(geenAfspraakZoekResultaten)

	const gesplitsteAdresStandplaats = huidigeAfspraak && huidigeAfspraak.adresStandplaats.split(",", 2)
	const resultatenGevonden = laatsteAfspraakZoekResultaten.length !== 0

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.MAMMA}
				  title={huidigeAfspraak ? getString(properties.page.title.afspraak_verzetten) : getString(properties.page.title.afspraak_maken)}
				  description={huidigeAfspraak ? getString(properties.page.description.afspraak_verzetten) : getString(properties.page.description.afspraak_maken)}
				  toonBlob={!!huidigeAfspraak}
				  blobTitle={getString(properties.blob.title)}
				  blobText={getString(properties.blob.afspraak_moment, huidigeAfspraak && [huidigeAfspraak.weergaveAfspraakMoment])}
				  blobAdresLocatie={getString(properties.blob.afspraak_locatie, gesplitsteAdresStandplaats && [gesplitsteAdresStandplaats[0], gesplitsteAdresStandplaats[1]])}>

			<Row>
				<Col md={5}>
					<MammaAfspraakMakenForm zoekFilter={zoekFilter}
											laatsteStandplaatsZoekFilter={laatsteStandplaatsZoekFilter}
											onSubmitSucces={((values, beschikbareDagen) => {
												zoekMetFormulier({
													...values,
													meerOpties: false,
												})
												setBeschikbareDagen(beschikbareDagen.sort(compareAsc))
											})}
											onZoekenClick={() => {
												if (width <= 768 && gevondenAfsprakenDiv.current) {
													window.scrollTo(0, gevondenAfsprakenDiv.current.offsetTop - 100)
												}
											}}
											onChange={() => {
												setLaatsteAfspraakZoekResultaten([])
											}}
											setDagenBeschikbaar={(value) => {
												setDagenBeschikbaar(value)
											}
											}/>
					<BigUrlButton title={getString(properties.searchresult.button.header.uitstellen)}
								  text={getString(properties.searchresult.button.text.uitstellen)}
								  link={"/mamma/uitstellen"}/>
					<BigUrlButton title={getString(properties.searchresult.button.header.contact)}
								  text={getString(properties.searchresult.button.text.contact)}
								  link={getContactUrl(regio)}/>
				</Col>
				<Col md={7} className={styles.results} ref={gevondenAfsprakenDiv}>
					{laatsteAfspraakZoekResultaten && laatsteAfspraakZoekResultaten.map((kandidaatAfspraak, index) =>
						<SearchResultAfspraken
							key={index}
							className={styles.result}

							col1={["", placeNonBreakingSpaceInDate(formatDateWithDayName(kandidaatAfspraak.datumTijd)), formatTime(kandidaatAfspraak.datumTijd) + " uur"]}
							col3={["Locatie", kandidaatAfspraak.adres, kandidaatAfspraak.postcode + " " + kandidaatAfspraak.plaats]}

							onHoverText={getString(properties.searchresult.hovertext)}
							onClickAction={() => {
								kandidaatAfspraak.filter = zoekFilter
								setGekozenAfspraak(kandidaatAfspraak)
							}}
						/>,
					)}

					{!dagenBeschikbaar && <FormErrorComponent text={getString(properties.searchresult.no_results)}/>}

					{resultatenGevonden && <NavigationDaysComponent geselecteerdeDatum={zoekFilter.vanaf}
																	terugKnopIsZichtbaar={zoekIndex(beschikbareDagen, zoekFilter.vanaf) > 0}
																	vooruitKnopIsZichtbaar={zoekFilter.vanaf ? zoekIndex(beschikbareDagen, zoekFilter.vanaf) < beschikbareDagen.length - 1 : false}
																	onClickBack={() => {
																		if (zoekFilter.vanaf !== undefined) {
																			const nieuwZoekFilter = {
																				...zoekFilter,
																				vanaf: beschikbareDagen[zoekIndex(beschikbareDagen, zoekFilter.vanaf) - 1],
																				meerOpties: false,
																			}
																			setZoekFilter(nieuwZoekFilter)
																			zoekAfspraken(nieuwZoekFilter)
																		}
																	}}
																	onClickForward={() => {
																		if (zoekFilter.vanaf !== undefined) {
																			const nieuwZoekFilter = {
																				...zoekFilter,
																				vanaf: beschikbareDagen[zoekIndex(beschikbareDagen, zoekFilter.vanaf) + 1],
																				meerOpties: false,
																			}
																			setZoekFilter(nieuwZoekFilter)
																			zoekAfspraken(nieuwZoekFilter)
																		}
																	}}/>}

					{resultatenGevonden && !zoekFilter.meerOpties &&
					<div className={styles.showMoreResultsButtonArea}>
						<Button
							className={styles.meerResultaten}
							label={getString(properties.navigation.more_results)}
							displayArrow={ArrowType.ARROW_DOWN}
							onClick={() => {
								const nieuwZoekFilter = {
									...zoekFilter,
									meerOpties: true,
								}
								setZoekFilter(nieuwZoekFilter)
								zoekAfspraken(nieuwZoekFilter)
							}}/>
					</div>}
				</Col>
			</Row>

			{(gekozenAfspraak && afspraakMakenNietGelukt &&
				<MammaAfspraakMakenPopup afspraak={gekozenAfspraak}
										 isBevestigingsPopup={false}
										 onClose={() => {
											 setGekozenAfspraak(undefined)
											 zoekAfspraken(zoekFilter)
											 setAfspraakMakenNietGelukt(false)
										 }}/>)}

			{(gekozenAfspraak && !afspraakMakenNietGelukt &&
				<MammaAfspraakMakenPopup afspraak={gekozenAfspraak}
										 isBevestigingsPopup={true}
										 onFailure={() => {
											 setAfspraakMakenNietGelukt(true)
										 }}
										 onClose={() => {
											 setGekozenAfspraak(undefined)
											 zoekAfspraken(zoekFilter)
										 }}/>)}

		</BasePage>
	)
}

export default MammaAfspraakMakenPage
